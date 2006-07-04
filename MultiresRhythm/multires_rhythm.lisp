;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;; 
;;;; A multiresolution model of musical rhythm.
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; See 
;;;;   author =  {Leigh M. Smith},
;;;;   title =   {A Multiresolution Time-Frequency Analysis and Interpretation of Musical Rhythm},
;;;;   school =  {Department of Computer Science, University of Western Australia},
;;;;   year =    1999,
;;;;   month =   {June},
;;;;   annote =  {\url{http://www.leighsmith.com/Research/Papers/MultiresRhythm.pdf}}
;;;;

;;; (in-package multires-rhythm)

;;; A rhythm includes a textual description, the sample rate and a general description of
;;; a signal. This allows representing a rhythm as a continuum between
;;; a signal processing representation (high sample rate) and a symbolic representation
;;; (low sample rate).
(defclass rhythm ()
  ((name :initform "unnamed")
   (description :initform "")
   (signal :initarg :signal :initform (make-double-array '(1) :accessor :signal))
   (sample-rate :accessor sample-rate)))

(defgeneric tactus-for-rhythm (rhythm-to-analyse &key voices-per-octave)
  (:documentation "Returns the selected tactus for the rhythm."))

(defmethod diff ((a n-array) &optional order)
  (let* ((input-dimensions (.array-dimensions a))
	  (rows (first input-dimensions))
	  (cols (second input-dimensions))
	  (result (make-double-array (list rows (- cols order)))))
    (dotimes (i order)
      (setf result (.- result (shift result))))
    result))

(defun preferred-tempo (magnitude phase voices-per-octave rhythm-sample-rate 
			;; Fraisse's "spontaneous tempo" interval in seconds
			&key (tempo-salience-peak 0.600))
  "Determine the scale which Fraisse's spontaneous tempo would occur at.
This is weighted by absolute constraints, look in the 600ms period range."

  (let* ((salient-IOI (* rhythm-sample-rate tempo-salience-peak)))
    ;; Convert salient-IOI into the scale to begin checking.
    ;; This assumes the highest frequency scale (shortest time support)
    ;; is index 0, lowest frequency (longest time support) is nScale.
    (floor (scale-from-period salient-IOI voices-per-octave))))

;; Scale index 1 is the highest frequency (smallest dilation) scale.
(defun tempo-salience-weighting (salient-scale time-frequency-dimensions)
  "Produce a weighting matching the analysis window using tempo preference."
  (let* ((nscale (first time-frequency-dimensions))
	 (ntime (second time-frequency-dimensions))
	 (tempo-weighting-over-time (make-double-array time-frequency-dimensions))
	 (tempo-scale-weighting (shift (gaussian-envelope nscale) (- salient-scale (/ nscale 2)))))
    (format t "preferred tempo scale = ~d~%" salient-Scale)

    (dotimes (scale nscale)
      (setf-subarray tempo-weighting-over-time (.aref tempo-scale-weighting scale) (list scale t)))
    (plot (.subarray tempo-scale-weighting (list 0 t))
	  :title "Preferred tempo weighting profile")
    tempo-weighting-over-time))

#|

  ;; We set any ill-conditioned phase (negligible magnitude) to zero here
  ;; after we take the phase derivative (in ridgesStatPhase) as it would
  ;; create false changes.
  phase = cleanPhase(mag, phase);

  (format t "Stationary Phase~%")
  (format t "Local Phase Congruency~%")
  ;; redundant
  ;;fprintf(promptStream, "Stationary Phase Ridges\n");
  ;;ridgeStatPhase = ridgesStatPhase(magnitude, phase, voices-per-octave);
  fprintf(promptStream, "Finding ridge (by scale)\n");

  ;; show what we got as an intensity plot
  plotCWT("correlation", correlation)
  )
|#

(defun normalise-by-scale (magnitude)
  "Normalise a magnitude finding the maximum scale at each time point.
 We assume values are positive values only"
  (let* ((time-frequency-dimensions (.array-dimensions magnitude))
	 (nScale (first time-frequency-dimensions))
	 (maxVals (make-double-array time-frequency-dimensions))
	 ;; Determine the maximum scale at each time.
	 (maxScalePerTime (.max (.abs magnitude)))
	 ;; If a maximum scale value is 0, then make those 1 to keep the division healthy.
	 (maxScalePerTime (.+ maxScalePerTime (.not maxScalePerTime))))
    ;; Duplicate the maximum scales per time for each scale to enable element-wise division.
    (dotimes (i nScale)
      (setf-subarray maxVals maxScalePerTime '(i t))
    ;; normalised 
    (./ magnitude maxVals)))

(defun clamp-to-bounds (signal testSignal 
			&key (low-bound 0) (clamp-low 0) (high-bound 0 high-bound-supplied-p) (clamp-high 0))
  "Clip a signal according to the bounds given, by testing another signal (which can be the same as signal). 
Anything clipped will be set to the clamp-low, clamp-high values"
  (let* ((above-low (.> testSignal low-bound))
	 (below-high (if high-bound-supplied-p
			 (.< testSignal high-bound)
			 (make-double-array (.array-dimensions signal) :initial-element 1d0))))
    (.+ (.* (.and above-low below-high) signal)
	(.* (.not above-low) clamp-low) 
	(.* (.not below-high) clamp-high))))

(defun stationary-phase (magnitude phase voices-per-octave &key (magnitude-minimum 0.01))
  "Compute points of stationary phase.

   Implementation of Delprat et. al's ridges from points of stationary phase
   convergent algorithm. See: delprat:asymptotic

 Inputs
   magnitude    Magnitude output of CWT
   phase        Phase output of CWT
   magnitude-minimum is the minimum magnitude to accept phase as valid
 Outputs
   statPhase - binary array indicating presence of ridge.
"
  ;; the scale is the central frequency of the mother wavelet divided by
  ;; the central frequency of the dilated wavelet
  (let* ((time-frequency-dimensions (.array-dimensions phase))
	 (nScale (first time-frequency-dimensions))
	 (nTime (second time-frequency-dimensions))
	 (ridges (make-double-array time-frequency-dimensions))
	 (signal-phase-diff (make-double-array time-frequency-dimensions))

	 ;; Compute a discrete approximation to the partial derivative of the
	 ;; phase with respect to translation (time) t.
	 (dt-phase (.transpose (diff (.transpose phase))))

	 ;; Phase is -pi -> pi.
	 ;; We need to add back 2 pi to compensate for the phase wraparound
	 ;; from pi to -pi. This wraparound will create a dt-phase close to 2 pi.
	 (whichwrap (.> (.abs dt-phase) (* pi 1.5))) ; anything > pi is a wraparound
	 (phasewrap (.- (.* 2 pi) (.abs dt-phase)))
	 ;; wrapped-dt-phase = (whichwrap == 0) .* dt-phase + phasewrap .* whichwrap;
	 (wrapped-dt-phase (+ (.* (.not whichwrap) dt-phase) (.* phasewrap whichwrap)))

	 ;; The acceleration of phase difference should be non-zero
	 ;; (Tchamitchian and Torresani Eq. III-7 pp128)
	 ;; phaseDiffAcceleration will therefore be two time sample points less
	 ;; than the original phase time extent.
	 ;; phaseDiffAcceleration = diff(wrapped-dt-phase.').';
	 
	 ;; Doing this will still produce NaN since the division already
	 ;; creates Inf and the clamping uses multiplication.
	 ;; wrapped-dt-phase = clamp-to-bounds(wrapped-dt-phase, magnitude, magnitude-minimum, 1);

	 ;; convert the phase derivative into periods in time
	 (signalPeriod (./ (.* 2 pi) wrapped-dt-phase))
	 (scale-time-support (time-support (.iseq 0 (1- nScale)) voices-per-octave))
	 (normalised-signal-phase-diff)
	 (maximal-stationary-phase))

    ;; When we have neglible magnitude, phase is meaningless, but can
    ;; oscillate so rapidly we have two adjacent phase values which are
    ;; equal (typically pi, pi/2, or 0). This can cause dt-phase to be
    ;; zero and therefore make signalPeriod NaN. This then causes
    ;; normaliseByScale to freakout because NaN is considered a maximum.
    ;; Our kludge is to set those NaN signalPeriods to 0.0 so they don't
    ;; cause excessive extrema and they will be zeroed out of the final
    ;; result using clamp-to-bounds. Strictly speaking, we should verify
    ;; mag is below magnitude-minimum before setting these zero dt-phase values.
    ;; Use fortran indexing.
    (setf (.aref signalPeriod (find (.not dt-phase))) 0.0)

    (dotimes (i nScale)
      ;; Accept if the signal period (from its phase derivatives) and
      ;; the time support of the wavelet at each dilation scale are within
      ;; 1.5 sample of one another.
      (setf-subarray signal-phase-diff 
		     (.abs (.- (.subarray signalPeriod (list i t)) (.aref scale-time-support i)))
		     (list i (1- ntime))))

    (setf normalised-signal-phase-diff (normalise-by-scale signal-phase-diff))

    ;; We invert the normalised phase difference so that maximum values
    ;; indicate stationary phase -> ridges.
    (setf maximal-stationary-phase (.- (make-double-array time-frequency-dimensions :initial-element 1d0)
			       normalised-signal-phase-diff))

    ;; There must be some magnitude at the half-plane points of stationary phase
    ;; for the ridge to be valid. 
    (clamp-to-bounds maximal-stationary-phase magnitude magnitude-minimum 0))))

(defun local-phase-congruency (magnitude phase &key (magnitude-minimum 0.01) (congruency-threshold 0.05))
  "Compute points of local phase congruency.
   Determines the points of phase with least deviation between adjacent scales.

  Inputs
    magnitude Magnitude output of CWT - currently ignored.
    phase  Phase output of CWT
    magnitude-minimum is the minimum magnitude to accept phase as valid.
  Outputs
    normalised-congruency - matrix indicating presence of congruency."

  (let* ((time-frequency-dimensions (.array-dimensions phase))
	 (nScale (first time-frequency-dimensions))
	 (nTime  (second time-frequency-dimensions))
	 (ridges (make-double-array time-frequency-dimensions))

	 ;; find derivative with respect to scale s.
	 (abs-ds-phase (.abs (diff phase)))

	 ;; Compensate for phase wrap-around with derivatives wrt scale such that
	 ;; the maximum angular difference between vectors <= pi radians.
	 (whichwrap (.> abs-ds-phase pi))
	 (phasewrap (.- (.* 2 pi) abs-ds-phase))

	 ;; Compute the troughs in the phase congruency, which indicates where
	 ;; phase most closely match between adjacent scales, most indicative
	 ;; of a frequency. 
	 (local-phase-diff (.+ (.* (.not whichwrap) abs-ds-phase) (.* whichwrap phasewrap)))

	 ;; Since we produce the derivative from the difference, local-phase-diff
	 ;; is one element less, so we need to interpolate the result to
	 ;; produce the correct value for each scale.
	 (scalePad (make-double-array nTime))
	 (congruency (./ 2 (.abs (.- (.concatenate local-phase-diff scalePad) (.concatenate scalePad local-phase-diff)))))

	 ;; determine outliers, those troughs greater in difference than threshold
	 (noOutliers (.< congruency congruency-threshold))

	 ;; The local phase congruency measure should be maximum for least
	 ;; difference, so we normalise it, then subtract from 1.
	 (maximalLocalPC (.- (make-double-array time-frequency-dimensions :initial-element 1d0) 
			     (normalise-by-scale congruency))))

    ;; There must be some magnitude at the half-plane points of local
    ;; phase congruency for the ridge to be valid.
    ;; Remove outliers.
    (.* (clamp-to-bounds maximalLocalPC magnitude magnitude-minimum 0) noOutliers)))

;; otherwise return normalisedMagnitude, statPhase or localPC individually.
(defun correlate-ridges (magnitude phase voices-per-octave)
  "Computes independent surfaces analysing the magnitude and phase
which are then combined to form an analytic surface from which we
then can extract ridges."
  ;; ahh, parallel machine anyone??
  (let ((stat-phase (stationary-phase magnitude phase voices-per-octave))
	(local-pc (local-phase-congruency magnitude phase))
	;; magnitude = abs(magnitude)
	(normalised-magnitude (normalise-by-scale magnitude)))
    ;; correlate (by averaging) the energy modulus, stationary phase and
    ;; local phase congruency. Since statPhase and local PC are both
    ;; conditioned on a minimal magnitude, we reduce false positives.
    (./ (.+ normalised-magnitude stat-phase local-pc) 3d0)))

(defmethod tactus-for-rhythm ((analysis-rhythm rhythm) &key (voices-per-octave 16))
  "Returns the selected tactus given the rhythm."
  (multiple-value-bind (magnitude phase) (cwt (signal analysis-rhythm) voices-per-octave)
    ;; (plot-cwt magnitude phase :title (name analysis-rhythm))
    ;; Correlate various ridges to produce a robust version.
    (let* ((correlated-ridges (correlate-ridges magnitude phase voices-per-octave))
	   ;; Scale index 1 is the highest frequency (smallest dilation) scale.
	   (salient-scale (preferred-tempo magnitude phase voices-per-octave (sample-rate analysis-rhythm)))
	   ;; Weight by the absolute tempo preference.
	   (tempo-weighted-ridges (.* correlated-ridges 
				      (tempo-salience-weighting salient-scale (.array-dimensions magnitude))))
	   ;; (ridge-set (extract-ridges tempo-weighted-ridges)))
	   (ridge-set (extract-ridges correlated-ridges)))
      ;; select out the tactus from all ridge candidates.
      (select-longest-tactus ridge-set))))
