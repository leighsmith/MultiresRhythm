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

(in-package :multires-rhythm)
(use-package :nlisp)

;;; A rhythm includes a textual description, the sample rate and a general description of
;;; a signal. This allows representing a rhythm as a continuum between
;;; a signal processing representation (high sample rate) and a symbolic representation
;;; (low sample rate).
(defclass rhythm ()
  ((name        :initarg :name        :accessor name        :initform "unnamed")
   (description :initarg :description :accessor description :initform "")
   (time-signal :initarg :time-signal :accessor time-signal :initform (make-double-array '(1)))
   (sample-rate :initarg :sample-rate :accessor sample-rate :initform 200)))

(defgeneric tactus-for-rhythm (rhythm-to-analyse &key voices-per-octave)
  (:documentation "Returns the selected tactus for the rhythm."))

(defun preferred-tempo (magnitude phase voices-per-octave rhythm-sample-rate 
			;; Fraisse's "spontaneous tempo" interval in seconds
			&key (tempo-salience-peak 0.600))
  "Determine the scale which Fraisse's spontaneous tempo would occur at.
This is weighted by absolute constraints, look in the 600ms period range."

  (declare (ignore magnitude phase))
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
	 ;; Create a Gaussian envelope spanning the number of scales.
	 (tempo-scale-weighting (gaussian-envelope nscale :center (- 1.0 (/ (* 2.0 salient-scale) nscale)))))
    (format t "preferred tempo scale = ~d~%" salient-Scale)

    (dotimes (time ntime)
      (setf-subarray (val tempo-weighting-over-time) 
		     (val (.reshape tempo-scale-weighting (list nscale 1))) (list t time)))
    (plot (.column tempo-weighting-over-time 0) nil :title "Preferred tempo weighting profile")
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

;; if normalise-by-scale distorts the topography?
(defun normalise (magnitude)
  "Normalise magnitude over the entire signal. This may not be realistic"
  (./ magnitude (.max magnitude)))

(defun normalise-by-scale (magnitude)
  "Normalise a magnitude finding the maximum scale at each time point.
 We assume values are positive values only"
  (let* ((time-frequency-dimensions (.array-dimensions magnitude))
	 (nScale (first time-frequency-dimensions))
	 (nTime (second time-frequency-dimensions))
	 (normalised-values (make-double-array time-frequency-dimensions)))
    ;; Duplicate the maximum scales per time for each scale to enable element-wise division.
    (dotimes (i nTime)
      (let* ((scales-per-time (.column magnitude i))
	     ;; Determine the maximum scale at each time.
	     (maxScalePerTime (.max (.abs scales-per-time)))
	     ;; Normalise each time-slice.
	     ;; If a maximum scale value is 0, then make it 1 to keep the division healthy.
	     (normalised-time-slice (./ scales-per-time (if (zerop maxScalePerTime) 1 maxScalePerTime))))
	;; scales-per-time will be returned as a vector, we need to reshape it in order to
	;; store it.
	(setf-subarray (val normalised-values) 
		       (val (.reshape normalised-time-slice (list nScale 1)))
		       (list t i))))
    normalised-values))

(defun clamp-to-bounds (signal test-signal 
			&key (low-bound 0) (clamp-low 0) (high-bound 0 high-bound-supplied-p) (clamp-high 0))
  "Clip a signal according to the bounds given, by testing another signal (which can be the same as signal). 
Anything clipped will be set to the clamp-low, clamp-high values"
  (let* ((above-low (.> test-signal low-bound))
	 (below-high (if high-bound-supplied-p
			 (.< test-signal high-bound)
			 1d0)))
    (.+ (.* (.and above-low below-high) signal)
	(.* (.not above-low) clamp-low) 
	(.* (.not below-high) clamp-high))))

;; (setf a (.rseq2 0 9 10))
;; (setf b (.rseq2 9 0 10))
;; (clamp-to-bounds b a :low-bound 5d0 :clamp-low -1d0)

(defun stationary-phase (magnitude phase voices-per-octave &key (magnitude-minimum 0.01))
  "Compute points of stationary phase. Implementation of Delprat
   et. al's ridges from points of stationary phase convergent
   algorithm. See: delprat:asymptotic

   Inputs
     magnitude    Magnitude output of CWT
     phase        Phase output of CWT
     magnitude-minimum is the minimum magnitude to accept phase as valid
   Outputs
     binary array indicating presence of ridge."
  ;; the scale is the central frequency of the mother wavelet divided by
  ;; the central frequency of the dilated wavelet
  (let* ((time-frequency-dimensions (.array-dimensions phase))
	 (nScale (first time-frequency-dimensions))
	 (nTime (second time-frequency-dimensions))
	 ;; (ridges (make-double-array time-frequency-dimensions))
	 (signal-phase-diff (make-double-array time-frequency-dimensions))

	 ;; Compute a discrete approximation to the partial derivative of the
	 ;; phase with respect to translation (time) t.
	 (dt-phase (diff phase))

	 ;; Phase is -pi -> pi.
	 ;; We need to add back 2 pi to compensate for the phase wraparound
	 ;; from pi to -pi. This wraparound will create a dt-phase close to 2 pi.
	 (which-wrapped (.> (.abs dt-phase) (* pi 1.5))) ; anything > pi is a wraparound
	 (phase-wrap (.- (* 2 pi) (.abs dt-phase)))
	 (wrapped-dt-phase (.+ (.* (.not which-wrapped) dt-phase) (.* phase-wrap which-wrapped)))

	 ;; The acceleration of phase difference should be non-zero (Tchamitchian and
	 ;; Torresani Eq. III-7 pp128) phaseDiffAcceleration will therefore be two time
	 ;; sample points less than the original phase time extent.
	 ;; phaseDiffAcceleration = diff(wrapped-dt-phase.').';
	 ;;
	 ;; Doing this will still produce NaN since the division already
	 ;; creates Inf and the clamping uses multiplication.
	 ;; wrapped-dt-phase = (clamp-to-bounds wrapped-dt-phase magnitude magnitude-minimum 1)

	 ;; convert the phase derivative into periods in time
	 (signal-period (./ (* 2 pi) wrapped-dt-phase))
	 (scale-time-support (time-support (.iseq 0 (1- nScale)) voices-per-octave))
	 ;; (normalised-signal-phase-diff)
	 (maximal-stationary-phase))

    ;; When we have neglible magnitude, phase is meaningless, but can oscillate so rapidly
    ;; we have two adjacent phase values which are equal (typically pi, pi/2, or 0). This
    ;; can cause dt-phase to be zero and therefore make signal-period NaN. This then
    ;; causes normalise-by-scale to freak out because NaN is considered a maximum.  Our
    ;; kludge is to set those NaN signal-periods to 0.0 so they don't cause excessive
    ;; extrema and they will be zeroed out of the final result using
    ;; clamp-to-bounds. Strictly speaking, we should verify mag is below magnitude-minimum
    ;; before setting these zero dt-phase values.
    ;; (setf (.aref signal-period (find (.not dt-phase))) 0.0)

    (dotimes (i nScale)
      ;; Accept if the signal period (from its phase derivatives) and the time support of
      ;; the wavelet at each dilation scale are within 1.5 sample of one another.
      (setf-subarray (val signal-phase-diff)
		     (val (.abs (.- (.subarray signal-period (list i t)) (.aref scale-time-support i))))
		     (list i (1- nTime))))

    ;; We invert the normalised phase difference so that maximum values indicate
    ;; stationary phase -> ridges.
    (setf maximal-stationary-phase (.- 1d0 (normalise-by-scale signal-phase-diff)))

    ;; There must be some magnitude at the half-plane points of stationary phase for the
    ;; ridge to be valid.
    (clamp-to-bounds maximal-stationary-phase magnitude :low-bound magnitude-minimum :clamp-low 0)))

(defun local-phase-congruency (magnitude phase 
			       &key (magnitude-minimum 0.01)
			       (congruency-threshold (/ pi 2)))
  "Compute points of local phase congruency.
   Determines the points of phase with least deviation between adjacent scales.

  Inputs
    magnitude Magnitude output of CWT - currently ignored.
    phase  Phase output of CWT
    magnitude-minimum is the minimum magnitude to accept phase as valid.
    congruency-threshold maximum deviation 
  Outputs
    matrix indicating presence of congruency."

  (let* ((time-frequency-dimensions (.array-dimensions phase))
	 (num-of-scales (first time-frequency-dimensions))

	 ;; Find derivative with respect to scale s. Transposes phase so that diffence is
	 ;; across rows (i.e scales).
	 (abs-ds-phase (.abs (diff (.transpose phase))))

	 ;; Compensate for phase wrap-around with derivatives with respect to scale (ds) such that
	 ;; the maximum angular difference between vectors is <= pi radians.
	 (which-wrapped (.> abs-ds-phase pi))
	 (phase-wrap (.- (* 2 pi) abs-ds-phase))

	 ;; Compute the troughs in the phase congruency, which indicates where
	 ;; phase most closely match between adjacent scales, most indicative
	 ;; of a frequency. 
	 (local-phase-diff (.+ (.* (.not which-wrapped) abs-ds-phase) (.* which-wrapped phase-wrap)))
	 (congruency (make-double-array time-frequency-dimensions))
	 (maximal-local-pc)
	 (local-pc-for-mag)
	 (no-outliers))
    ;; Since we produce the derivative from the difference, local-phase-diff is one
    ;; element less.
    (setf-subarray (val congruency) (val (.transpose local-phase-diff)) (list (list 1 (1- num-of-scales)) t))

#|
    ;; so we need to interpolate the result to
	 ;; produce the correct value for each scale.
	 (congruency (./ (.abs (.+ (.concatenate local-phase-diff scale-pad) 
	 (.concatenate scale-pad local-phase-diff)))) 2)
;; or
	 (congruency (.transpose local-phase-diff))
|#

    ;; The local phase congruency measure should be maximal at points of minimal
    ;; difference between scales, so we normalise it, then subtract from 1.
    ;; (setf maximal-local-pc (.- 1d0 (normalise-by-scale congruency)))
    (setf maximal-local-pc (.- 1d0 (normalise congruency)))

    ;; There must be some magnitude at the half-plane points of local phase congruency for
    ;; the ridge to be valid.  
    (setf local-pc-for-mag (clamp-to-bounds maximal-local-pc magnitude :low-bound magnitude-minimum :clamp-low 0))
    local-pc-for-mag))
#|
    (plot-image #'magnitude-image "-clampedpc" (list local-pc-for-mag))
    (plot (.subarray local-pc-for-mag '(t 182)) nil)

    ;; determine outliers: those troughs greater in difference than threshold.
    (setf no-outliers (.< congruency congruency-threshold))

    ;; Remove outliers.
    (.* local-pc-for-mag no-outliers)))
|#

;; TODO alternatively return normalised-magnitude, stationary-phase or
;; local-phase-congruency individually.
;; Should use a functional approach, passing the functions applicable as a list:
;; (&key analyzers '(#'stationary-phase #'local-phase-congruency #'normalise))
(defun correlate-ridges (magnitude phase voices-per-octave)
  "Computes independent surfaces analysing the magnitude and phase
which are then combined to form an analytic surface from which we
then can extract ridges."
  ;; ahh, parallel machine anyone??
  (let ((stat-phase (stationary-phase magnitude phase voices-per-octave))
	(local-pc (local-phase-congruency magnitude phase))
	;; (normalised-magnitude (normalise magnitude))
	(normalised-magnitude (normalise-by-scale magnitude)))
    ;; correlate (by averaging) the energy modulus, stationary phase and
    ;; local phase congruency. Since stationary phase and local phase congruency are both
    ;; conditioned on a minimal magnitude, we reduce false positives.
    (./ (.+ normalised-magnitude stat-phase local-pc) 3d0)))

(defun shift-scales (scales up)
  "Shifts the rows (rotating) upwards (towards lower indices) if up = t, down if up = nil"
  (let* ((time-frequency-dimensions (.array-dimensions scales))
	 (last-scale (1- (first time-frequency-dimensions)))
	 (scales-shifted (make-double-array time-frequency-dimensions))
	 (matrix-position (if up 
			      (list (list (list 1 last-scale) t) (list (list 0 (1- last-scale)) t))
			      (list (list (list 0 (1- last-scale)) t) (list (list 1 last-scale) t))))
	 (rotated-row (if up 
			  (list (list 0 t) (list last-scale t)) 
			  (list (list last-scale t) (list 0 t)))))
    (setf-subarray (val scales-shifted) (val (.subarray scales (first matrix-position))) (second matrix-position))
    (setf-subarray (val scales-shifted) (val (.subarray scales (first rotated-row))) (second rotated-row))
    scales-shifted))

(defun determine-scale-peaks (correlated-profile &key (correlation-minimum 0.01))
  "Finds the peaks in the combined correlation profile 
  (of energy modulus, stationary phase and local phase congruency)
   across the dilation scale axis at each time point."
  (let* ((profile-shifted-up (shift-scales correlated-profile t))
	 (profile-shifted-down (shift-scales correlated-profile nil)))
    ;; the multiplication retains the scale peak values, clamping non maxima to zero.
    (.* (.and (.> correlated-profile profile-shifted-down) 
	      (.> correlated-profile profile-shifted-up) 
	      (.> correlated-profile correlation-minimum))
	correlated-profile)))

(defmethod tactus-for-rhythm ((analysis-rhythm rhythm) &key (voices-per-octave 16))
  "Returns the selected tactus given the rhythm."
  (multiple-value-bind (magnitude phase) (cwt (time-signal analysis-rhythm) voices-per-octave)
    (plot-cwt magnitude phase :title (name analysis-rhythm))
    ;; Correlate various ridges to produce a robust version.
    (let* ((correlated-ridges (correlate-ridges magnitude phase voices-per-octave))
	   ;; Scale index 1 is the highest frequency (smallest dilation) scale.
	   (salient-scale (preferred-tempo magnitude phase voices-per-octave (sample-rate analysis-rhythm)))
	   ;; Weight by the absolute tempo preference.
	   (tempo-weighted-ridges (.* correlated-ridges 
				      (tempo-salience-weighting salient-scale (.array-dimensions magnitude))))
	   ;; TODO substitute tempo-weighted-ridges for tempo selectivity.
	   (correlated-ridge-scale-peaks (determine-scale-peaks correlated-ridges))
	   (ridge-set (extract-ridges correlated-ridge-scale-peaks))
	   ;; select out the tactus from all ridge candidates.
	   (chosen-tactus (select-longest-tactus ridge-set)))
      (plot-ridges-and-tactus correlated-ridge-scale-peaks chosen-tactus :title (name analysis-rhythm)))))
