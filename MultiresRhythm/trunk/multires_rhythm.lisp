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

;;;; Declarations

;;; A rhythm includes a textual description, the sample rate and a general description of
;;; a signal. This allows representing a rhythm as a continuum between
;;; a signal processing representation (high sample rate) and a symbolic representation
;;; (low sample rate).
(defclass rhythm ()
  ((name        :initarg :name        :accessor name        :initform "unnamed")
   (description :initarg :description :accessor description :initform "")
   (time-signal :initarg :time-signal :accessor time-signal :initform (make-double-array '(1)))
   (sample-rate :initarg :sample-rate :accessor sample-rate :initform 200)))

(defgeneric duration (rhythm-to-analyse)
  (:documentation "Returns the length of the rhythm in seconds."))

(defgeneric sample-length (rhythm-to-analyse)
  (:documentation "Returns the length of the rhythm in samples."))

(defgeneric rhythm-iois (rhythm-to-analyse)
  (:documentation "Given a rhythm, returns IOIs specified in seconds."))

(defgeneric save-rhythm (rhythm-to-save)
  (:documentation "Writes the rhythm to a MusicKit scorefile."))

(defgeneric plot-rhythm (rhythm-to-plot)
  (:documentation "Plot locations of beats of the given rhythm."))

(defgeneric clap-to-rhythm (rhythm-to-analyse &key tactus-selector)
  (:documentation "Returns a set of sample times to clap to given the supplied rhythm"))

(defgeneric tactus-for-rhythm (rhythm-to-analyse &key voices-per-octave tactus-selector)
  (:documentation "Returns the selected tactus for the rhythm."))

(defgeneric scaleogram-of-rhythm (rhythm-to-analyse &key voices-per-octave)
  (:documentation "Returns the scaleogram for the rhythm."))

(defgeneric skeleton-of-rhythm (rhythm-to-analyse &key voices-per-octave)
  (:documentation "Returns a list of ridges (a skeleton) for the given rhythm."))

(defgeneric rhythm-complexity (rhythm-to-analyse)
  (:documentation "Returns a normalised measure of the complexity of the rhythm,
   where 0 = impossibly simple -> 1 = impossibly hard."))

;;;; Implementation

(defun time-of-beat (rhythm beat-number)
  "Returns the sample number of the beat-number'th beat in the given rhythm"
  (let* ((beat-positions (.find (time-signal rhythm))))
    (.aref beat-positions beat-number)))

(defun iois-to-rhythm (name iois &key (shortest-ioi 1.0) (sample-rate 200))
  "Returns a rhythm instance given a list of inter-onset intervals"
    (make-instance 'rhythm 
		   :name name
		   :description name ; TODO transliterate '-' for ' '.
		   :time-signal (nlisp::list-to-array 
				 (butlast 
				  (onsets-to-grid 
				   (iois-to-onsets 
				    (intervals-in-samples iois :ioi shortest-ioi)))))
		   :sample-rate sample-rate))

(defun rhythm-of-onsets (name onsets &key (sample-rate 200))
  "Given an narray of onsets, creates a rhythm instance"
  (iois-to-rhythm name (nlisp::array-to-list (.diff onsets)) :sample-rate sample-rate))

(defun rhythm-of-grid (name grid &key (tempo 80 tempo-supplied-p)
				(shortest-ioi 1.0 ioi-supplied-p)
				(sample-rate 200))
  "Given a rhythmic grid list, returns a rhythm instance"
  ;; Append an impulse to ensure trailing silences are counted.
  (iois-to-rhythm name 
		  (onsets-to-iois (grid-to-onsets (append grid '(1))))
		  :shortest-ioi shortest-ioi
		  ; (if tempo-supplied-p :tempo tempo)
		  :sample-rate sample-rate))

;  (clap-signal (make-double-array (.array-dimensions rhythm-signal) :initial-element 0d0))
;  (map nil (lambda (index) (setf (.aref clap-signal index) max-computed-scale)) (val onsets))

(defmethod sample-length ((rhythm-to-analyse rhythm))
  (.length (time-signal rhythm-to-analyse)))

(defmethod duration ((rhythm-to-analyse rhythm))
  (/ (sample-length rhythm-to-analyse) (.* 1.0 (sample-rate rhythm-to-analyse))))

(defmethod rhythm-iois ((rhythm-to-analyse rhythm))
  (./ (.* 1d0 (.diff (.find (time-signal rhythm-to-analyse))))
      (sample-rate rhythm-to-analyse)))

(defmethod plot-rhythm ((rhythm-to-plot rhythm))
  (plot (time-signal rhythm-to-plot) nil
	 :label (format nil "Rhythm onsets of ~a" (description rhythm-to-plot))
	 :style "impulses linetype 6"
	 :xlabel "Time"
	 :ylabel "Scaled Intensity"
	 :title (format nil "Rhythm of ~a" (name rhythm-to-plot))
	 :aspect-ratio 0.66))

(defmethod save-rhythm ((rhythm-to-save rhythm))
  (save-scorefile (format nil "/Users/leigh/~a.score" (name rhythm-to-save)) 
		  ;; Convert to seconds.
		(nlisp::array-to-list (rhythm-iois rhythm-to-save))
		:instrument "midi"
		:midi-channel 10
		:description (description rhythm-to-save)))

(defun preferred-tempo (rhythm-scaleogram rhythm-sample-rate 
			;; Fraisse's "spontaneous tempo" interval in seconds
			&key (tempo-salience-peak 0.600))
  "Determine the scale which Fraisse's spontaneous tempo would occur at.
This is weighted by absolute constraints, look in the 600ms period range."
  (let* ((salient-IOI (* rhythm-sample-rate tempo-salience-peak)))
    ;; Convert salient-IOI into the scale to begin checking.
    ;; This assumes the highest frequency scale (shortest time support)
    ;; is index 0, lowest frequency (longest time support) is number-of-scales.
    (floor (scale-from-period salient-IOI (voices-per-octave rhythm-scaleogram)))))

;; Scale index 0 is the highest frequency (smallest dilation) scale.
(defun tempo-salience-weighting (salient-scale time-frequency-dimensions)
  "Produce a weighting matching the analysis window using tempo preference."
  (let* ((number-of-scales (first time-frequency-dimensions))
	 (time-in-samples (second time-frequency-dimensions))
	 (tempo-weighting-over-time (make-double-array time-frequency-dimensions))
	 ;; Create a Gaussian envelope spanning the number of scales.
	 ;; Match the mean to a span across -5 < mean < 5 standard deviations.
	 (tempo-scale-weighting (gaussian-envelope number-of-scales 
						   :mean (- (/ (* 10.0 salient-scale) number-of-scales) 5.0)
						   :stddev 2.0d0 ; keeps the weighting broad
						   :scaling 1d0)))
    (dotimes (time time-in-samples)
      (setf-subarray (val tempo-weighting-over-time) 
		     (val (.reshape tempo-scale-weighting (list number-of-scales 1))) (list t time)))
    tempo-weighting-over-time))

;; (plot (.column (tempo-salience-weighting 78 '(144 1)) 0) nil :title "Preferred tempo weighting profile")

(defun normalise-by-scale (magnitude)
  "Normalise a magnitude finding the maximum scale at each time point.
 We assume values are positive values only"
  (let* ((time-frequency-dimensions (.array-dimensions magnitude))
	 (number-of-scales (first time-frequency-dimensions))
	 (time-in-samples (second time-frequency-dimensions))
	 (normalised-values (make-double-array time-frequency-dimensions)))
    ;; Duplicate the maximum scales per time for each scale to enable element-wise division.
    (dotimes (i time-in-samples)
      (let* ((scales-per-time (.column magnitude i))
	     ;; Determine the maximum scale at each time.
	     (max-scale-per-time (.max (.abs scales-per-time)))
	     ;; Normalise each time-slice.
	     ;; If a maximum scale value is 0, then make it 1 to keep the division healthy.
	     (normalised-time-slice (./ scales-per-time (if (zerop max-scale-per-time) 1 max-scale-per-time))))
	;; scales-per-time will be returned as a vector, we need to reshape it in order to
	;; store it.
	(setf-subarray (val normalised-values) 
		       (val (.reshape normalised-time-slice (list number-of-scales 1)))
		       (list t i))))
    normalised-values))

;;thresholdMag = magnitude > threshold;
;;newphase = (thresholdMag .* phase) + (!thresholdMag .* clamp);
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
	 (number-of-scales (first time-frequency-dimensions))
	 (time-in-samples (second time-frequency-dimensions))
	 ;; (ridges (make-double-array time-frequency-dimensions))
	 (signal-phase-diff (make-double-array time-frequency-dimensions))

	 ;; Compute a discrete approximation to the partial derivative of the
	 ;; phase with respect to translation (time) t.
	 (dt-phase (.diff phase))

	 ;; Phase is -pi -> pi.
	 ;; We need to add back 2 pi to compensate for the phase wraparound
	 ;; from pi to -pi. This wraparound will create a dt-phase close to 2 pi.
	 (which-wrapped (.> (.abs dt-phase) (* pi 1.5))) ; anything > pi is a wraparound
	 (phase-wrap (.- (* 2 pi) (.abs dt-phase)))
	 (wrapped-dt-phase (.+ (.* (.not which-wrapped) dt-phase) (.* phase-wrap which-wrapped)))

	 ;; The acceleration of phase difference should be non-zero (Tchamitchian and
	 ;; Torresani Eq. III-7 pp128) phase-diff-acceleration will therefore be two time
	 ;; sample points less than the original phase time extent.
	 ;; phase-diff-acceleration = (.diff wrapped-dt-phase)
	 ;;
	 ;; Doing this will still produce NaN since the division already
	 ;; creates Inf and the clamping uses multiplication.
	 ;; wrapped-dt-phase = (clamp-to-bounds wrapped-dt-phase magnitude magnitude-minimum 1)

	 ;; convert the phase derivative into periods in time
	 (signal-period (./ (* 2 pi) wrapped-dt-phase))
	 (scale-time-support (time-support (.iseq 0 (1- number-of-scales)) voices-per-octave))
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

    (dotimes (i number-of-scales)
      ;; Accept if the signal period (from its phase derivatives) and the time support of
      ;; the wavelet at each dilation scale are within 1.5 sample of one another.
      (setf-subarray (val signal-phase-diff)
		     (val (.abs (.- (.subarray signal-period (list i t)) (.aref scale-time-support i))))
		     (list i (1- time-in-samples))))

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
  (declare (ignore congruency-threshold))
  (let* ((time-frequency-dimensions (.array-dimensions phase))
	 (number-of-scales (first time-frequency-dimensions))

	 ;; Find derivative with respect to scale s. Transposes phase so that diffence is
	 ;; across rows (i.e scales).
	 (abs-ds-phase (.abs (.diff (.transpose phase))))

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
	 ;; (no-outliers)
	 (local-pc-for-mag))
    ;; Since we produce the derivative from the difference, local-phase-diff is one
    ;; element less.
    (setf-subarray (val congruency) (val (.transpose local-phase-diff)) (list (list 1 (1- number-of-scales)) t))

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
    (setf maximal-local-pc (.- 1d0 (.normalise congruency)))

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

;;; TODO alternatively return normalised-magnitude, stationary-phase or
;;; local-phase-congruency individually.
;;; Should use a functional approach, passing the functions applicable as a list:
;;; (&key analyzers '(#'stationary-phase #'local-phase-congruency #'.normalise))
(defun correlate-ridges (magnitude phase voices-per-octave)
  "Computes independent surfaces analysing the magnitude and phase
which are then combined to form an analytic surface from which we
then can extract ridges."
  ;; ahh, parallel machine anyone??
  (let ((stat-phase (stationary-phase magnitude phase voices-per-octave))
	(local-pc (local-phase-congruency magnitude phase))
	;; (normalised-magnitude (.normalise magnitude))
	(normalised-magnitude (normalise-by-scale magnitude)))
    ;; correlate (by averaging) the energy modulus, stationary phase and
    ;; local phase congruency. Since stationary phase and local phase congruency are both
    ;; conditioned on a minimal magnitude, we reduce false positives.
    (./ (.+ normalised-magnitude stat-phase local-pc) 3d0)))

(defun shift-scales (scales direction)
  "Shifts the rows (rotating) upwards (towards lower indices) if direction = :up, down if direction = :down"
  (let* ((time-frequency-dimensions (.array-dimensions scales))
	 (last-scale (1- (first time-frequency-dimensions)))
	 (scales-shifted (make-double-array time-frequency-dimensions))
	 (matrix-position (if (eql direction :up)
			      (list (list (list 1 last-scale) t) (list (list 0 (1- last-scale)) t))
			      (list (list (list 0 (1- last-scale)) t) (list (list 1 last-scale) t))))
	 (rotated-row (if (eql direction :up) 
			  (list (list 0 t) (list last-scale t)) 
			  (list (list last-scale t) (list 0 t)))))
    (setf-subarray (val scales-shifted) (val (.subarray scales (first matrix-position))) (second matrix-position))
    (setf-subarray (val scales-shifted) (val (.subarray scales (first rotated-row))) (second rotated-row))
    scales-shifted))

(defun determine-scale-peaks (correlated-profile &key (correlation-minimum 0.01))
  "Finds the peaks in the combined correlation profile 
  (of energy modulus, stationary phase and local phase congruency)
   across the dilation scale axis at each time point."
  (let* ((profile-shifted-up (shift-scales correlated-profile :up))
	 (profile-shifted-down (shift-scales correlated-profile :down)))
    ;; the multiplication retains the scale peak values, clamping non maxima to zero.
    (.* (.and (.> correlated-profile profile-shifted-down) 
	      (.> correlated-profile profile-shifted-up) 
	      (.> correlated-profile correlation-minimum))
	correlated-profile)))

(defun scale-peaks-of-scaleogram (scaleogram sample-rate)
  (let* ((magnitude (scaleogram-magnitude scaleogram)) ; short-hand.
	 (phase (scaleogram-phase scaleogram))
	 ;; Correlate various ridges to produce a robust version.
	 (correlated-ridges (correlate-ridges magnitude phase (voices-per-octave scaleogram)))
	 ;; Scale index 1 is the highest frequency (smallest dilation) scale.
	 (salient-scale (preferred-tempo scaleogram sample-rate))
	 (tempo-weighting (tempo-salience-weighting salient-scale (.array-dimensions magnitude)))
	 ;; Weight by the absolute tempo preference.
	 (tempo-weighted-ridges (.* correlated-ridges tempo-weighting)))
    (format t "Preferred tempo scale = ~d of ~d hierarchy~%" salient-scale (.array-dimension magnitude 0))
    ;; (plot (.column tempo-weighting 0) nil :title "Preferred tempo weighting profile")
    ;; show what we got as an intensity plot
    ;; (setf *magnitude-colour-map* #'jet-colormap)
    ;; This tends to flatten everything out...
    (plot-image #'magnitude-image "-correlation" (list correlated-ridges) :title "ridges") 
    (plot-image #'magnitude-image "-correlation" (list tempo-weighted-ridges) :title "tempo-ridges")
    ;; :title (name analysis-rhythm))
    ;; substituted tempo-weighted-ridges for correlated-ridges to enable tempo selectivity.
    (determine-scale-peaks tempo-weighted-ridges)))
    ;; (determine-scale-peaks correlated-ridges) ; for no tempo weighting

(defmethod scaleogram-of-rhythm ((analysis-rhythm rhythm) &key (voices-per-octave 16))
  (cwt (time-signal analysis-rhythm) voices-per-octave))

(defmethod skeleton-of-rhythm ((analysis-rhythm rhythm) &key (voices-per-octave 16))
  "Returns the skeleton given the rhythm."
  (let* ((scaleogram (scaleogram-of-rhythm analysis-rhythm :voices-per-octave voices-per-octave))
	 (correlated-ridge-scale-peaks (scale-peaks-of-scaleogram scaleogram (sample-rate analysis-rhythm)))
       	 (skeleton (extract-ridges correlated-ridge-scale-peaks)))
    ;; (plot-cwt scaleogram :title (name analysis-rhythm))
    (values skeleton scaleogram correlated-ridge-scale-peaks)))

(defmethod tactus-for-rhythm ((analysis-rhythm rhythm) 
			      &key (voices-per-octave 16)
			      (tactus-selector #'select-longest-lowest-tactus))
  "Returns the selected tactus given the rhythm."
  (multiple-value-bind (skeleton scaleogram correlated-ridge-scale-peaks)
      (skeleton-of-rhythm analysis-rhythm :voices-per-octave voices-per-octave)
    (let ((chosen-tactus (funcall tactus-selector skeleton)))   ; select out the tactus from all ridge candidates.
      (format t "Computed skeleton and chosen tactus~%")
      (plot-cwt scaleogram :title (name analysis-rhythm))
      (plot-cwt+tactus scaleogram chosen-tactus :title (name analysis-rhythm))
      (plot-ridges+tactus correlated-ridge-scale-peaks chosen-tactus :title (name analysis-rhythm))
      (format t "Finished plotting scalograms~%")
      (values chosen-tactus scaleogram))))

;;; TODO We could set any ill-conditioned phase (negligible magnitude) to zero using this function
;;; after we take the phase derivative (in stationary-phase) as it would
;;; create false changes.
(defun clean-phase (magnitude phase &key (threshold 0.001) (clamp 0.0))
  "We clamp any ill-conditioned phase (negligble magnitude) to the value given (defaulting to zero)"
  (clamp-to-bounds phase magnitude :low-bound threshold :clamp-low clamp))

;; TODO Return ((time intensity) (time intensity) ...)
;; use constant intensity claps but weight the amplitude for when we mix in Common Music.
(defun clap-to-tactus-phase (original-rhythm rhythm-scaleogram tactus 
			     &key (start-from-beat 0))
  "Function to compute times to clap and how hard from the extracted tactus.
   Returns a matrix where row 1 is the time to tap at, row 2 is the intensity."
  (let* ((time-frequency-dimensions (.array-dimensions (scaleogram-magnitude rhythm-scaleogram)))
	 (time-in-samples (second time-frequency-dimensions))

	 ;; The right way to do this is with a tight (well, two octave)
	 ;; Gaussian envelope over it.
	 ;; tactus-mag (gaussian-envelope tactus voices-per-octave)
	 ;; But just a single voice alone produces the correct oscillation, with lower amplitude:
	 (tactus-mag (insert-ridge tactus (make-double-array time-frequency-dimensions) :constant-value 1d0))
  
	 ;; TODO To be completely correct the original padded output from dyadic-cwt should be used
	 ;; for input to the dyadic-icwt.
	 ;; TODO create a scalogram
	 ;; (clamped-magnitude-scaleogram (copy-instance rhythm-scaleogram))
	 ;; (setf (scaleogram-magnitude clamped-magnitude-scalogram) tactus-mag)
	 ;; (icwt clamped-magnitude-scaleogram)
	 ;; Use the modified magnitude and original phase to compute a sinusoid and it's
	 ;; Hilbert transform, from that, determine the phase of the sinusoid.
	 (foot-tap-phase (.phase (icwt tactus-mag
				       (scaleogram-phase rhythm-scaleogram)
				       (voices-per-octave rhythm-scaleogram))))

	 ;; Note the phase of the oscillating sinusoid at the beat to start tapping from.
	 (down-beat-sample (time-of-beat original-rhythm start-from-beat))
	 (clap-on-phase-datum (.aref foot-tap-phase down-beat-sample))

	 ;; check clap-on-phase-datum >= current and < next phase measure.
	 ;; TODO this could be a problem on the last phase point before 2 pi wrap.
	 ;; identify reoccurance of the initial clap phase across the translation (time) axis
	 (phase-reoccured (.and (.>= clap-on-phase-datum 
				     (.subarray foot-tap-phase (list 0 (list 0 (- time-in-samples 2))))) 
				(.< clap-on-phase-datum 
				    (.subarray foot-tap-phase (list 0 (list 1 (1- time-in-samples)))))))
	 (clap-at (.find phase-reoccured))
	 ;; Create a clapping rhythm
	 (clap-rhythm (rhythm-of-onsets (name original-rhythm) clap-at)))

    (format t "Handclapping from beat ~d of original rhythm, sample ~d~%" start-from-beat down-beat-sample)
    (plot-claps (time-signal original-rhythm) 
		clap-at 
		:foot-tap-AM foot-tap-phase 
		:signal-description (name original-rhythm))
    clap-at))

(defmethod clap-to-rhythm ((performed-rhythm rhythm) &key (tactus-selector #'select-longest-lowest-tactus))
  "Returns a set of sample times to clap to given the supplied rhythm"
  (multiple-value-bind (computed-tactus rhythm-scaleogram)
      (tactus-for-rhythm performed-rhythm :tactus-selector tactus-selector)
    (clap-to-tactus-phase performed-rhythm rhythm-scaleogram computed-tactus)))

;; Approaches:
;; 1. Count the number of ridges.
;; 2. Each ridge is tempo weighted by each of it's scales. Therefore 1 long ridge would be more complex than shorter ridges.
;; 3. Weight the entire scale peaks or magnitude measure by tempo.
;; 4. Ease of handclapping as a measure of complexity?

;;; Could use a weighted bitmap of the ridges as a general density measure. This is
;;; weighted by absolute tempo. It should also be weighted by the number of ridges (a
;;; dense single ridge is less complex than one which is composed of many small ridges.
(defmethod rhythm-complexity ((rhythm-to-analyse rhythm))
  "Returns a normalised measure of the complexity of the rhythm, where 0 = impossibly simple -> 1 = impossibly hard."
  (let ((skeleton (skeleton-of-rhythm rhythm-to-analyse)))
    (length skeleton)))
