;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;; 
;;;; A multiresolution model of musical rhythm.
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006, 2007 All Rights Reserved.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; See multiresrhythm.asd for further info.
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

(defparameter *mra-cache-path* "/Users/leigh/Data/")

;;; An analysis includes a textual description, skeleton, scaleogram and
;;; correlated-ridge-scale-peaks instances. 
(defclass multires-analysis ()
  ((description   :initarg :description   :accessor description :initform "")
   (skeleton      :initarg :skeleton      :accessor skeleton)
   (scaleogram    :initarg :scaleogram    :accessor scaleogram)
   (ridge-peaks   :initarg :ridge-peaks   :accessor ridge-peaks)
   (ridge-troughs :initarg :ridge-troughs :accessor ridge-troughs)
   (sample-rate   :initarg :sample-rate   :accessor sample-rate)))

;;;; Declarations
(defgeneric choose-tactus (rhythm-to-analyse analysis &key tactus-selector)
  (:documentation "Returns the selected tactus for the rhythm."))

(defgeneric tactus-for-rhythm (rhythm-to-analyse &key voices-per-octave tactus-selector)
  (:documentation "Returns the selected tactus for the rhythm."))

(defgeneric scaleogram-of-rhythm (rhythm-to-analyse &key voices-per-octave)
  (:documentation "Returns the scaleogram for the rhythm."))

(defgeneric skeleton-of-scaleogram (scaleogram sample-rate)
  (:documentation "Returns the skeleton given the scaleogram and sample-rate."))

(defgeneric analysis-of-rhythm (rhythm-to-analyse &key voices-per-octave)
  (:documentation "Returns a multires-analysis instance for the given rhythm."))

(defgeneric analysis-of-rhythm-cached (rhythm-to-analyse &key voices-per-octave cache-directory)
  (:documentation "Reads the skeleton, scaleogram and ridge-scale-peaks from disk if they have been cached,
otherwise, generates them and writes to disk, and returns a multires-analysis instance."))

;;; Tactus selectors
(defgeneric select-longest-lowest-tactus (rhythm-to-analyse multires-rhythm)
  (:documentation "Returns the longest duration and lowest scale ridge."))

;;;; Implementation

(defmethod duration-in-samples ((analysis multires-analysis))
  "Returns the duration of the skeleton, which should match the rhythm"
  (duration-in-samples (scaleogram analysis)))

;; Fastest scale for tactus 
;; (scale-from-period (* 200 0.25) 16)
;; Slowest scale for tactus
;; (scale-from-period (* 200 2.0) 16)
;; Slowest scale "accessible to the senses" (London 2004)
;; (scale-from-period (* 200 5.0) 16)

(defun preferred-tempo-scale (voices-per-octave rhythm-sample-rate 
			      ;; Fraisse's "spontaneous tempo" interval in seconds
			      &key (tempo-salience-peak 0.600))
  "Determine the scale which Fraisse's spontaneous tempo would occur at.
This is weighted by absolute constraints, look in the 600ms period range."
  (let* ((salient-IOI (* rhythm-sample-rate tempo-salience-peak)))
    ;; Convert salient-IOI into the scale to begin checking.
    ;; This assumes the highest frequency scale (shortest time support)
    ;; is index 0, lowest frequency (longest time support) is number-of-scales.
    (floor (scale-from-period salient-IOI voices-per-octave))))

;;; Scale index 0 is the highest frequency (smallest dilation) scale.
;;; TODO this should be weighted by the log scale character, rather than symmetrical.
(defun tempo-salience-weighting (salient-scale time-frequency-dimensions &key (voices-per-octave 16)
				 ;; Define a doubling in frequency, 1 octave as 1 stddev.
				 (octaves-per-stddev 1.0))
  "Produce a weighting matching the analysis window using tempo preference."
  (let* ((number-of-scales (first time-frequency-dimensions))
	 (time-in-samples (second time-frequency-dimensions))
	 (tempo-weighting-over-time (make-double-array time-frequency-dimensions))
	 ;; Match the mean to a span across -5 < mean < 5 standard deviations.
	 (stddev-span 10.0)
	 ;; Create a Gaussian envelope spanning the number of scales.
	 (tempo-scale-weighting (gaussian-envelope number-of-scales 
						   :mean (- (/ (* stddev-span salient-scale) number-of-scales) (/ stddev-span 2.0))
						   :stddev (/ (* voices-per-octave stddev-span octaves-per-stddev) number-of-scales)
						   :scaling 1d0)))
    (dotimes (time time-in-samples)
      (setf (.subarray tempo-weighting-over-time (list t time))
	    (.reshape tempo-scale-weighting (list number-of-scales 1))))
    tempo-weighting-over-time))

;; (plot (.column (tempo-salience-weighting 78 '(144 1) :voices-per-octave 16.0) 0) nil :title "Preferred tempo weighting profile")

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
	(setf (.subarray normalised-values (list t i))
	      (.reshape normalised-time-slice (list number-of-scales 1)))))
    normalised-values))

(defun phase-diff (phase)
  "Compute the first order differences of a phase signal, such that we take into account
  wrap around at the pi and -pi boundaries."
  (let* ((dt-phase (.diff phase))
	 (abs-dt-phase (.abs dt-phase))
	 ;; Phase is -pi -> pi.
	 ;; We need to add back 2 pi to compensate for the phase wrap-around from pi to -pi. 
	 ;; This wrap-around will create a dt-phase close to 2 pi.
	 (which-wrapped (.> abs-dt-phase pi)) ; anything > pi is a wrap-around
	 (phase-wrap (.- (* 2 pi) abs-dt-phase)))
    (.+ (.* (.not which-wrapped) dt-phase) (.* phase-wrap which-wrapped))))

(defun stationary-phase (magnitude phase voices-per-octave &key (magnitude-minimum 0.01)
			 (phase-match-threshold 4d0))
  "Compute points of stationary phase. Implementation of Delprat
   et. al's ridges from points of stationary phase convergent
   algorithm. See: delprat:asymptotic

   Inputs
     magnitude    Magnitude output of CWT
     phase        Phase output of CWT
     voices-per-octave Dilation resolution.
     magnitude-minimum is the minimum magnitude to accept phase as valid
   Outputs
     binary array indicating presence of ridge."
  ;; the scale is the central frequency of the mother wavelet divided by
  ;; the central frequency of the dilated wavelet.
  (let* ((time-frequency-dimensions (.array-dimensions phase))
	 (number-of-scales (first time-frequency-dimensions))
	 (time-in-samples (second time-frequency-dimensions))
	 (last-time (- time-in-samples 2)) ; phase-diff will reduce the length by 1.
	 (signal-phase-diff (make-double-array time-frequency-dimensions))

	 ;; Compute a discrete approximation to the partial derivative of the
	 ;; phase with respect to translation (time) t.
	 (wrapped-dt-phase (phase-diff phase))

	 ;; When we have neglible magnitude, phase is meaningless, but can oscillate so
	 ;; rapidly we have two adjacent phase values which are equal (typically pi, pi/2,
	 ;; or 0). This can cause wrapped-dt-phase to be zero and therefore make
	 ;; signal-period NaN.  Our kludge is to set those dt-phase values to 2pi so
	 ;; signal-period will be 1 and they will be zeroed out of the final result with
	 ;; the magnitude-minimum clamp-to-bounds. Strictly speaking, we should verify mag
	 ;; is below magnitude-minimum before changing these zero wrapped-dt-phase values.
	 (protected-dt-phase (.+ wrapped-dt-phase (.* (.not wrapped-dt-phase) (* 2 pi))))

	 ;; The acceleration of phase difference should be non-zero (Tchamitchian and
	 ;; Torresani Eq. III-7 pp128) phase-diff-acceleration will therefore be two time
	 ;; sample points less than the original phase time extent.
	 ;; (phase-diff-acceleration (.diff wrapped-dt-phase))

	 ;; Convert the phase derivative into periods in time.
	 (signal-period (./ (* 2 pi) protected-dt-phase))
	 ;; Calculate the time support of each dilated wavelet.
	 (scale-time-support (time-support (.iseq 0 (1- number-of-scales)) voices-per-octave))
	 (maximal-phase-diff)
	 (clipped-phase-diff))

    ;; Compute the difference between the signal period (from its phase derivatives) and the time support of
    ;; the wavelet at each dilation scale.
    (dotimes (scale-index number-of-scales)
      (setf (.subarray signal-phase-diff (list scale-index (list 0 last-time)))
	    (.abs (.- (.row signal-period scale-index) (.aref scale-time-support scale-index)))))

    ;; Should accept if the signal period (from its phase derivatives) and the time support of
    ;; the wavelet at each dilation scale are within phase-match-threshold samples of one another.
    (setf maximal-phase-diff (.- 1d0 (./ signal-phase-diff phase-match-threshold)))
    (setf clipped-phase-diff (clamp-to-bounds maximal-phase-diff maximal-phase-diff :low-bound 0d0 :clamp-low 0d0))

    ;; There must be some magnitude at the half-plane points of stationary phase for the ridge to be valid.
    (clamp-to-bounds clipped-phase-diff magnitude :low-bound magnitude-minimum :clamp-low 0d0)))

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
	 ;; TODO replace all above with: 
	 ;; (local-phase-diff (.abs (phase-diff (.transpose phase))))

	 ;; The local phase congruency measure should be maximal at points of minimal
	 ;; difference between scales, so we normalise it, then subtract from 1.
	 ;; (maximal-local-pc (.- 1d0 (.normalise local-phase-diff)))
	 (maximal-local-pc (.- 1d0 (./ local-phase-diff pi)))
	 (congruency (make-double-array time-frequency-dimensions))) ; what we will return.
    ;; Since we produce the derivative from the difference, maximal-local-pc is one element less.
    ;; Alternative would be to interpolate the result to produce the correct value for each scale.
    ;; (congruency (./ (.abs (.+ (.concatenate local-phase-diff scale-pad) 
    ;; (.concatenate scale-pad local-phase-diff)))) 2)
    ;; or
    ;; (congruency (.transpose local-phase-diff))
    ;; (format t "local-phase-diff range (~a ~a)~%" (.min local-phase-diff) (.max local-phase-diff))
    ;; (format t "maximal-local-pc range (~a ~a)~%" (.min maximal-local-pc) (.max maximal-local-pc))
    (setf (.subarray congruency (list (list 1 (1- number-of-scales)) t))
	  (.transpose maximal-local-pc))

    ;; There must be some magnitude at the half-plane points of local phase congruency for
    ;; the ridge to be valid.  
    (clamp-to-bounds congruency magnitude :low-bound magnitude-minimum :clamp-low 0)))

#|
    ;; determine outliers: those troughs greater in difference than threshold.
    (setf no-outliers (.< congruency congruency-threshold))

    ;; Remove outliers.
    (.* local-pc-for-mag no-outliers)))
|#


(defun local-phase-congruency-new (magnitude phase &key (magnitude-minimum 0.01))
  "Compute points of local phase congruency.
   Determines the points of phase with least deviation between adjacent scales.

  Inputs
    magnitude Magnitude output of CWT.
    phase  Phase output of CWT
    magnitude-minimum is the minimum magnitude to accept phase as valid.
  Outputs
    matrix indicating presence of congruency."
  (let* ((time-frequency-dimensions (.array-dimensions phase))
	 (number-of-scales (first time-frequency-dimensions))
	 ;; Transposes phase so that diffence is across rows (i.e scales).
	 (phase-transpose (.transpose phase))
	 ;; polar derivative with respect to scale s. 
	 (ds-diff (.+ (.abs (.diff (.sin phase-transpose))) (.abs (.diff (.cos phase-transpose)))))
	 ;; normalise the difference, trancendental functions will never be simultaneously
	 ;; larger than 1.0 on the sin and cos axes. Max diff on the projections is
	 ;; therefore +/- 2.0.
	 (norm-ds-diff (./ (.+ ds-diff 2.0d0) 4.0d0))
	 ;; invert, so that minimal difference between scales is maximum local phase congruency.
	 (maximal-local-pc (.- 1d0 (.transpose norm-ds-diff)))
	 (congruency (make-double-array time-frequency-dimensions))) ; what we will return.
    (format t "ds-diff range (~a ~a)~%" (.min ds-diff) (.max ds-diff))
    (format t "norm-ds-diff range (~a ~a)~%" (.min norm-ds-diff) (.max norm-ds-diff))
    (format t "maximal-local-pc range (~a ~a)~%" (.min maximal-local-pc) (.max maximal-local-pc))
    ;; Since we produce the derivative from the difference, local-phase-diff is one element less.
    (setf (.subarray congruency (list (list 1 (1- number-of-scales)) t)) maximal-local-pc)
    ;; There must be some magnitude at the half-plane points of local phase congruency for
    ;; the ridge to be valid.
    (clamp-to-bounds congruency magnitude :low-bound magnitude-minimum :clamp-low 0)))

(defun phase-congruency (magnitude phase)
  "Calculate a dimensionless measure of local energy at each time point,
given magnitude and phase components of the wavelet coefficients. 
Phase is assumed to be -pi to pi."
  (let* ((real-bit (.* magnitude (.cos phase)))
	 (imag-bit (.* magnitude (.sin phase)))
	 (local-energy (.sqrt (.+ (.expt (.partial-sum real-bit) 2) (.expt (.partial-sum imag-bit) 2))))
	 (local-magnitude (.sum magnitude)))
    (./ local-energy local-magnitude)))

;;; TODO alternatively return normalised-magnitude, stationary-phase or
;;; local-phase-congruency individually.
;;; Should use a functional approach, passing the functions applicable as a list:
;;; (&key analyzers '(#'stationary-phase #'local-phase-congruency #'.normalise))
;;; TODO Should use parabolic interpolation to pick the maximum point of the modulus.
;;; However this may not improve things if the magnitude itself wanders due to interaction
;;; between voices.
;;; "Degree-two polynomial interpolation, i.e. parabolic interpolation, is particularly convenient as it uses
;;; only three bins of the magnitude spectrum."

#|
(defun correlate-ridges (magnitude phase voices-per-octave)
  "Computes independent surfaces analysing the magnitude and phase
which are then combined to form an analytic surface from which we
then can extract ridges."
  ;; ahh, parallel machine anyone??
  (let ((local-pc (local-phase-congruency magnitude phase))
	(normalised-magnitude (normalise-by-scale magnitude)))
    ;; Correlate (by averaging) the energy modulus and local phase congruency. 
    ;; Since local phase congruency is conditioned on a minimal magnitude, we reduce false positives.
    ;; Stationary phase has been dropped for rhythm data since it mostly introduces
    ;; spurious ridges, not really improving discrimination of ridges.
    (./ (.+ normalised-magnitude local-pc) 2d0)))
|#

(defun correlate-ridges (magnitude phase voices-per-octave)
  "Computes independent surfaces analysing the magnitude and phase
which are then combined to form an analytic surface from which we
then can extract ridges."
  (let ((normalised-magnitude (normalise-by-scale magnitude)))
    ;; Correlate (by averaging) the energy modulus and local phase congruency. 
    ;; Since local phase congruency is conditioned on a minimal magnitude, we reduce false positives.
    ;; Stationary phase has been dropped for rhythm data since it mostly introduces
    ;; spurious ridges, not really improving discrimination of ridges.
    normalised-magnitude))

(defun determine-scale-peaks (correlated-profile &key (correlation-minimum 0.01))
  "Finds the peaks in the combined correlation profile 
  (of energy modulus, stationary phase and local phase congruency)
   across the dilation scale axis at each time point."
  (.* (.> correlated-profile correlation-minimum)
      (extrema-points correlated-profile :extrema :max) 
      correlated-profile))

(defun scale-peaks-of-scaleogram (scaleogram sample-rate &key (absolute-tempo-weighting nil))
  "Return the peaks of the scaleogram by correlating the magnitude, local phase congruency
and stationary phase measures, optionally weighed by absolute tempo preferences."
  (let* ((magnitude (scaleogram-magnitude scaleogram)) ; short-hand.
	 (phase (scaleogram-phase scaleogram))
	 (vpo (voices-per-octave scaleogram))
	 ;; Correlate various ridges to produce a robust version.
	 (correlated-ridges (correlate-ridges magnitude phase vpo))
	 ;; Scale index 1 is the highest frequency (smallest dilation) scale.
	 (salient-scale (preferred-tempo-scale (voices-per-octave scaleogram) sample-rate))
	 (tempo-weighting (tempo-salience-weighting salient-scale (.array-dimensions magnitude)))
	 ;; Weight by the absolute tempo preference.
	 (tempo-weighted-ridges (.* correlated-ridges tempo-weighting)))
    (cond (absolute-tempo-weighting
	   (format t "Preferred tempo scale = ~d of ~d hierarchy, ~f samples, ~f seconds duration.~%" 
		   salient-scale (.array-dimension magnitude 0) (time-support salient-scale vpo)
		   (time-support-seconds salient-scale vpo sample-rate))
	   (plot (.column tempo-weighting 0) nil :title "Preferred tempo weighting profile")))
    ;; show what we got as an intensity plot
    ;; (setf *magnitude-colour-map* #'jet-colormap)
    ;; This tends to flatten everything out...
    ;; (plot-image #'magnitude-image (list correlated-ridges) :title "ridges") 
    ;; (plot-image #'magnitude-image (list tempo-weighted-ridges) :title "tempo-ridges of " (name analysis-rhythm)))
    ;; Substitutes tempo-weighted-ridges for correlated-ridges to enable tempo selectivity.
    (determine-scale-peaks (if absolute-tempo-weighting tempo-weighted-ridges correlated-ridges))))

(defmethod scaleogram-of-rhythm ((analysis-rhythm rhythm) &key (voices-per-octave 16))
  (format t "Length of rhythm \"~a\" is ~f seconds~%" (name analysis-rhythm) (duration analysis-rhythm))
  (cwt (time-signal analysis-rhythm) voices-per-octave))

(defmethod skeleton-of-scaleogram ((analysis-scaleogram scaleogram) sample-rate)
  "Returns the skeleton given the scaleogram and sample-rate."
  (let* ((correlated-ridge-scale-peaks (scale-peaks-of-scaleogram analysis-scaleogram sample-rate))
       	 (skeleton (skeleton-of-ridge-peaks analysis-scaleogram correlated-ridge-scale-peaks)))
    (values skeleton correlated-ridge-scale-peaks)))

(defmethod analysis-of-rhythm ((analysis-rhythm rhythm) &key (voices-per-octave 16))
  "Returns the multires analysis of the given rhythm."
  (let* ((sample-rate (sample-rate analysis-rhythm))
	 (scaleogram (scaleogram-of-rhythm analysis-rhythm :voices-per-octave voices-per-octave))
	 (correlated-ridge-scale-peaks (scale-peaks-of-scaleogram scaleogram sample-rate))
	 (skeleton (skeleton-of-ridge-peaks scaleogram correlated-ridge-scale-peaks)))
    (make-instance 'multires-analysis 
		   :description (description analysis-rhythm)
		   :scaleogram scaleogram
		   :skeleton skeleton
		   :ridge-peaks correlated-ridge-scale-peaks
		   :ridge-troughs (extrema-points (scaleogram-magnitude scaleogram) :extrema :min)
		   :sample-rate sample-rate)))

;;; Should all be in tactus-selection.lisp
(defmethod select-tactus-by-beat-multiple ((performed-rhythm rhythm) (rhythm-analysis multires-analysis))
  "Returns the ridges that are the most commonly occurring lowest integer multiples of the beat"
  (let* ((skeleton (skeleton rhythm-analysis))
	 (vpo (voices-per-octave skeleton))
	 ;; TODO Try Gaussian weighting the scale-persistency?
 	 (beat-period (unweighted-beat-period-of-rhythm performed-rhythm (scaleogram rhythm-analysis)))
 	 ;; (beat-scale (scale-from-period beat-period vpo))
 	 (beat-multiples (.iseq 1 7))  ; determines the bar periods
 	 (candidate-bar-periods (.* beat-period beat-multiples))
 	 (candidate-bar-scales (scale-from-period candidate-bar-periods vpo)))
    (format t "Checking multiples for beat period ~a, as ~a~%" beat-period (.round candidate-bar-scales))
    (loop
       for candidate-bar-scale across (val (.round candidate-bar-scales))
       ;; Retrieve ridges possessing a candidate-bar-scale
       for candidate-bar-scale-ridges = (ridges-containing-scale skeleton candidate-bar-scale)
       ;; Measure total duration covered by all candidate bar scale ridges, 
       for candidate-ridge-duration = (reduce #'+ (mapcar #'duration-in-samples candidate-bar-scale-ridges))
       ;; TODO weight by distances of scales in ridge from the nominated scale.
       ;; do (format t "For candidate-bar-scale ~a, period ~a~%" candidate-bar-scale (time-support candidate-bar-scale vpo))
       ;; Longest total duration is the selected ridge.
       maximizing candidate-ridge-duration into max-candidate-ridge-duration
       collecting (list candidate-ridge-duration candidate-bar-scale-ridges) into ridge-totals
       ;; finally (format t "candidate ridges and durations ~a~%" ridge-totals)
       finally (return (second (find max-candidate-ridge-duration ridge-totals :key #'first))))))

(defmethod select-longest-lowest-tactus ((performed-rhythm rhythm) (rhythm-analysis multires-analysis))
  "Returns the longest duration and lowest scale ridge."
  (let* ((skeleton-to-analyse (skeleton rhythm-analysis))
	 (max-ridge (make-instance 'ridge)))
    (dolist (ridge (ridges skeleton-to-analyse))
      (if (or (> (duration-in-samples ridge) (duration-in-samples max-ridge))
	      ;; average-scale returns scale numbers indexed from the highest scales, so 
	      (and (eql (duration-in-samples ridge) (duration-in-samples max-ridge))
		   (> (average-scale ridge) (average-scale max-ridge))))
	  (setf max-ridge ridge)))
    (list max-ridge)))

(defmethod choose-tactus ((analysis-rhythm rhythm) (analysis multires-analysis)
			  &key (tactus-selector #'select-longest-lowest-tactus))
  "Returns the selected tactus given the rhythm."
  ;; select out the tactus from all ridge candidates.
  (let* ((chosen-tactus (funcall tactus-selector analysis-rhythm analysis))
	 (chosen-tactus-list (if (listp chosen-tactus) chosen-tactus (list chosen-tactus))))
    (format t "Chosen tactus ~a using ~a~%" chosen-tactus-list tactus-selector)
    (diag-plot 'cwt
      (plot-cwt+ridges (scaleogram analysis) chosen-tactus-list analysis-rhythm
		       ;; :phase-palette :greyscale
		       ;; :magnitude-palette :jet
		       :title (name analysis-rhythm)))
    (diag-plot 'tactus
      (plot-highlighted-ridges (scaleogram analysis)
			       chosen-tactus-list
			       (ridge-peaks analysis)
			       :title (name analysis-rhythm)
			       :sample-rate (sample-rate analysis-rhythm))
      (format t "Finished plotting scalograms~%"))
    chosen-tactus-list))

(defmethod tactus-for-rhythm ((analysis-rhythm rhythm) 
			      &key (voices-per-octave 16)
			      (tactus-selector #'select-longest-lowest-tactus))
  "Returns the selected tactus given the rhythm."
  (let* ((analysis (analysis-of-rhythm analysis-rhythm :voices-per-octave voices-per-octave)))
    (values (choose-tactus analysis-rhythm analysis :tactus-selector tactus-selector) analysis)))

;;; TODO This can probably be replaced with choose-tactus above.
#|(defmethod tactus-for-rhythm ((analysis-rhythm rhythm) 
			      &key (voices-per-octave 16)
			      (tactus-selector #'select-longest-lowest-tactus))
  "Returns the selected tactus given the rhythm."
  (let* ((analysis (analysis-of-rhythm analysis-rhythm :voices-per-octave voices-per-octave))
	 (chosen-tactus (funcall tactus-selector analysis-rhythm analysis))   ; select out the tactus from all ridge candidates.
	 (chosen-tactus-list (if (listp chosen-tactus) chosen-tactus (list chosen-tactus))))
    (format t "Computed skeleton and chosen tactus ~a using ~a~%" chosen-tactus-list tactus-selector)
    (plot-cwt+ridges (scaleogram analysis) chosen-tactus-list analysis-rhythm
		     ;; :phase-palette :greyscale
		     ;; :magnitude-palette :jet
		     :title (name analysis-rhythm))
    (plot-highlighted-ridges (scaleogram analysis)
			     chosen-tactus-list
			     (ridge-peaks analysis)
			     :title (name analysis-rhythm)
			     :sample-rate (sample-rate analysis-rhythm))
    (format t "Finished plotting scalograms~%")
    (values chosen-tactus-list analysis)))
|#

(defmethod ridge-persistency-of ((analysis multires-analysis))
  "Returns the ridge persistency of the precomputed ridge peaks"
  (scale-persistency (ridge-peaks analysis)))

;;; To verify the ridge extraction accuracy:
;;; (plot-highlighted-ridges-of-rhythm scaleogram (ridges skeleton) correlated-ridge-scale-peaks analysis-rhythm)
;;; No black ridges should be shown, only red highlighted ridges.

;;; TODO We could set any ill-conditioned phase (negligible magnitude) to zero using this function
;;; after we take the phase derivative (in stationary-phase) as it would
;;; create false changes.
(defun clean-phase (magnitude phase &key (threshold 0.001) (clamp 0.0))
  "We clamp any ill-conditioned phase (negligble magnitude) to the value given (defaulting to zero)"
  (clamp-to-bounds phase magnitude :low-bound threshold :clamp-low clamp))

;; Calculates the persistency of period multiples in the skeleton.
(defun meter-evidence (analysis tactus beat-multiple)
  "Returns the evidence of harmonicity of the beat to the given beat-multiple."
  ;; Get the scales in the ridge and their relative ratios.
  ;; TODO using the first ridge in the list is a hack.
  (multiple-value-bind (tactus-scales scale-weights) (scales-and-weights-in-ridge (first tactus))
    (let* ((skeleton (skeleton analysis))
	   (vpo (voices-per-octave skeleton))
	   (persistency-profile (ridge-persistency-of analysis)) ; (ridge-persistency-of (skeleton analysis))
	   (beat-periods (time-support tactus-scales vpo))
	   (candidate-bar-periods (.* beat-periods beat-multiple))
	   (candidate-scales (prune-to-limit (.round (scale-from-period candidate-bar-periods vpo))
					     (1- (number-of-scales skeleton))))
	   (weighted-meter-evidence (if (> (.length candidate-scales) 0)
					(.sum (.* (.arefs persistency-profile candidate-scales) scale-weights))
					0d0))
	   (unweighted-meter-evidence (.sum (.arefs persistency-profile candidate-scales))))
;;       (format t "beat-periods ~a candidate-bar-periods ~a scale-weights ~a scale-values ~a unweighted meter evidence ~a weighted meter evidence ~a~%" 
;; 	      beat-periods candidate-bar-periods scale-weights 
;; 	      (.arefs persistency-profile candidate-scales) unweighted-meter-evidence weighted-meter-evidence)
;;       (plot-command "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
;; 		    (label-scale-as-time-support-seconds (skeleton analysis) (sample-rate analysis)))
;;       (plot (.reverse persistency-profile) nil :title (description analysis) :aspect-ratio 0.66 :reset nil)
      weighted-meter-evidence)))

(defun meter-of-analysis (analysis tactus)
  "Look for harmonicity of the beat, either duple or triple. Returns the selected beat multiple."
  (let* ((meters '(3 4))
	 (evidence (mapcar (lambda (beat-multiple) (meter-evidence analysis tactus beat-multiple)) meters)))
    (nth (position (apply #'max evidence) evidence) meters)))

;;; File I/O

(defmethod save-to-file ((analysis-to-write multires-analysis) (path-to-write pathname))
  (let* ((skeleton-filename (make-pathname :defaults path-to-write :type "skeleton"))
	 (scaleogram-filename (make-pathname :defaults path-to-write :type "scaleogram"))
	 (ridge-peaks-filename (make-pathname :defaults path-to-write :type "peaks")))
    (format t "Writing ~a~%" skeleton-filename) 
    (save-to-file (skeleton analysis-to-write) skeleton-filename)
    (format t "Writing ~a~%" scaleogram-filename) 
    (save-to-file (scaleogram analysis-to-write) scaleogram-filename)
    (format t "Writing ~a~%" ridge-peaks-filename)
    (.save (ridge-peaks analysis-to-write) ridge-peaks-filename :format :binary)))

(defmethod read-mra-from ((path-to-read pathname))
  "Reads and returns a multires-analysis instance, returns nil when EOF"
  (make-instance 'multires-analysis
		 :skeleton (read-skeleton-from (make-pathname :defaults path-to-read :type "skeleton"))
		 :scaleogram (read-scaleogram-from (make-pathname :defaults path-to-read :type "scaleogram"))
		 :ridge-peaks (.load (make-pathname :defaults path-to-read :type "peaks") :format :binary)))

(defmethod analysis-of-rhythm-cached ((analysis-rhythm rhythm) &key (voices-per-octave 16)
				      (cache-directory *mra-cache-path*))
  "Reads the skeleton, scaleogram and ridge-scale-peaks from disk if they have been cached,
otherwise, generates them and writes to disk."
  (let ((cache-root-pathname (make-pathname :directory cache-directory :name (name analysis-rhythm))))
    (if (probe-file (make-pathname :defaults cache-root-pathname :type "skeleton"))
	(read-mra-from cache-root-pathname)
	(let ((mra (analysis-of-rhythm analysis-rhythm :voices-per-octave voices-per-octave)))
	  (save-to-file mra cache-root-pathname)
	  mra))))
