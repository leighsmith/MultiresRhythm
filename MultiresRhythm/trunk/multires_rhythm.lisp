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

;;;; TODO should we be declaring a MRA class that holds a skeleton, scaleogram and
;;;; correlated-ridge-scale-peaks frames?
;;;; Declarations (see rhythm.lisp for the class definition)

(defgeneric clap-to-rhythm (rhythm-to-analyse &key tactus-selector start-from-beat)
  (:documentation "Returns a set of sample times to clap to given the supplied rhythm"))

(defgeneric tactus-for-rhythm (rhythm-to-analyse &key voices-per-octave tactus-selector)
  (:documentation "Returns the selected tactus for the rhythm."))

(defgeneric scaleogram-of-rhythm (rhythm-to-analyse &key voices-per-octave)
  (:documentation "Returns the scaleogram for the rhythm."))

(defgeneric skeleton-of-scaleogram (scaleogram sample-rate)
  (:documentation "Returns the skeleton given the scaleogram and sample-rate."))

(defgeneric skeleton-of-rhythm (rhythm-to-analyse &key voices-per-octave)
  (:documentation "Returns a skeleton (holds a list of ridges) for the given rhythm."))

(defgeneric skeleton-of-rhythm-cached (rhythm-to-analyse &key voices-per-octave cache-directory)
  (:documentation "Reads the skeleton, scaleogram and ridge-scale-peaks from disk if they have been cached,
otherwise, generates them and writes to disk."))

;;;; Implementation

;; Fastest scale for tactus 
;; (scale-from-period (* 200 0.25) 16)
;; Slowest scale for tactus
;; (scale-from-period (* 200 2.0) 16)
;; Slowest scale "accessible to the senses" (London 2004)
;; (scale-from-period (* 200 5.0) 16)

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
      (setf (.subarray tempo-weighting-over-time (list t time))
	    (.reshape tempo-scale-weighting (list number-of-scales 1))))
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

(defun determine-scale-peaks (correlated-profile &key (correlation-minimum 0.01))
  "Finds the peaks in the combined correlation profile 
  (of energy modulus, stationary phase and local phase congruency)
   across the dilation scale axis at each time point."
  (.* (.> correlated-profile correlation-minimum)
      (extrema-points correlated-profile :rows) 
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
	 (salient-scale (preferred-tempo scaleogram sample-rate))
	 (tempo-weighting (tempo-salience-weighting salient-scale (.array-dimensions magnitude)))
	 ;; Weight by the absolute tempo preference.
	 (tempo-weighted-ridges (.* correlated-ridges tempo-weighting)))
    (if absolute-tempo-weighting
	(progn 
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
  (format t "Length of Rhythm ~f seconds~%" (duration analysis-rhythm))
  (cwt (time-signal analysis-rhythm) voices-per-octave))

(defmethod skeleton-of-scaleogram ((analysis-scaleogram scaleogram) sample-rate)
  "Returns the skeleton given the scaleogram and sample-rate."
  (let* ((correlated-ridge-scale-peaks (scale-peaks-of-scaleogram analysis-scaleogram sample-rate))
       	 (skeleton (make-instance 'skeleton 
				  :ridges (extract-ridges correlated-ridge-scale-peaks)
				  :duration (duration-in-samples analysis-scaleogram)
				  :scales (number-of-scales analysis-scaleogram)
				  :voices-per-octave (voices-per-octave analysis-scaleogram)
				  :skip-highest-octaves (skip-highest-octaves analysis-scaleogram))))
    (values skeleton correlated-ridge-scale-peaks)))

(defmethod skeleton-of-rhythm ((analysis-rhythm rhythm) &key (voices-per-octave 16))
  "Returns the skeleton given the rhythm."
  (let* ((scaleogram (scaleogram-of-rhythm analysis-rhythm :voices-per-octave voices-per-octave)))
    (multiple-value-bind (skeleton correlated-ridge-scale-peaks) 
	(skeleton-of-scaleogram scaleogram (sample-rate analysis-rhythm))
    (values skeleton scaleogram correlated-ridge-scale-peaks))))

(defmethod tactus-for-rhythm ((analysis-rhythm rhythm) 
			      &key (voices-per-octave 16)
			      (tactus-selector #'select-longest-lowest-tactus))
  "Returns the selected tactus given the rhythm."
  (multiple-value-bind (skeleton scaleogram correlated-ridge-scale-peaks)
      (skeleton-of-rhythm analysis-rhythm :voices-per-octave voices-per-octave)
    (let* ((chosen-tactus (funcall tactus-selector skeleton))   ; select out the tactus from all ridge candidates.
	   (chosen-tactus-list (if (listp chosen-tactus) chosen-tactus (list chosen-tactus))))
      (format t "Computed skeleton and chosen tactus ~a~%" chosen-tactus-list)
      (plot-cwt+ridges scaleogram chosen-tactus-list analysis-rhythm
		       ;; :phase-palette :greyscale
		       ;; :magnitude-palette :jet
		       :title (name analysis-rhythm))
      (plot-highlighted-ridges scaleogram 
			       chosen-tactus-list
			       correlated-ridge-scale-peaks 
			       :title (name analysis-rhythm)
			       :sample-rate (sample-rate analysis-rhythm))
      (format t "Finished plotting scalograms~%")
      (values chosen-tactus-list scaleogram))))

;;; To verify the ridge extraction accuracy:
;;; (plot-highlighted-ridges-of-rhythm scaleogram (ridges skeleton) correlated-ridge-scale-peaks analysis-rhythm)
;;; No black ridges should be shown, only red highlighted ridges.

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
	 (empty-magnitude (make-double-array time-frequency-dimensions))
	 (tactus-mag (dolist (tactus-ridge (if (listp tactus) tactus (list tactus)) empty-magnitude)
			     (insert-ridge tactus-ridge empty-magnitude :constant-value 1d0)))

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
	 ;; (clap-rhythm (rhythm-of-onsets (name original-rhythm) clap-at))
	 )

    (format t "Handclapping from beat ~d of original rhythm, sample ~d~%" start-from-beat down-beat-sample)
    (plot-claps original-rhythm clap-at :foot-tap-AM foot-tap-phase)
    clap-at))

(defmethod clap-to-rhythm ((performed-rhythm rhythm) &key 
			   (start-from-beat 1)
			   (tactus-selector #'select-longest-lowest-tactus))
  "Returns a set of sample times to clap to given the supplied rhythm"
  (multiple-value-bind (computed-tactus rhythm-scaleogram)
      (tactus-for-rhythm performed-rhythm :tactus-selector tactus-selector)
    (clap-to-tactus-phase performed-rhythm rhythm-scaleogram computed-tactus :start-from-beat start-from-beat)))

(defun save-rhythm-and-claps (original-rhythm clap-at)
  "Writes out the rhythm and the handclaps to a scorefile"
  (save-scorefile (format nil "/Users/leigh/~a.handclap.score" (name original-rhythm)) 
		  (list (nlisp::array-to-list (onsets-in-seconds original-rhythm)) 
			(nlisp::array-to-list (./ clap-at (* (sample-rate original-rhythm) 1d0))))
		  :instrument "midi"
		  :midi-channel 10
		  :key-numbers (list *low-woodblock* *closed-hi-hat*)
		  :description (format nil "Handclapping to ~a" (description original-rhythm))))

;;; File I/O

(defmethod save-mra-to-file ((skeleton-to-write skeleton) 
			 (scaleogram-to-write scaleogram)
			 ridge-peaks-to-write
			 (path-to-write pathname))
  (let* ((skeleton-filename (make-pathname :defaults path-to-write :type "skeleton"))
	 (scaleogram-filename (make-pathname :defaults path-to-write :type "scaleogram"))
	 (ridge-peaks-filename (make-pathname :defaults path-to-write :type "peaks")))
      (format t "Writing ~a~%" skeleton-filename) 
      (save-to-file skeleton-to-write skeleton-filename)
      (format t "Writing ~a~%" scaleogram-filename) 
      (save-to-file scaleogram-to-write scaleogram-filename)
      (format t "Writing ~a~%" ridge-peaks-filename)
      (.save-to-octave-file ridge-peaks-to-write ridge-peaks-filename)))

(defmethod read-mra-from-file ((path-to-read pathname))
  "Reads and returns skeleton and scaleogram instances, returns nil when EOF"
  (values (read-skeleton-from-file (make-pathname :defaults path-to-read :type "skeleton"))
	  (read-scaleogram-from-file (make-pathname :defaults path-to-read :type "scaleogram"))
	  (.load-octave-file (make-pathname :defaults path-to-read :type "peaks"))))

(defmethod skeleton-of-rhythm-cached ((analysis-rhythm rhythm) &key (voices-per-octave 16)
				      (cache-directory *mra-cache-path*))
  "Reads the skeleton, scaleogram and ridge-scale-peaks from disk if they have been cached,
otherwise, generates them and writes to disk."
  (let ((cache-root-pathname (make-pathname :directory cache-directory :name (name analysis-rhythm))))
    (if (probe-file (make-pathname :defaults cache-root-pathname :type "skeleton"))
	(read-mra-from-file cache-root-pathname)
	(let ((mra (multiple-value-list (skeleton-of-rhythm analysis-rhythm :voices-per-octave voices-per-octave))))
	  (apply #'save-mra-to-file (append mra (list cache-root-pathname)))
	  (values-list mra)))))
