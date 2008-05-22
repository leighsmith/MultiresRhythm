;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for generating expectation points from a given set of event times.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2007
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

;;; Interface

(defclass expectation ()
  ((expected-time     :initarg :time        :accessor expected-time :initform 0)
   (confidence        :initarg :confidence  :accessor confidence)
   (precision         :initarg :precision   :accessor precision)
   (expected-features :initarg :features    :accessor expected-features :initform '())
   (sample-rate       :initarg :sample-rate :accessor sample-rate)) ; not yet used.
  (:documentation "Defines the expectation to the next point in time."))

(defgeneric time-in-seconds (expectation sample-rate)
  (:documentation "Returns the expected time in seconds, given the sample rate."))

(defgeneric list-in-seconds (expectation sample-rate)
 (:documentation "Returns the expectation as a list, times in seconds, given the sample rate."))

;;; Implementation

(defmethod print-object ((expectation-to-print expectation) stream)
  (call-next-method expectation-to-print stream) ;; print the superclass
  (format stream " expected time: ~,3f confidence: ~,3f precision: ~,3f" 
	  (expected-time expectation-to-print) 
	  (confidence expectation-to-print)
	  (precision expectation-to-print)))

(defun print-expectations (all-expectations)
  "Print the entire list of expectations"
  (dolist (expectation-from-time all-expectations)
    (dolist (expectation (second expectation-from-time))
      (format t "time ~a projection ~8,3f ~a~%"
	      (first expectation-from-time) 
	      (- (expected-time expectation) (first expectation-from-time))
	      expectation))))

(defmethod time-in-seconds ((expectation-to-report expectation) sample-rate)
  (/ (expected-time expectation-to-report) (float sample-rate)))

;;; TODO obsolete.
(defmethod list-in-seconds ((expectation-to-list expectation) sample-rate)
  "Returns the expectancy as a list, times in seconds"
  (list (time-in-seconds expectation-to-list sample-rate) 
	(confidence expectation-to-list)
	(precision expectation-to-list)))

(defun width-of-peaks (peaks minima number-of-scales)
  "Returns the widths of the peaks, by finding the distance between two minima either side of each peak"
  (let* ((minima-val (val minima))
	 (minima-length (length minima-val))
	 (minima-widths (make-integer-array (.array-dimensions peaks)))
	 (last-minima (if (zerop minima-length) 0 (.aref minima (1- minima-length))))
	 (minima-diffs (.diff minima)))
    (loop
       for peak across (val peaks)
       for peak-index = 0 then (1+ peak-index)
       for right-position = (position peak minima-val :test #'<=)
       do (setf (.aref minima-widths peak-index) 
		(cond ((null right-position)  ; peak exceeds last minima.
		       (- number-of-scales last-minima 1)) ; difference is between last minima and the last scale.
		      ((zerop right-position) ; peak preceded first minima.
		       (.aref minima 0))      ; the first element is the difference.
		      (t
		       (.aref minima-diffs (1- right-position))))))
    minima-widths))

;;; Precision = sharpness of peak. We calculate this as the area under the peak
;;; between the other peaks. Do this by calculating the distance from the ridge
;;; scale to the two nearest minima. The more narrow the width, the higher the precision.
(defun precisions-at-time (analysis time)
  "Returns the precision of each ridge at the given time"
  (let* ((maxima (ridge-peaks analysis))
	 (minima (ridge-troughs analysis))
	 (widths (width-of-peaks (.find (.column maxima time)) (.find (.column minima time)) (.row-count maxima))))
    ;; Determine the widths in relative measures of scale span and from the inverse of
    ;; those, the precision.
    (.- 1d0 (./ widths (* (.row-count maxima) 1d0)))))

;;; This returns the normalized maximum value for each of the peaks. We could calculate
;;; this by (./ normalised-precision (.partial-sum normalised-precision)) to choose from
;;; the most precise peaks (and ignoring those regions that are not a peak), but this is
;;; more efficient.
(defun most-precise (maxima minima)
  "Returns a matrix of the relative measure of precision of each peak, given the maxima and minima of the scaleogram"
  (let* ((time-freq-dimensions (.array-dimensions maxima))
	 (number-of-scales (.row-count maxima))
	 (duration (.column-count maxima))
	 (precision (make-double-array time-freq-dimensions)))
    (dotimes (time duration)
      (let* ((ridge-peak-profile (.column maxima time))
	     (peak-scales (.find ridge-peak-profile))
	     (peak-widths (width-of-peaks peak-scales (.find (.column minima time)) number-of-scales))
	     ;; Determine the widths in relative measures of scale span and from the inverse of
	     ;; those, the precision.
	     (normalized-precision (.- 1d0 (./ peak-widths (* 1d0 (.sum peak-widths)))))
	     (precision-profile (make-double-array number-of-scales)))
	(setf (.arefs precision-profile peak-scales) normalized-precision)
	(setf (.column precision time) precision-profile)))
    precision))

(defun normalized-precision-profile (ridge-peak-profile ridge-trough-profile)
  "Determine the precision given the peak and troughs expressed as vectors"
  (diag-plot 'precision-profiles
    (nplot (list ridge-peak-profile ridge-trough-profile) nil :styles '("impulses lw 2" "impulses lw 2")))
  (let* ((number-of-scales (.length ridge-peak-profile))
	 (peak-scales (.find ridge-peak-profile))
	 (peak-widths (width-of-peaks peak-scales (.find ridge-trough-profile) number-of-scales))
	 ;; Determine the widths in relative measures of scale span and from the inverse of
	 ;; those, the precision.
	 (normalized-precision (./ peak-widths (coerce number-of-scales 'double-float)))
	 (precision-profile (make-double-array number-of-scales)))
    (setf (.arefs precision-profile peak-scales) normalized-precision)
    precision-profile))

(defun normalized-precision (maxima minima)
  "Returns a matrix of the relative measure of precision of each peak, with respect to the
  total number of scales, given the maxima and minima of the scaleogram."
  (let* ((time-freq-dimensions (.array-dimensions maxima))
	 (precision (make-double-array time-freq-dimensions)))
    (dotimes (time (.column-count maxima)) ; loop over the duration
      ;; (format t "precision of ~a~%" time)
      (setf (.column precision time) 
	    (normalized-precision-profile (.column maxima time) (.column minima time))))
    precision))

(defun confidence-and-precision (analysis)
  "Returns the most likely which is the most confident and most precise"
  (let ((peaks (ridge-peaks analysis)))
    (.* peaks (most-precise peaks (ridge-troughs analysis)))))

(defun phase-diff-from-start (phase scale time)
  (format t "phase at time ~a phase at 0 ~a~%" (.aref phase scale time) (.aref phase scale 0))
  (.aref (phase-diff (make-narray (list (.aref phase scale time) (.aref phase scale 0)))) 0))

(defun normalized-phase (phi) 
  "Return a value between 0 and 1 for a phase value from 0 -> pi, -pi -> 0"
  (let ((pi2 (* pi 2)))
    (/ (+ phi (if (minusp phi) pi2 0)) pi2)))

(defun phase-corrected-time-from (time-projection time phase-correct-from)
  "Compute the time prediction, modified by the phase from a given moment."
  (if (null phase-correct-from)
      (+ time time-projection)
      ;; Calculate the number of periods of projection within the gap between phase-correct-from & the
      ;; moment of expectancy projection (time).
      (multiple-value-bind (periods phase-offset)
	  (floor (- time phase-correct-from) time-projection)
	(format t "From: time projection ~,3f phase correct from ~a periods ~a phase-offset ~a~%" 
		time-projection phase-correct-from periods phase-offset)
	(cond ((> periods 1)		; Multiple projections within the gap, implies the
	       (+ time time-projection)) ; projection should apply from the analysis time.
	      ((= periods 1)		; A single projection fits within the gap implies
	       (+ time time-projection (- phase-offset))) ; the projection should be phase corrected.
	      ((zerop periods)		; The projection exceeds the gap implies it originates
	       (+ phase-correct-from time-projection)))))) ; from phase-correct-from (assumed the last onset).

;;; Phase goes 0 -> pi, -pi -> 0.
(defun phase-corrected-time-computed (time-projection phase scale time)
  "Compute the correction using the assumption the phase value progresses regularly
   through the period. This doesn't happen in practice!"
  (let* ((norm-phase (normalized-phase (.aref phase scale time)))
	 (phase-offset (* time-projection norm-phase))
	 (zero-index (round (- time phase-offset))))
    (format t "Computed: time projection ~,3f phase zero index ~d norm-phase ~,3f phase-offset ~,3f~%" 
	    time-projection zero-index norm-phase phase-offset)
    (format t "value at expected phase zero index, & either side (~,3f ~,3f ~,3f)~%"
	    (.aref phase scale (1- zero-index))
	    (.aref phase scale zero-index)
	    (if (= zero-index time) 0 (.aref phase scale (1+ zero-index))))
    (+ time (* time-projection (- 1.0d0 norm-phase)))))

;; TODO phase change from start of time
;; (phase-corrected-time (+ time (* (time-support scale vpo) 
;;			  (- 1.0d0 (normalized-phase (phase-diff-from-start phase scale time))))))
;;    (format t "time projection ~,5f phase weighting ~,5f time ~a expected-time ~,3f weighted ~,3f unweighted ~,3f~%"
;; (time-support scale vpo) (- 1.0d0 (normalized-phase phase-of-ridge)) time 
;;	(time-support scale vpo) (- 1.0d0 (normalized-phase (phase-diff-from-start phase scale time))) time 
;;	expected-time phase-corrected-time uncorrected-time)
;;    (format t "phase-diff ~a~%" (phase-diff-from-start phase scale time))

(defun phase-zero-samples (phase scale)
  "Returns the location of each phase 0 crossing index for a given scale"
  (let ((phase-at-scale (.row phase scale))
	(time-length (.array-dimension phase 1)))
    (.find (.and (.subarray (.>= phase-at-scale 0) (list 0 (list 1 (1- time-length))))
		 (.subarray (.< phase-at-scale 0) (list 0 (list 0 (- time-length 2))))))))
	
(defun phase-corrected-time-chasing (time-projection phase scale time)
  "Returns the time-projection corrected by it's phase measure based on the most recent occurrance of zero phase. 
   Chases for phase-zero values backwards from time."
  (loop
     for zero-index from time downto 1
     until (and (not (minusp (.aref phase scale zero-index)))
		(minusp (.aref phase scale (1- zero-index))))
     finally (return 
	       (progn (format t "Chasing: time projection ~,3f phase zero index ~d computed phase offset ~,3f~%" 
			      time-projection zero-index (- time zero-index))
		      (if (< (/ (- time zero-index) time-projection) 0.8)  ; check we did not wind back too far.
			  (+ zero-index time-projection)
			  (+ time time-projection))))))

(defun expectancy-of-ridge-at-time (ridge time scaleogram all-precision all-peaks 
				    &key (time-limit-expectancies nil) (phase-correct-from nil))
  "Return an expectation instance holding the expection time, the confidence and the precision" 
  (let* ((vpo (voices-per-octave scaleogram))
	 (max-time (if time-limit-expectancies (duration-in-samples scaleogram) most-positive-fixnum))
	 (scale (scale-at-time ridge time))
	 (phase (scaleogram-phase scaleogram))
	 (uncorrected-time (+ time (time-support scale vpo)))
	 ;; Compute the time prediction, modified by the phase.
	 (phase-corrected-time (phase-corrected-time-chasing (time-support scale vpo) phase scale time))
	 ;; (phase-corrected-time (phase-corrected-time-computed (time-support scale vpo) phase scale time))
	 ;; (phase-corrected-time (phase-corrected-time-from (time-support scale vpo) time phase-correct-from))
	 ;; Add 1 for projection from the next sample.
	 (expected-time (1+ (if phase-correct-from phase-corrected-time uncorrected-time)))
	 ;; energy = absolute height of scaleogram or ridge-peaks (depending on what's
	 ;; passed into all-peaks)
	 (energy (.aref all-peaks scale time)))
    (format t "time ~a time projection ~,3f phase-corrected ~,3f uncorrected ~,3f expected-time ~,3f~%"
	    time (time-support scale vpo) phase-corrected-time uncorrected-time expected-time)
    (make-instance 'expectation 
		   :time (min expected-time max-time)
		   ;; :sample-rate (sample-rate ?) TODO sample rate of?
		   :confidence energy
		   :precision (.aref all-precision scale time))))

(defun expectancies-of-skeleton-at-times (skeleton times scaleogram precision ridge-peaks &key
					  (cutoff-scale 16) (phase-correct-from nil))
  "Returns the expectancies determined from the skeleton, scaleogram and precision at the indicated times"
  (loop
     for time in times
     do (format t "Expectancies at time ~a:~%" time)
     collect (list time
		   (loop
		      for ridge in (ridges-at-time skeleton time)
		      ;; filter out the ridges higher than a cut off point determined by preferred tempo rate.
		      when (> (scale-at-time ridge time) cutoff-scale)
		      collect (expectancy-of-ridge-at-time ridge time scaleogram precision ridge-peaks
							   :phase-correct-from phase-correct-from
							   :time-limit-expectancies nil)))))

(defun cumulative-scaleogram (scaleogram)
  "Returns the cumulative sum of the scaleogram energy as another scaleogram" 
  (let* ((cumulative-scale-persistency (./ (cumsum (scaleogram-magnitude scaleogram))
					   (duration-in-samples scaleogram))))
    (make-instance 'scaleogram 
		   :magnitude cumulative-scale-persistency
		   :phase (scaleogram-phase scaleogram)
		   :voices-per-octave (voices-per-octave scaleogram))))

#|

(defun expectancies-of-rhythm-integrator (rhythm-to-analyse 
					  &key (times-to-check (nlisp::array-to-list (onsets-in-samples rhythm-to-analyse)))
					  (phase-correct-from nil))
  "Return a list structure of expectancies. Computes the expectancies using the maximum
   value of the cumulative sum of the scaleogram energy" 
  (let* ((analysis (analysis-of-rhythm rhythm-to-analyse :padding #'causal-pad))
	 (scaleogram (scaleogram analysis))
	 (cumulative-scale-persistency (cumsum (scaleogram-magnitude scaleogram)))
	 (vpo (voices-per-octave scaleogram))
	 (sample-rate (sample-rate rhythm-to-analyse))
	 ;; Use Parncutt's tempo salience peak measure at 720mS
	 (salient-scale (preferred-tempo-scale vpo sample-rate :tempo-salience-peak 0.72))
;;	 (tempo-beat-preference (tempo-salience-weighting salient-scale 
;;							  (.array-dimensions cumulative-scale-persistency)
;;							  :octaves-per-stddev 1.0))
	 (tempo-beat-preference (tempo-salience-weighting-log salient-scale (.array-dimensions cumulative-scale-persistency)))
 	 (weighted-persistency-profile (./ (.* cumulative-scale-persistency tempo-beat-preference)
					   (duration-in-samples rhythm-to-analyse)))
	 (weighted-scale-peaks (determine-scale-peaks weighted-persistency-profile))
	 (weighted-scale-troughs (extrema-points weighted-persistency-profile :extrema :min))
	 (weighted-precision (normalized-precision weighted-scale-peaks weighted-scale-troughs))
       	 (weighted-skeleton (skeleton-of-ridge-peaks scaleogram weighted-scale-peaks))
	 (weighted-scaleogram (make-instance 'scaleogram 
					     :magnitude weighted-persistency-profile
					     :phase (scaleogram-phase scaleogram)
					     :voices-per-octave (voices-per-octave scaleogram))))
    (expectancies-of-skeleton-at-times weighted-skeleton
				       times-to-check
				       weighted-scaleogram
				       weighted-precision
				       :phase-correct-from phase-correct-from)))
|#

(defun expectancies-of-rhythm-integrator (rhythm-to-analyse 
					  &key (times-to-check (nlisp::array-to-list (onsets-in-samples rhythm-to-analyse)))
					  (phase-correct-from nil))
  "Return a list structure of expectancies. Computes the expectancies using the maximum
   value of the cumulative sum of the scaleogram energy" 
  (let* ((analysis (analysis-of-rhythm rhythm-to-analyse :padding #'causal-pad))
	 (scaleogram (scaleogram analysis))
 	 (cumulative-persistency (./ (cumsum (scaleogram-magnitude scaleogram)) (duration-in-samples rhythm-to-analyse)))
	 (vpo (voices-per-octave scaleogram))
	 (sample-rate (sample-rate rhythm-to-analyse))
	 ;; Use Parncutt's tempo salience peak measure at 720mS
	 (salient-scale (preferred-tempo-scale vpo sample-rate :tempo-salience-peak 0.72))
	 ;; (tempo-beat-preference (tempo-salience-weighting-log salient-scale (.array-dimensions cumulative-persistency)))
	 (tempo-beat-preference (tempo-salience-weighting salient-scale (.array-dimensions cumulative-persistency)))
	 (scale-peaks (determine-scale-peaks cumulative-persistency))
	 (scale-troughs (extrema-points cumulative-persistency :extrema :min))
	 (weighted-scale-peaks (.* tempo-beat-preference scale-peaks))
	 (precision (normalized-precision scale-peaks scale-troughs))
       	 (weighted-skeleton (skeleton-of-ridge-peaks scaleogram weighted-scale-peaks))
	 (cumulative-scaleogram (make-instance 'scaleogram 
					       :magnitude cumulative-persistency
					       :phase (scaleogram-phase scaleogram)
					       :voices-per-octave (voices-per-octave scaleogram))))
#|
    (diag-plot 'cumulative-persistency
      (plot-image #'magnitude-image (list cumulative-persistency) '((1.0 0.5) (0.0 0.3))
		  (axes-labelled-in-seconds scaleogram sample-rate 4)
		  :title (format nil "cumulative persistency profile of ~a" (name rhythm-to-analyse))))
    (diag-plot 'tempo-preference
      (plot-command "set xtics (~{~{\"~d\" ~5d~}~^, ~})~%" (label-scale-as-time-support scaleogram))
      (nplot (list (.reverse (.* (.column tempo-beat-preference (first (last times-to-check)))
				 (.max cumulative-persistency)))
		   (.reverse (.column cumulative-persistency (first (last times-to-check)))))
	     nil 
	     :aspect-ratio 0.2 
	     :reset nil))
     (diag-plot 'unweighted-profile 
      (plot-scale-energy+peaks-at-time cumulative-scaleogram
				       (first (last times-to-check))
				       (.normalise scale-peaks)))
    (diag-plot 'scale-energy-profile
      (plot-scale-energy+peaks-at-time cumulative-scaleogram
				       (first (last times-to-check))
				       (.normalise weighted-scale-peaks)))
				       ;; :sample-rate (sample-rate rhythm-to-analyse)))
|#
    (expectancies-of-skeleton-at-times weighted-skeleton
				       times-to-check
				       cumulative-scaleogram
				       precision
				       weighted-scale-peaks
				       :phase-correct-from phase-correct-from)))


;; (expectancies-of-rhythm-integrator rhythm-to-analyse :times-to-check (list (1- (duration-in-samples rhythm-to-analyse))))

(defun most-persistent-scales (ridge-persistency)
  "Return those scales sorted in order of most persistent"
  ;; TODO Need to look under the area of the curve to determine the peaks which are worth identifying.
  (let* ((prominent-scales (.find (extrema-points-vector ridge-persistency :extrema :max))))
    ;; TODO should we use +1? Because the indexes for ridge-persistency are zero?
    (make-instance (class-of prominent-scales) 
		   :ival (sort (val prominent-scales) #'> :key (lambda (x) (.aref ridge-persistency x))))))

(defun tempo-weighted-ridge-persistency-of (analysis)
  "Returns the ridge persistency of the analysis, weighted by the absolute tempo sensitivity"
  (let* ((ridge-persistency (unweighted-ridge-persistency-of analysis))
	 (scaleogram (scaleogram analysis))
	 (vpo (voices-per-octave scaleogram))
	 ;; Use Parncutt's tempo salience peak measure at 720mS 
	 (salient-tempo-scale (preferred-tempo-scale vpo (sample-rate analysis) :tempo-salience-peak 0.72))
	 ;; Cut off freq above 3 stddev's from preferred-scale = 3 octaves.
	 (cutoff-scale (- salient-tempo-scale (* 3 vpo)))
	 ;; Tuned filter that gives the future projections more space.
	 (tempo-beat-preference (tempo-salience-weighting-vector salient-tempo-scale 
								 (.length ridge-persistency)
	  							 :envelope #'skewed-gaussian-envelope
								 :octaves-per-stddev 2.0)))
	 ;; (tempo-beat-preference (tempo-salience-weighting-vector salient-tempo-scale (.length ridge-persistency)))
	 ;; no tempo weighting
	 ;; (tempo-beat-preference (make-double-array (.length ridge-persistency) :initial-element 1d0))
    (diag-plot 'tempo-beat-preference
      (plot-tempo-preference tempo-beat-preference scaleogram (sample-rate analysis)))
    (diag-plot 'ridge-persistency
      (plot-ridge-persistency ridge-persistency scaleogram "(name rhythm-to-analyse)"))
    (values (.* tempo-beat-preference ridge-persistency) cutoff-scale)))

(defun most-likely-scales (ridge-persistency &key
			   ;; max. # of simultaneous expectations listeners can possibly hold (cf Miller 1956).
			   (memory-limit 7) 
			   ;; 0.5 standard deviation above the mean is our definition of worthwhile.
			   (worthwhile 0.5d0))
  "Returns a list of scales given the ridge persistency which are most likely to be expectation periods"
  (let* ((most-likely-scales (most-persistent-scales ridge-persistency))
	 (peak-persistencies (.arefs ridge-persistency most-likely-scales))
	 (above-average-peaks (+ (mean peak-persistencies) (* worthwhile (stddev peak-persistencies)))) 
	 ;; threshold the scales used to only those above half the most persistent.
	 ;; check (> scale cutoff-scale) ; TODO unneeded with weighting?
	 (number-likely (min (position above-average-peaks (val peak-persistencies) :test #'>=) memory-limit)))
    (format t "above-average peaks threshold ~a~%" above-average-peaks)
    (.subarray most-likely-scales (list 0 (list 0 (1- number-likely))))))

;;; TODO Surely there must exist something similar already?
(defun impulses-at (peak-scales total-number-of-scales)
  (let ((new-vector (make-double-array total-number-of-scales)))
    (setf (.arefs new-vector peak-scales) 
	  (make-double-array (.length peak-scales) :initial-element 1d0))))

(defun expectancies-of-rhythm-ridge-persistency (rhythm-to-analyse 
						 &key (times-to-check (list (1- (duration-in-samples rhythm-to-analyse))))
						 (phase-correct-from nil))
  "Return a list structure of expectancies. Computes the expectancies using the maximum
   value of the ridge persistency" 
  ;; Allows sldb to find our problems quicker.
  (declare (optimize (debug 3)) (ignore phase-correct-from))
  (let* ((analysis (analysis-of-rhythm rhythm-to-analyse :padding #'causal-pad))
	 (tempo-weighted-ridge-persistency (tempo-weighted-ridge-persistency-of analysis))
	 (most-likely-scales (most-likely-scales tempo-weighted-ridge-persistency))
	 ;; Make a new peak profile of just the most likely scales for the precision calculation.
	 (likely-persistency-peaks (impulses-at most-likely-scales (.length tempo-weighted-ridge-persistency)))
	 ;; Since ridge-persistency produces many zero values, and extrema-points-vector won't
	 ;; catch corners, only minima, including locations of zeros tightens the precision estimation.
	 (persistency-troughs (.or (extrema-points-vector tempo-weighted-ridge-persistency :extrema :min) 
				   (.not tempo-weighted-ridge-persistency))) 
	 (precision (normalized-precision-profile likely-persistency-peaks persistency-troughs))
	 (time (first times-to-check)))
    (diag-plot 'tempo-ridge-persistency
      (plot-ridge-persistency tempo-weighted-ridge-persistency (scaleogram analysis) (name rhythm-to-analyse)))
    (list ; over times
     (list time
	   (loop
	      with vpo = (voices-per-octave (scaleogram analysis))
	      for scale across (val most-likely-scales)
	      ;; TODO replace with a modified version of expectancy-of-ridge-at-time including phase correction.
	      collect (make-instance 'expectation 
				     ;; +1 for projection beyond end of rhythm
				     :time (1+ (+ time (time-support scale vpo)))
				     :sample-rate (sample-rate rhythm-to-analyse)
				     :confidence (.aref tempo-weighted-ridge-persistency scale)
				     :precision (.aref precision scale)))))))

(defun expectancies-of-rhythm (rhythm &key (times-to-check (nlisp::array-to-list (onsets-in-samples rhythm)))
			       (time-limit-expectancies nil) (phase-correct-from nil))
  "Return a list structure of expectancies. If limit-expectancies is true, only calculate
  expectancies for events within the period analysed. If false, forward expectancies are generated
  for the last event."
  (let* ((rhythm-analysis (analysis-of-rhythm rhythm :padding #'causal-pad))
	 (rhythm-scaleogram (scaleogram rhythm-analysis))
	 ;; The skeleton has already picked scale peaks at each time.
	 (skeleton (skeleton rhythm-analysis))
	 (precision (normalized-precision (ridge-peaks rhythm-analysis) (ridge-troughs rhythm-analysis)))
	 (preferred-tempo-scale (preferred-tempo-scale (voices-per-octave rhythm-scaleogram) (sample-rate rhythm)))
	 ;; Cut off freq above 3 stddev's from preferred-scale = 3 octaves.
	 (cutoff-scale (- preferred-tempo-scale (* 3 (voices-per-octave rhythm-scaleogram)))))
    (if time-limit-expectancies 
	(setf times-to-check (butlast times-to-check)))
    (diag-plot 'scale-energy-profile
      (plot-scale-energy+peaks-at-time rhythm-scaleogram 
				       (first (last times-to-check))
				       (ridge-peaks rhythm-analysis)))
    ;; (format t "times to check ~a~%" times-to-check)
    (format t "cutoff scale ~a period ~a~%" 
	    cutoff-scale (time-support cutoff-scale (voices-per-octave rhythm-scaleogram)))
    (expectancies-of-skeleton-at-times skeleton times-to-check rhythm-scaleogram 
				       precision (scaleogram-magnitude rhythm-scaleogram)
				       :cutoff-scale cutoff-scale :phase-correct-from phase-correct-from)))

;;; Accumulation of onset expectations.
(defun onsets-of-expectations (all-expectations)
  "Given a set of expectations, sum the confidences (when overlapping), returns an array spanning the projection time"
  (let* ((all-expect-times (make-narray (mapcar (lambda (e) (expected-time e)) all-expectations)))
	 (maximum-expect-time (ceiling (.max all-expect-times)))
	 (projection-confidences (make-double-array maximum-expect-time)))
    (dolist (expectation all-expectations)
      (incf (.aref projection-confidences (floor (expected-time expectation))) (confidence expectation)))
    projection-confidences))

(defun last-expectations-project-onsets (rhythm-to-expect)
  "Determine the expectations by summing the expectations at each onset"
  (let* ((all-expectations-of-rhythm (expectancies-of-rhythm rhythm-to-expect :phase-correct-from nil :time-limit-expectancies nil))
	 ;; rearrange all the expectations into a single list.
	 (all-expectations (reduce #'append (mapcar #'second all-expectations-of-rhythm)))
	 (expectancy-confidences (onsets-of-expectations all-expectations))
	 (projections-into-future (.subarray expectancy-confidences 
					     (list 0 (list
						      (duration-in-samples rhythm-to-expect)
						      (1- (.length expectancy-confidences)))))))
    (let ((padded-rhythm-signal (.concatenate (time-signal rhythm-to-expect)
					      (make-double-array (- (.length expectancy-confidences)
								    (duration-in-samples rhythm-to-expect))))))
      (nplot (list expectancy-confidences (.- 0 padded-rhythm-signal))
	     nil
	     :styles '("impulses" "impulses") :aspect-ratio 0.66))
    projections-into-future))

;;; last-expectations, Charles Dickens undiscovered work from his secret life as a Lisp programmer...
;;; This is needed to throw away the time and break the last expectation out of the list.
(defun last-expectations (rhythm-to-expect 
			  &key (last-time (1- (duration-in-samples rhythm-to-expect)))
			  (expectancies-generator #'expectancies-of-rhythm-integrator))
  "Return the list of expectations for the last moment in the rhythm"
  (let ((expectations (funcall expectancies-generator rhythm-to-expect 
			       :times-to-check (list last-time)
			       ;; Do no phase correction when testing metrical expectancies.
			       ;; :phase-correct-from nil))))
			       :phase-correct-from (last-onset-time rhythm-to-expect))))
    (print-expectations expectations)
    (second (first expectations))))
  
;;; This is needed to throw away the time and break the last expectation out of the list.
(defun last-onset-expectations (rhythm-to-expect)
  "Return the list of expectations for the last onset in the rhythm"
  (let ((last-time (last-onset-time rhythm-to-expect))) ; (1- (duration-in-samples rhythm-to-expect))
    (second (first (expectancies-of-rhythm rhythm-to-expect :times-to-check (list last-time))))))

;;;;
;;;; Expectancy File I/O
;;;;

;;; TODO this should produce XML
(defmethod exchange-format ((expectation-to-exchange expectation) sample-rate)
  "Returns the expectancy in the EmCAP interchange output format."
  (format nil "~,5f ~,5f ~,5f;~:{~,5f,~,5f~:^ ~}" 
	  (time-in-seconds expectation-to-exchange sample-rate) 
	  (confidence expectation-to-exchange)
	  (precision expectation-to-exchange)
	  (expected-features expectation-to-exchange)))

(defun write-expectancies-to-stream (expectancies sample-rate stream)
  "Write the expectancies to a stream with the labelling the MTG code needs."
  (format stream "<EXPECTANCIES>~%") ; Write enclosing tags.
  (loop 
     for expectancy in expectancies
     for event-index = 0 then (1+ event-index)
     do (format stream "<EXPECT ID=\"~,5d\" TIME=\"~,5f\">~%~{~a~%~}</EXPECT>~%" 
		event-index
		(/ (first expectancy) (float sample-rate)) ; write the time of the expectation
		(mapcar (lambda (expectation) (exchange-format expectation sample-rate))
			(second expectancy)))
     finally (format stream "</EXPECTANCIES>~%")))

(defun split-string (string-to-split split-char)
  "Return a list of strings split by each occurrance of split-char"
  (loop 
     for search-from = 0 then (1+ separator-position)
     for separator-position = (position split-char string-to-split :start search-from) ; returns nil on end
     collect (subseq string-to-split search-from separator-position)
     while separator-position))

;;; Format is:
;;; ONSET-TIME PHENOMENAL-ACCENT ONSET-TIME-VAR;FEAT1-VAL,FEAT1-VAR FEAT2-VAL,FEAT2-VAR ... FEATn-VAL,FEATn-VAR
;;; Because this format is so full of useless context sensitive formatting noise, we strip
;;; all the crap away to get it to a form that Lisp can devour simply.
(defun strip-emcap-formatting (emcap-data-line)
  (destructuring-bind (onset features) (split-string emcap-data-line #\;)
    (list (mapcar #'read-from-string (split-string onset #\Space))
	  (mapcar (lambda (feature-string) (mapcar #'read-from-string (split-string feature-string #\,))) 
		  (if (zerop (length features)) nil (split-string features #\Space))))))

(defun summarise-features-to-accent (features)
  (reduce #'+ (mapcar #'car features)))

;;; TODO a good candidate to change to XML parsing.
(defun weighted-onsets-from-emcap-stream (stream)
  (loop
     for emcap-data-record = (read-line stream nil)
     while emcap-data-record
     collect (destructuring-bind (onset-time features) (strip-emcap-formatting emcap-data-record)
	       (declare (ignore features))
	       (list (first onset-time) (second onset-time)))))

(defun read-emcap-onsets-from-stream (stream name sample-rate)
  "Reads the EmCAP onsets format, returning a rhythm instance"
  (rhythm-of-weighted-onsets name (weighted-onsets-from-emcap-stream stream) :sample-rate sample-rate))

(defun rhythm-of-plain-onset-file (input-filepath sample-rate)
  (let* ((events (.load input-filepath :format :text))
	 (times-in-seconds (.column events 0)))
    (rhythm-of-onsets (pathname-name input-filepath) times-in-seconds :sample-rate sample-rate)))

(defun rhythm-of-emcap-onset-file (input-filepath sample-rate)
  (with-open-file (emcap-data-stream input-filepath)
    (read-emcap-onsets-from-stream emcap-data-stream (pathname-name input-filepath) sample-rate)))

(defun expectancies-at-times-in-file (input-filepath output-filepath &key (sample-rate 200.0d0))
  "Computes the expectancies at each onset in the file from the discrete onset times"
  (let* ((times-as-rhythm (rhythm-of-emcap-onset-file input-filepath sample-rate))
	 (expectancies-at-times (expectancies-of-rhythm times-as-rhythm :time-limit-expectancies nil)))
    (with-open-file (expectancy-file output-filepath :direction :output :if-exists :supersede)
      (write-expectancies-to-stream expectancies-at-times sample-rate expectancy-file))))

(defun last-expectancy-of-file (input-filepath output-filepath &key (sample-rate 200.0d0))
  "Computes the expectancies at the last moment in the file from the discrete onset times"
  (let* ((times-as-rhythm (rhythm-of-emcap-onset-file input-filepath sample-rate))
	 ;; (last-time (last-onset-time times-as-rhythm))  ; take either last onset
	 (last-time (1- (duration-in-samples times-as-rhythm)))	; or last moment of window.
	 (expectancies-at-last-time (expectancies-of-rhythm-integrator times-as-rhythm 
								       :times-to-check (list last-time))))
    (with-open-file (expectancy-file output-filepath :direction :output :if-exists :supersede)
      (write-expectancies-to-stream expectancies-at-last-time sample-rate expectancy-file))))

(defun last-expectancy-of-salience (saliency-filepath onset-times-filepath output-filepath
				    &key (sample-rate 200.0d0)) 
  "Computes the expectancies at the last moment in the file from the perceptual salience trace"
  (let* ((times-as-rhythm (perceptual-salience-rhythm saliency-filepath onset-times-filepath :sample-rate sample-rate))
	 (expectancies-at-times (expectancies-of-rhythm times-as-rhythm)))
    (with-open-file (expectancy-file output-filepath :direction :output :if-exists :supersede)
      (write-expectancies-to-stream (last expectancies-at-times) sample-rate expectancy-file))))
