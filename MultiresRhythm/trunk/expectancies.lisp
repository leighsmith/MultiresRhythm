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
   (period            :initarg :period      :accessor period)
   (confidence        :initarg :confidence  :accessor confidence)
   (precision         :initarg :precision   :accessor precision)
   (expected-features :initarg :features    :accessor expected-features :initform '())
   (sample-rate       :initarg :sample-rate :accessor sample-rate)) ; not yet used.
  (:documentation "Defines the expectation to the next point in time."))

(defgeneric expected-time-seconds (expectation)
  (:documentation "Returns the expected time in seconds, given the sample rate."))

(defgeneric precision-in-seconds (expectation)
  (:documentation "Returns the precision time in seconds, given the sample rate."))

(defgeneric absolute-duration-confidence (expectation rhythm)
  (:documentation "Scales the confidence of the expectation by the absolute duration of the rhythm"))

(defgeneric offset-expectation (expectation by-samples)
  (:documentation "Offsets the expected time by a given amount in samples, can be negative"))

;;; Implementation

(defmethod print-object ((expectation-to-print expectation) stream)
  (call-next-method expectation-to-print stream) ;; print the superclass
  (format stream " expected time: ~,3f projection: ~,3f confidence: ~,3f precision: ~,3f" 
	  (expected-time expectation-to-print)
	  (period expectation-to-print)
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

(defmethod expected-time-seconds ((expectation-to-list expectation))
  "Returns the expected time in seconds given the sample rate"
  (/ (expected-time expectation-to-list) (float (sample-rate expectation-to-list))))

(defmethod precision-in-seconds ((expectation-to-list expectation))
  "Returns the expectation precision in seconds"
  (/ (precision expectation-to-list) (float (sample-rate expectation-to-list))))

(defmethod absolute-duration-confidence ((expectation-to-modify expectation) (rhythm-expected rhythm))
  "Scales the confidence of the expectation prediction by the absolute duration of the rhythm"
  (let* ((duration (duration rhythm-expected))
	 (confidence-threshold 2.0)	; number of seconds of listening before listeners are confident of expectation.
	 (confidence-weighting (if (> duration confidence-threshold) 1.0 (/ duration confidence-threshold))))
    (setf (confidence expectation-to-modify) (* (confidence expectation-to-modify) confidence-weighting)))
  expectation-to-modify)

(defmethod absolute-duration-confidence ((expectations list) (rhythm-expected rhythm))
  (list 
   (list (first (first expectations))
	 (loop 
	    for expect in (second (first expectations))
	    collect (absolute-duration-confidence expect rhythm-expected)))))

(defmethod offset-expectation ((expectation-to-modify expectation) by-samples)
  (incf (expected-time expectation-to-modify) by-samples)
  expectation-to-modify) ; return the modified expectation.

(defmethod offset-expectation ((expectations list) by-samples)
  (mapcar (lambda (x) (offset-expectation x by-samples)) expectations))

(defun pad-minima (minima number-of-scales)
  "Inserts the zero index and the last scale index either end of the minima as boundaries
  to the search for the peak locations"
  (.concatenate (make-narray '(0)) minima (make-narray (list (1- number-of-scales)))))

(defun width-of-peaks (peaks minima &key (minima-distances minima))
  "Returns the widths of the peaks (by default measured in scales), by finding the distance between two minima either side of each peak"
  (let* ((minima-val (val minima))
	 (minima-widths (nlisp::narray-of-type minima-distances (.array-dimensions peaks)))
	 (minima-diffs (.diff minima-distances)))
    (loop
       for peak across (val peaks)
       for peak-index = 0 then (1+ peak-index)
       for right-position = (position peak minima-val :test #'<=)
       do (setf (.aref minima-widths peak-index) (.aref minima-diffs (1- right-position))))
    minima-widths))

;; (sample-width-of-peaks (make-narray '(30 46 68 76 88 103 111)) (make-narray '(26 37 67 71 81 95 106)) 128)

(defun sample-width-of-peaks (peaks minima number-of-scales voices-per-octave)
  "Returns the widths of the peaks in samples, by finding the distance between two minima either side of each peak"
  (let ((padded-minima (pad-minima minima number-of-scales)))
    (width-of-peaks peaks padded-minima :minima-distances (time-support padded-minima voices-per-octave))))

;; (sample-width-of-peaks (make-narray '(20 27)) (make-narray '(18 23 27 29)) 40 16)

;;; Precision = sharpness of peak. We calculate this as the area under the peak
;;; between the other peaks. Do this by calculating the distance from the ridge
;;; scale to the two nearest minima. The more narrow the width, the higher the precision.
(defun precisions-at-time (analysis time)
  "Returns the precision of each ridge at the given time"
  (let* ((maxima (ridge-peaks analysis))
	 (minima (ridge-troughs analysis))
	 (widths (width-of-peaks (.find (.column maxima time))
				 (pad-minima (.find (.column minima time)) (.row-count maxima)))))
    ;; Determine the widths in relative measures of scale span and from the inverse of
    ;; those, the precision.
    (.- 1d0 (./ widths (* (.row-count maxima) 1d0)))))

(defun precision-profile (ridge-peak-profile ridge-trough-profile voices-per-octave)
  "Determine the precision in samples, given the peaks and troughs expressed as vectors of scale indices"
  (diag-plot 'precision-profiles
    (plot (list ridge-peak-profile ridge-trough-profile) nil :styles '("impulses lw 2" "impulses lw 2")))
  (let* ((number-of-scales (.length ridge-peak-profile))
	 (peak-scales (.find ridge-peak-profile))
	 (trough-scales (.find ridge-trough-profile))
	 ;; Determine the widths between peaks in sample times.
	 (peak-widths (sample-width-of-peaks peak-scales trough-scales
					     number-of-scales voices-per-octave))
	 (precision-profile (make-double-array number-of-scales)))
;;     (format t "peaks at scales ~a~%troughs at scales ~a~%widths in scales ~a~%in samples ~a~%"
;; 	    peak-scales trough-scales
;; 	    (width-of-peaks peak-scales (pad-minima trough-scales number-of-scales))
;; 	    peak-widths)
    (setf (.arefs precision-profile peak-scales) peak-widths)
    precision-profile))

(defun precision-in-samples (maxima minima vpo)
  "Returns a matrix of the measure of precision of each peak in samples, with respect to the
  total number of scales, given the maxima and minima of the scaleogram."
  (let* ((time-freq-dimensions (.array-dimensions maxima))
	 (precision (make-double-array time-freq-dimensions)))
    (dotimes (time (.column-count maxima)) ; loop over the duration
      ;; (format t "precision of ~a~%" time)
      (setf (.column precision time) 
	    (precision-profile (.column maxima time) (.column minima time) vpo)))
    precision))

;;; This returns the normalised maximum value for each of the peaks. We could calculate
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
	     (peak-widths (width-of-peaks peak-scales (pad-minima (.find (.column minima time)) number-of-scales)))
	     ;; Determine the widths in relative measures of scale span and from the inverse of
	     ;; those, the precision.
	     (normalised-precision (.- 1d0 (./ peak-widths (* 1d0 (.sum peak-widths)))))
	     (precision-profile (make-double-array number-of-scales)))
	(setf (.arefs precision-profile peak-scales) normalised-precision)
	(setf (.column precision time) precision-profile)))
    precision))

(defun confidence-and-precision (analysis)
  "Returns the most likely which is the most confident and most precise"
  (let ((peaks (ridge-peaks analysis)))
    (.* peaks (most-precise peaks (ridge-troughs analysis)))))

(defun phase-diff-from-start (phase scale time)
  (format t "phase at time ~a phase at 0 ~a~%" (.aref phase scale time) (.aref phase scale 0))
  (.aref (phase-diff (make-narray (list (.aref phase scale time) (.aref phase scale 0)))) 0))

(defun normalised-phase (phi) 
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
  (let* ((norm-phase (normalised-phase (.aref phase scale time)))
	 (phase-offset (* time-projection norm-phase))
	 (zero-index (round (- time phase-offset))))
    (format t "Computed: time projection ~,3f phase zero index ~d norm-phase ~,3f phase-offset ~,3f~%" 
	    time-projection zero-index norm-phase phase-offset)
    (format t "value at expected phase zero index, & either side (~,3f ~,3f ~,3f)~%"
	    (.aref phase scale (1- zero-index))
	    (.aref phase scale zero-index)
	    (if (= zero-index time) 0 (.aref phase scale (1+ zero-index))))
    (+ time (* time-projection (- 1.0d0 norm-phase)))))

(defun phase-zero-samples (phase scale)
  "Returns the location of each phase 0 crossing index for a given scale"
  (let ((phase-at-scale (.row phase scale))
	(time-length (.array-dimension phase 1)))
    (.find (.and (.subarray (.>= phase-at-scale 0) (list 0 (list 1 (1- time-length))))
		 (.subarray (.< phase-at-scale 0) (list 0 (list 0 (- time-length 2))))))))

;; (plot (list (impulses-at (phase-zero-samples phase scale) (.array-dimension phase 1)) (.row phase scale)) nil)

;; TODO phase change from start of time
;; (phase-corrected-time (+ time (* (time-support scale vpo) 
;;			  (- 1.0d0 (normalised-phase (phase-diff-from-start phase scale time))))))
;;    (format t "time projection ~,5f phase weighting ~,5f time ~a expected-time ~,3f weighted ~,3f unweighted ~,3f~%"
;; (time-support scale vpo) (- 1.0d0 (normalised-phase phase-of-ridge)) time 
;;	(time-support scale vpo) (- 1.0d0 (normalised-phase (phase-diff-from-start phase scale time))) time 
;;	expected-time phase-corrected-time uncorrected-time)
;;    (format t "phase-diff ~a~%" (phase-diff-from-start phase scale time))

(defun phase-corrected-time-total-diff (time-projection phase scale time)
  "Compute projection time from the mean gradient of the phase from time until end of the analysis period"
  (let* ((phase-datum (.aref phase scale time))
	 (mean-gradient (mean (phase-diff (.row phase scale))))
	 (phase-projected-time (+ (/ (* 2 pi) mean-gradient) phase-datum)))
    (format t "Total: time projection ~,3f phase-projected-time ~,3f~%" time-projection phase-projected-time)
    (+ time phase-projected-time)))

(defun phase-corrected-time-final-diff (time-projection phase scale time)
  "Compute projection time from the mean gradient of the phase from time until end of the analysis period"
  (let* ((final-phase-diff (phase-diff (.subarray phase (list scale (list time t)))))
	 (phase-datum (.aref phase scale time))
	 (mean-gradient (mean final-phase-diff))
	 (phase-projected-time (+ (/ (* 2 pi) mean-gradient) phase-datum)))
    ;; (plot (.row phase scale) nil
    ;; 	  :aspect-ratio 0.66
    ;; 	  :title (format nil "Phase at ~a" scale))
    ;; (read-line)
    ;; (plot projected-phase nil)
    ;; (read-line)
    ;; (plot phase-diff nil)
    (format t "Final: time projection ~,3f phase-projected-time ~,3f~%" time-projection phase-projected-time)
    (+ time phase-projected-time)))

;;; TODO can we replace with (phase-occurrances )?
(defun phase-corrected-time-chasing (time-projection phase scale time)
  "Returns the time-projection corrected by it's phase measure based on the most recent occurrance of zero phase. 
   Chases for phase-zero values backwards from time."
  (loop
     for zero-index from time downto 1
     until (and (not (minusp (.aref phase scale zero-index)))
		(minusp (.aref phase scale (1- zero-index))))
     finally (return 
	       (let ((number-of-projections (/ (- time zero-index) time-projection)))
		 (format t "Chasing: time projection ~,3f phase zero index ~d computed phase offset ~,3f # of projections ~,3f~%" 
			 time-projection zero-index (- time zero-index) number-of-projections)
		 ;; if we chase back so far that the projection falls before time, we
		 ;; project enough multiples of the period beyond time.
		 (cond ((> number-of-projections 1.0)
			(+ zero-index (* (ceiling number-of-projections) time-projection)))
		       ;; If the projection extends sufficiently beyond time, just use the projection.
		       ((< number-of-projections 0.9)
			(+ zero-index time-projection))
		       ;; Otherwise the projection is falling practically on time, so we
		       ;; need a further time projection.
		       (t 
			(+ zero-index time-projection time-projection)))))))

(defun phase-histogram (scaleogram scale times)
  (let* ((phase (scaleogram-phase scaleogram))
	 (magnitude-at-times (.arefs (.row (scaleogram-magnitude scaleogram) scale) times))
	 (phases-at-times (.arefs (.row phase scale) times))
	 (title (format nil "Phases of Select Times for Scale ~a" scale)))
;;     (plot-binned-histogram (.* magnitude-at-times phases-at-times)
;; 			   (format nil "Phases of Select Times for Scale ~a" scale))))
    (multiple-value-bind (counts time-bins accumulated-likelihood)
	(binned-values phases-at-times magnitude-at-times)
      (window)
      (reset-plot)
      (plot-command "set title font \"Times,24\"")
      (plot-command "set xlabel font \"Times,24\"")
      (plot-command "set ylabel font \"Times,24\"")
      (plot-command "set xtics ~a out" 0.2)
      ;; Downsample the bin labels.
      ;; (.arefs time-bins (.* (.iseq 0 (/ (.length time-bins) 2)) 2))
      (plot accumulated-likelihood time-bins
	    :styles '("boxes fill solid 1.0 border -1")
	    :xlabel title
	    :ylabel "Occurrence"
	    :legends (list (format nil "~a Occurrence" title))
	    :aspect-ratio 0.66
	    :reset nil
	    :title (format nil "Occurrence of ~a" title)))
    (close-window)))


(defun phase-histogram-of-rhythm (rhythm)
  (let* ((a (analysis-of-rhythm rhythm :padding #'causal-pad))
	 (mls (most-likely-scales (tempo-weighted-ridge-persistency-of a))))
    (loop 
       for likely-scale across (val mls)
       do (phase-histogram (scaleogram a) likely-scale (onsets-in-samples rhythm)))))

(defun random-expectations (subset-rhythm full-rhythm)
  "Generate 3 random values between the end of the rhythm and the maximum rhythm"
  (loop
     repeat 3
     with max-time = (- (duration-in-samples full-rhythm) (duration-in-samples subset-rhythm))
     for projection = (random max-time)
     collect (make-instance 'expectation 
			    :time (+ (duration-in-samples subset-rhythm) projection)
			    :period projection
			    :sample-rate (sample-rate subset-rhythm)
			    :confidence (random 1.0d0)
			    :precision (random 1.0d0))))

(defun expectation-of-scale-at-time (scale time scaleogram confidence precision sample-rate
				     &key (phase-correct-from nil) (time-limit-expectancies nil))
  "Returns an expectation instance. Effectively a factory method with phase correction of expectation time"
  (let* ((vpo (voices-per-octave scaleogram))
	 (max-time (if time-limit-expectancies (duration-in-samples scaleogram) most-positive-fixnum))
	 (projection (time-support scale vpo))
	 (uncorrected-time (+ time projection))
	 ;; Compute the time prediction, modified by the phase.
	 ;; (phase-corrected-time (phase-corrected-time-total-diff projection (scaleogram-phase scaleogram) scale time))
	 ;; (phase-corrected-time (phase-corrected-time-final-diff projection (scaleogram-phase scaleogram) scale time))
	 (phase-corrected-time (phase-corrected-time-chasing projection (scaleogram-phase scaleogram) scale time))
	 ;; (phase-corrected-time (phase-corrected-time-computed projection (scaleogram-phase scaleogram) scale time))
	 ;; (phase-corrected-time (phase-corrected-time-from projection time phase-correct-from))
	 (expected-time (if phase-correct-from phase-corrected-time uncorrected-time)))
    ;; (format t "phase zero samples at: ~a~%diffs ~a~%" 
    ;; 	    (phase-zero-samples (scaleogram-phase scaleogram) scale)
    ;; 	    (.diff (phase-zero-samples (scaleogram-phase scaleogram) scale)))
    (format t "time ~a time projection ~,3f phase-corrected ~,3f uncorrected ~,3f expected-time ~,3f~%"
	    time projection phase-corrected-time uncorrected-time expected-time)
    (make-instance 'expectation 
		   :time (min expected-time max-time)
		   :period projection
		   :sample-rate sample-rate
		   :confidence (.aref confidence scale)
		   :precision (.aref precision scale))))

(defun expectancy-of-ridge-at-time (ridge time scaleogram all-precision all-peaks sample-rate
				    &key (time-limit-expectancies nil) (phase-correct-from nil))
  "Return an expectation instance holding the expection time, the confidence and the precision" 
  ;; (plot-phase ridge (scaleogram-phase scaleogram))
  ;; (plot (phase-of ridge scaleogram) nil :aspect-ratio 0.15)
  ;; (format t "time of ridge at ~,3f" time)
  ;; (read-line)
  (expectation-of-scale-at-time (scale-at-time ridge time) 
				time
				scaleogram 
				;; energy = absolute height of scaleogram or ridge-peaks (depending on what's
				;; passed into all-peaks)
				(.column all-peaks time)
				(.column all-precision time)
				sample-rate
				:phase-correct-from phase-correct-from
				:time-limit-expectancies time-limit-expectancies))

(defun expectancies-of-skeleton-at-times (skeleton times scaleogram precision ridge-peaks sample-rate &key
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
							   sample-rate
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
	 (precision (precision-in-samples scale-peaks scale-troughs vpo))
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
      (plot (list (.reverse (.* (.column tempo-beat-preference (first (last times-to-check)))
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
				       sample-rate
				       :phase-correct-from phase-correct-from)))


;; (expectancies-of-rhythm-integrator rhythm-to-analyse :times-to-check (list (1- (duration-in-samples rhythm-to-analyse))))

(defun most-persistent-scales (ridge-persistency)
  "Return those scales sorted in order of most persistent"
  ;; TODO Need to look under the area of the curve to determine the peaks which are worth identifying.
  ;; TODO should we use +1? Because the indexes for ridge-persistency are zero?
  (highest-peaks ridge-persistency))

;;; TODO Could be moved out of expectancies.
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
			   (memory-limit 3) 
			   ;; 0.5 standard deviation above the mean is our definition of worthwhile.
			   (worthwhile 0.5d0))
  "Returns a list of scales given the ridge persistency which are most likely to be expectation periods"
  (let* ((most-likely-scales (most-persistent-scales ridge-persistency))
	 (peak-persistencies (.arefs ridge-persistency most-likely-scales))
	 (above-average-peaks (+ (mean peak-persistencies) (* worthwhile (stddev peak-persistencies))))
	 ;; threshold the scales used to only those above half the most persistent.
	 (number-above-average (or (position above-average-peaks (val peak-persistencies) :test #'>=) 0))
	 (number-likely (min number-above-average memory-limit)))
    ;; (format t "above-average peaks threshold ~a~%" above-average-peaks)
    (.subarray most-likely-scales (list 0 (list 0 (1- number-likely))))))
    ;; TODO unneeded since we are weighting by tempo?
    ;; (remove-if (lambda (scale) (<= scale cutoff-scale)) (val most-likely-scales))

(defun expectancies-of-rhythm-ridge-persistency (rhythm-to-analyse 
						 &key (times-to-check (list (1- (duration-in-samples rhythm-to-analyse))))
						 (phase-correct-from nil))
  "Return a list structure of expectancies. Computes the expectancies using the maximum
   value of the ridge persistency" 
  ;; (declare (ignore phase-correct-from))
  (let* ((analysis (analysis-of-rhythm rhythm-to-analyse :padding #'causal-pad))
	 (scaleogram (scaleogram analysis))
	 (tempo-weighted-ridge-persistency (tempo-weighted-ridge-persistency-of analysis))
	 (most-likely-scales (most-likely-scales tempo-weighted-ridge-persistency))
	 ;; Make a new peak profile of just the most likely scales for the precision calculation.
	 (likely-persistency-peaks (impulses-at most-likely-scales (.length tempo-weighted-ridge-persistency)))
	 ;; Since ridge-persistency produces many zero values, and extrema-points-vector won't
	 ;; catch corners, only minima including locations of zeros, tightens the precision estimation.
	 (persistency-troughs (.or (extrema-points-vector tempo-weighted-ridge-persistency :extrema :min) 
				   (.not tempo-weighted-ridge-persistency))) 
	 (precision (precision-profile likely-persistency-peaks persistency-troughs
				       (voices-per-octave scaleogram)))
	 (time (first times-to-check)))
    (diag-plot 'tempo-ridge-persistency
      (plot-ridge-persistency tempo-weighted-ridge-persistency scaleogram (name rhythm-to-analyse)))
    (list ; over times
     (list time
	   (loop
	      for scale across (val most-likely-scales)
	      collect (expectation-of-scale-at-time scale time scaleogram
						    tempo-weighted-ridge-persistency
						    precision
						    (sample-rate rhythm-to-analyse)
						    :phase-correct-from phase-correct-from))))))

(defun expectancies-of-rhythm (rhythm &key (times-to-check (nlisp::array-to-list (onsets-in-samples rhythm)))
			       (time-limit-expectancies nil) (phase-correct-from nil))
  "Return a list structure of expectancies. If limit-expectancies is true, only calculate
  expectancies for events within the period analysed. If false, forward expectancies are generated
  for the last event."
  (let* ((rhythm-analysis (analysis-of-rhythm rhythm :padding #'causal-pad))
	 (rhythm-scaleogram (scaleogram rhythm-analysis))
	 ;; The skeleton has already picked scale peaks at each time.
	 (skeleton (skeleton rhythm-analysis))
	 (precision (precision-in-samples (ridge-peaks rhythm-analysis) (ridge-troughs rhythm-analysis) 
					  (voices-per-octave rhythm-scaleogram)))
	 (preferred-tempo-scale (preferred-tempo-scale (voices-per-octave rhythm-scaleogram) (sample-rate rhythm)))
	 ;; Cut off freq above 3 stddev's from preferred-scale = 3 octaves.
	 (cutoff-scale (- preferred-tempo-scale (* 3 (voices-per-octave rhythm-scaleogram)))))
    (if time-limit-expectancies 
	(setf times-to-check (butlast times-to-check)))
    (diag-plot 'scale-energy-profile
      (plot-scale-energy+peaks-at-time rhythm-scaleogram 
				       (first (last times-to-check))
				       (ridge-peaks rhythm-analysis)))
    (format t "times to check ~a~%" times-to-check)
    (format t "cutoff scale ~a period ~a~%" 
	    cutoff-scale (time-support cutoff-scale (voices-per-octave rhythm-scaleogram)))
    (expectancies-of-skeleton-at-times skeleton times-to-check rhythm-scaleogram 
				       precision (.normalise (scaleogram-magnitude rhythm-scaleogram))
				       (sample-rate rhythm)
				       :cutoff-scale cutoff-scale :phase-correct-from phase-correct-from)))

;;; Accumulation of onset expectations.
(defun accumulated-confidence-of-expectations (all-expectations)
  "Given a set of expectations, sum the confidences (when overlapping), returns an array spanning the projection time"
  (if (null all-expectations) ;; catch the empty list for debugging purposes.
      (make-double-array 0)
      (let* ((all-expect-times (make-narray (mapcar (lambda (e) (expected-time e)) all-expectations)))
	     (maximum-expect-time (ceiling (.max all-expect-times)))
	     (projection-confidences (make-double-array (1+ maximum-expect-time))))
	(dolist (expectation all-expectations)
	  (incf (.aref projection-confidences (floor (expected-time expectation))) (confidence expectation)))
	projection-confidences)))
  
(defmethod plot-expectations+rhythm ((rhythm-to-expect rhythm) (all-expectations list) &key 
				     (rhythm-starts-at 0)
				     (title (format nil "Accumulated expectations of ~a" (name rhythm-to-expect))))
  "Plot the expectation times, confidences and precision and the rhythm."
  (let* ((expectancy-confidences (accumulated-confidence-of-expectations all-expectations))
	 (expect-times (.find expectancy-confidences))
	 (expect-confidences (.arefs expectancy-confidences expect-times)))
    (plot (list (time-signal rhythm-to-expect) expect-confidences expect-confidences)
	  (list (.iseq rhythm-starts-at (+ rhythm-starts-at (1- (duration-in-samples rhythm-to-expect)))) 
		expect-times 
		expect-times)
	  :styles '("impulses linetype 3" "points linetype 1 pointtype 10" "impulses linetype 1")
	  :legends (list "original rhythm" "accumulated expectations" "")
	  :xlabel "Time"
	  :ylabel "Confidence" 
	  :aspect-ratio 0.15
	  :title title)))

(defmethod plot-expectations (all-expectations title)
  "Plot the expectation times, confidences and precision."
  (let ((expect-times (make-narray (mapcar (lambda (expect) (expected-time expect)) all-expectations)))
	(expect-confidences (make-narray (mapcar (lambda (expect) (confidence expect)) all-expectations))))
    (window)
    (plot expect-confidences expect-times 
 	  :styles '("impulses")
	  :xlabel "Time" :ylabel "Confidence" 
 	  :aspect-ratio 0.66
 	  :title title)
    (close-window)))

;;; TODO needs work...
(defun last-expectations-project-onsets (rhythm-to-expect)
  "Determine the expectations by summing the expectations at each onset"
  (let* ((all-expectations-of-rhythm (expectancies-of-rhythm rhythm-to-expect :phase-correct-from nil :time-limit-expectancies nil))
	 ;; rearrange all the expectations into a single list.
	 (all-expectations (reduce #'append (mapcar #'second all-expectations-of-rhythm)))
	 (expectancy-confidences (accumulated-confidence-of-expectations all-expectations)))
    ;; projections into future
    (.subarray expectancy-confidences 
	       (list 0 (list
			(duration-in-samples rhythm-to-expect)
			(1- (.length expectancy-confidences)))))))
    
;;; (great) last-expectations, Charles Dickens undiscovered work from his secret life as a Lisp programmer...
;;; This is needed to throw away the time and break the last expectation out of the list.
(defun last-expectations (rhythm-to-expect 
			  &key (last-time (1- (duration-in-samples rhythm-to-expect)))
			  (expectancies-generator #'expectancies-of-rhythm-ridge-persistency))
  "Return the list of expectations for the last moment in the rhythm"
  (format t "Using ~a~%" expectancies-generator)
  (let ((expectations (funcall expectancies-generator rhythm-to-expect 
			       ;; Do no phase correction when testing metrical expectancies.
			       ;; :phase-correct-from nil
			       :phase-correct-from (last-onset-time rhythm-to-expect)
			       :times-to-check (list last-time))))
    (print-expectations expectations)
    (second (first expectations))))
  
;;; This is needed to throw away the time and break the last expectation out of the list.
;; (defun last-onset-expectations (rhythm-to-expect)
;;   "Return the list of expectations for the last onset in the rhythm"
;;   (let ((last-time (last-onset-time rhythm-to-expect)))
;;     (second (first (expectancies-of-rhythm rhythm-to-expect 
;; 					   :times-to-check (list last-time)
;; 					   :phase-correct-from last-time)))))

;;; TODO last-onset-expectations could be a macro, except for the phase-correct-from parameter.
(defmacro last-onset-expectations (rhythm-to-expect)
  `(last-expectations ,rhythm-to-expect 
 		      :expectancies-generator #'expectancies-of-rhythm 
 		      :last-time (last-onset-time ,rhythm-to-expect)))

;; (plot (list (phase-of tactus rhythm-scaleogram) (.row (mrr::scaleogram-phase rhythm-scaleogram) 100)) nil :aspect-ratio 0.15)
