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
  (format stream " time: ~,5f confidence: ~,5f precision: ~,5f" 
	  (expected-time expectation-to-print) 
	  (confidence expectation-to-print)
	  (precision expectation-to-print)))

(defmethod time-in-seconds ((expectation-to-report expectation) sample-rate)
  (/ (expected-time expectation-to-report) (float sample-rate)))

;;; TODO obsolete.
(defmethod list-in-seconds ((expectation-to-list expectation) sample-rate)
  "Returns the expectancy as a list, times in seconds"
  (list (time-in-seconds expectation-to-list sample-rate) 
	(confidence expectation-to-list)
	(precision expectation-to-list)))

;;; TODO this should produce XML
(defmethod exchange-format ((expectation-to-exchange expectation) sample-rate)
  "Returns the expectancy in the EmCAP interchange output format."
  (format nil "~,5f ~,5f ~,5f;~:{~,5f,~,5f~:^ ~}" 
	  (time-in-seconds expectation-to-exchange sample-rate) 
	  (precision expectation-to-exchange)
	  (confidence expectation-to-exchange)
	  (expected-features expectation-to-exchange)))

(defun width-of-peaks (peaks minima number-of-scales)
  "Returns the widths of the peaks, by finding the distance between two minima either side of each peak"
  (let* ((minima-val (val minima))
	 (minima-length (length minima-val))
	 (minima-widths (make-fixnum-array (.array-dimensions peaks)))
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

(defun normalized-precision (maxima minima)
  "Returns a matrix of the relative measure of precision of each peak, with respect to the
  total number of scales, given the maxima and minima of the scaleogram."
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
	     (normalized-precision (./ peak-widths (coerce number-of-scales 'double-float)))
	     (precision-profile (make-double-array number-of-scales)))
	;; TODO Have to do this in two assignments since we can't do .arefs across a single column.
	(setf (.arefs precision-profile peak-scales) normalized-precision)
	(setf (.column precision time) precision-profile)))
    precision))

;; (defun precision (analysis)
;;   "Returns the precision of each ridge"
;;   (loop
;;      with maxima = (ridge-peaks analysis)
;;      with minima = (ridge-troughs analysis)
;;      with number-of-scales = (.row-count maxima)
;;      for time from 0 below (duration-in-samples analysis)
;;      for peak-scales = (.find (.column maxima time))
;;      for peak-widths = (width-of-peaks peak-scales (.find (.column minima time)))
;;      ;; Determine the widths in relative measures of scale span and from the inverse of
;;      ;; those, the precision.
;;      collect (./ peak-widths (* number-of-scales 1d0))))

(defun confidence-and-precision (analysis)
  "Returns the most likely which is the most confident and most precise"
  (let ((peaks (ridge-peaks analysis)))
    (.* peaks (most-precise peaks (ridge-troughs analysis)))))

(defun expectancy-of-ridge-at-time (ridge time scaleogram all-precision &key (time-limit-expectancies nil))
  "Return an expectation instance holding the expection time, the confidence and the precision" 
  (let* ((vpo (voices-per-octave scaleogram))
	 (max-time (if time-limit-expectancies (duration-in-samples scaleogram) most-positive-fixnum))
	 (magnitude (scaleogram-magnitude scaleogram))
	 (scale (scale-at-time ridge time))
	 (expected-time (+ time (time-support scale vpo)))
	 ;; energy = absolute height (of scaleogram or ridge-peaks)
	 (energy (.aref magnitude scale time))
	 (relative-ridge-duration (if (> time 0) (- 1.0 (/ (start-sample ridge) time)) 1)))
    (make-instance 'expectation 
		   :time (min expected-time max-time)
		   ;; :sample-rate (sample-rate ?) TODO sample rate of?
		   ;; relative confidence = energy * duration of the ridge up until this moment.
		   :confidence energy
		   :precision (.aref all-precision scale time))))
;; TODO :expected-features (create-expected-features magnitude)

(defun expectancies-of-skeleton-at-times (skeleton times scaleogram precision &key (cutoff-scale 16))
  "Returns the expectancies determined from the skeleton, scaleogram and precision at the indicated times"
  (loop
     for time in times
     collect (list time
		   (loop
		      for ridge in (ridges-at-time skeleton time)
		      ;; filter out the ridges higher than a cut off point determined by preferred tempo rate.
		      when (> (scale-at-time ridge time) cutoff-scale)
		      collect (expectancy-of-ridge-at-time ridge time scaleogram precision 
							   :time-limit-expectancies nil)))))

(defun expectancies-of-rhythm-integrator (rhythm-to-analyse 
					  &key (times-to-check (nlisp::array-to-list (onsets-in-samples rhythm-to-analyse))))
  "Return a list structure of expectancies. Computes the expectancies using the maximum
   value of the cumulative sum of the scaleogram energy" 
  (let* ((analysis (analysis-of-rhythm rhythm-to-analyse :padding #'causal-pad))
	 (scaleogram (scaleogram analysis))
	 (cumulative-scale-persistency (cumsum (scaleogram-magnitude scaleogram)))
	 (vpo (voices-per-octave scaleogram))
	 (sample-rate (sample-rate rhythm-to-analyse))
	 (salient-scale (preferred-tempo-scale vpo sample-rate))
	 (tempo-beat-preference (tempo-salience-weighting salient-scale 
							  (.array-dimensions cumulative-scale-persistency)
							  :octaves-per-stddev 1.0))
 	 (weighted-persistency-profile (.* cumulative-scale-persistency tempo-beat-preference))
	 (weighted-scale-peaks (determine-scale-peaks weighted-persistency-profile))
	 (weighted-scale-troughs (extrema-points weighted-persistency-profile :extrema :min))
	 (weighted-precision (normalized-precision weighted-scale-peaks weighted-scale-troughs))
       	 (weighted-skeleton (skeleton-of-ridge-peaks scaleogram weighted-scale-peaks)))
    (diag-plot 'weighted-beat-ridge
      (window)
      (plot-image #'magnitude-image (list weighted-persistency-profile) '((1.0 0.5) (0.0 0.3))
		  (axes-labelled-in-seconds scaleogram sample-rate 4)
		  :title (format nil "weighted persistency profile of ~a" (name rhythm-to-analyse)))
      (close-window))
    (plot-scale-energy+peaks-at-time scaleogram 
				     (first (last times-to-check))
				     weighted-scale-peaks :sample-rate (sample-rate rhythm-to-analyse))
    (expectancies-of-skeleton-at-times weighted-skeleton
				       times-to-check
				       scaleogram
				       weighted-precision)))

;; (expectancies-of-rhythm-integrator rhythm-to-analyse :times-to-check (list (1- (duration-in-samples rhythm-to-analyse))))


(defun expectancies-of-rhythm (rhythm &key (times-to-check (nlisp::array-to-list (onsets-in-samples rhythm)))
			       (time-limit-expectancies nil))
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
    ;; (format t "times to check ~a~%" times-to-check)
    (expectancies-of-skeleton-at-times skeleton times-to-check rhythm-scaleogram precision
				       :cutoff-scale cutoff-scale)))

#|
    (loop
       for time in times-to-check
       ;; do (plot-scale-energy+peaks-at-time rhythm-scaleogram time (ridge-peaks rhythm-analysis) :sample-rate (sample-rate rhythm))
       ;; (break)
       collect (list time
		     ;; TODO able to make this a separate function? 
		     ;; (expectancies-at-time skeleton time)
		     (loop
			for ridge in (ridges-at-time skeleton time)
			;; filter out the ridges higher than a cut off point determined by preferred tempo rate.
			when (> (scale-at-time ridge time) cutoff-scale)
			collect (expectancy-of-ridge-at-time ridge 
							     time
							     rhythm-scaleogram
							     precision 
							     :time-limit-expectancies time-limit-expectancies))))))
|#

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
;;; ONSET-TIME,ONSET-TIME-VAR;FEAT1-VAL,FEAT1-VAR FEAT2-VAL,FEAT2-VAR ... FEATn-VAL,FEATn-VAR
;;; Because this format is so full of useless context sensitive formatting noise, we strip
;;; all the crap away to get it to a form that Lisp can devour simply.
(defun strip-emcap-formatting (emcap-data-line)
  (destructuring-bind (onset features) (split-string emcap-data-line #\;)
    (list (mapcar #'read-from-string (split-string onset #\,))
	  (mapcar (lambda (feature-string) (mapcar #'read-from-string (split-string feature-string #\,))) 
		  (split-string features #\Space)))))

(defun summarise-features-to-accent (features)
  (reduce #'+ (mapcar #'car features)))

(defun weighted-onsets-from-emcap-stream (stream)
  (loop
     for emcap-data-record = (read-line stream nil)
     while emcap-data-record
     collect (destructuring-bind (onset-time features) (strip-emcap-formatting emcap-data-record)
	       (list (first onset-time) (summarise-features-to-accent features)))))

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
	 (expectancies-at-times (expectancies-of-rhythm times-as-rhythm)))
    (with-open-file (expectancy-file output-filepath :direction :output :if-exists :supersede)
      (write-expectancies-to-stream (last expectancies-at-times) sample-rate expectancy-file))))

(defun last-expectancy-of-salience (saliency-filepath onset-times-filepath output-filepath
				    &key (sample-rate 200.0d0)) 
  "Computes the expectancies at the last moment in the file from the perceptual salience trace"
  (let* ((times-as-rhythm (perceptual-salience-to-rhythm saliency-filepath onset-times-filepath :sample-rate sample-rate))
	 (expectancies-at-times (expectancies-of-rhythm times-as-rhythm)))
    (with-open-file (expectancy-file output-filepath :direction :output :if-exists :supersede)
      (write-expectancies-to-stream (last expectancies-at-times) sample-rate expectancy-file))))

