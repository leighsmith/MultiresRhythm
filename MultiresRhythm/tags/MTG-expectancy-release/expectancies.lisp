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
(use-package :cl-fad)

;; Routines to use with the cl-fad directory routines.
(defun is-file-of-type (filepath type)
  (equal (pathname-type filepath) type))

(defun is-dot-file (filepath)
  (equal (aref (pathname-name filepath) 0) #\.))

(defun does-not-have-file-of-type (filepath type)
  (and (is-file-of-type filepath "corneronsetsqrt-p1-f30-s10-g075")	; ensure filepath isn't a clap file
       (not (is-dot-file filepath))		; it isn't a .DS_Store
       ;; and that it doesn't have an accompanying clap file
       (not (probe-file (make-pathname :defaults filepath :type type)))))

(defun does-not-have-expectancies-file (filepath)
  (does-not-have-file-of-type filepath "expectancies"))

(defun does-not-have-last-expectancy-file (filepath)
  (does-not-have-file-of-type filepath "last_expectancy"))

(defun does-not-have-claps-file (filepath)
  (does-not-have-file-of-type filepath "claps"))

;;; TODO can probably be moved out.								    
;;; Convert times in seconds to a rhythm.
(defun clap-to-times-in-file (filepath &key (sample-rate 200.0d0))
  (let* ((times-in-seconds (.column (.load filepath :format :text) 0))
	 (times-as-rhythm (rhythm-of-onsets (pathname-name filepath) times-in-seconds :sample-rate sample-rate))
	 (clap-times (clap-to-rhythm times-as-rhythm 
				     :tactus-selector #'create-weighted-beat-ridge 
				     ;; :beat-multiple 1
				     :start-from-beat 0))
    	 (clap-times-in-seconds (./ clap-times sample-rate))
	 (clap-filepath (make-pathname :defaults filepath :type "claps")))
    (format t "Beat times of ~a in seconds:~%~a~%" (name times-as-rhythm) clap-times-in-seconds)
    (.save clap-times-in-seconds clap-filepath :format :text)))


(defun width-of-peaks (peaks minima number-of-scales)
  "Returns the widths of the peaks, by finding the distance between two minima either side of each peak"
  (let ((minima-diffs (.row (.diff minima) 0))
	(minima-val (val minima))
	(last-minima (.aref minima (1- (.length minima))))
	(minima-widths (make-fixnum-array (.array-dimensions peaks))))
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

#|
(defun precision (analysis)
  "Returns the precision of each ridge"
  (loop
     with maxima = (ridge-peaks analysis)
     with minima = (ridge-troughs analysis)
     with number-of-scales = (.row-count maxima)
     for time from 0 below (duration-in-samples analysis)
     for peak-scales = (.find (.column maxima time))
     for peak-widths = (width-of-peaks peak-scales (.find (.column minima time)))
     ;; Determine the widths in relative measures of scale span and from the inverse of
     ;; those, the precision.
     collect (./ peak-widths (* number-of-scales 1d0))))
|#

;;; Need better name.
(defun normalized-precision-old (maxima minima)
  "Returns a matrix of the relative measure of precision of each peak, 
given the maxima and minima of the scaleogram"
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
  "Returns a matrix of the relative measure of precision of each peak, 
given the maxima and minima of the scaleogram"
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
	(setf (.arefs precision-profile peak-scales) normalized-precision)
	(setf (.column precision time) precision-profile)))
    precision))


(defun confidence-and-precision (analysis)
  "Returns the most likely which is the most confident and most precise"
  (let ((peaks (ridge-peaks analysis)))
    (.* peaks (normalized-precision-old peaks (ridge-troughs analysis)))))

(defun expectancy-of-ridge-at-time (ridge time scaleogram all-precision &key
				    (time-limit-expectancies t))
  "Return a list indicating the expection time, the confidence and the precision" 
  (let* ((vpo (voices-per-octave scaleogram))
	 (max-time (if time-limit-expectancies (duration-in-samples scaleogram) most-positive-fixnum))
	 (magnitude (scaleogram-magnitude scaleogram))
	 (scale (scale-at-time ridge time))
	 (expected-time (+ time (time-support scale vpo)))
	 ;; energy = relative height (of scaleogram or ridge-peaks)
	 (energy (/ (.aref magnitude scale time) (.max (.column magnitude time))))
	 (relative-ridge-duration (if (> time 0) (- 1.0 (/ (start-sample ridge) time)) 1))
	 ;; relative confidence = energy * duration of the ridge up until this moment.
	 (confidence (* energy relative-ridge-duration))
	 (precision (.aref all-precision scale time)))
    (list (min expected-time max-time) confidence precision)))

(defun expectancies-of-rhythm (rhythm &key (time-limit-expectancies t))
  "Return a list structure of expectancies. If limit-expectancies is true, only calculate
  expectancies for events within the period analysed. If false, forward expectancies are generated
  for the last event."
  (let* ((rhythm-analysis (analysis-of-rhythm rhythm))
	 (rhythm-scaleogram (scaleogram rhythm-analysis))
	 ;; The skeleton has already picked scale peaks at each time.
	 (skeleton (skeleton rhythm-analysis))
	 (precision (normalized-precision (ridge-peaks rhythm-analysis) (ridge-troughs rhythm-analysis)))
	 (preferred-tempo-scale (preferred-tempo-scale (voices-per-octave rhythm-scaleogram) (sample-rate rhythm)))
	 ;; Cut off freq above 3 stddev's from preferred-scale = 3 octaves.
	 (cutoff-scale (- preferred-tempo-scale (* 3 (voices-per-octave rhythm-scaleogram))))
	 (times-to-check (nlisp::array-to-list (onsets-in-samples rhythm))))
    (if time-limit-expectancies (setf times-to-check (butlast times-to-check)))
    (loop
       for time in times-to-check
       for event-index = 0 then (1+ event-index)
       ;; do (plot-scale-energy+peaks-at-time rhythm-scaleogram time (ridge-peaks rhythm-analysis) :sample-rate (sample-rate rhythm))
       ;; (break)
       collect (list event-index time
		     (loop
			for ridge in (ridges-at-time skeleton time)
			;; filter out the ridges higher than a cut off point determined by preferred tempo rate.
			when (> (scale-at-time ridge time) cutoff-scale)
			collect (expectancy-of-ridge-at-time ridge 
							     time
							     rhythm-scaleogram
							     precision 
							     :time-limit-expectancies time-limit-expectancies))))))

(defun expectancies-in-seconds (expectancy-structure sample-rate)
  "Converts sample times to seconds"
  (mapcar (lambda (expected-time) 
	    (list (first expected-time)
		  (/ (second expected-time) (float sample-rate))
		  (mapcar (lambda (x) (list (/ (first x) (float sample-rate)) (second x) (third x)))
			  (third expected-time)))) expectancy-structure))

(defun label-expectancies (expectancy-structure sample-labels)
  (loop 
     for expectancy in expectancy-structure
     collect (list (first expectancy) 
		   (second expectancy)
		   (.aref sample-labels (first expectancy) 0)
		   (floor (.aref sample-labels (first expectancy) 1))
		   (floor (.aref sample-labels (first expectancy) 2))
		   (third expectancy))))

(defun expectancies-at-times-in-file (filepath &key (sample-rate 200.0d0))
  (let* ((events (.load filepath :format :text))
	 (times-in-seconds (.column events 0))
	 (times-as-rhythm (rhythm-of-onsets (pathname-name filepath) times-in-seconds :sample-rate sample-rate))
	 (expectancies-at-times (expectancies-of-rhythm times-as-rhythm))
    	 (expectancy-times-in-seconds (expectancies-in-seconds expectancies-at-times sample-rate))
	 (labelled-expectancy-times (label-expectancies expectancy-times-in-seconds events))
	 (expectancy-filepath (make-pathname :defaults filepath :type "expectancies")))
    ;; Write the structure to a file.
    (with-open-file (expectancy-file expectancy-filepath :direction :output :if-exists :supersede)
      (format expectancy-file "~{~{~d, ~,5f, ~,5f, ~d, ~d, ~:{~,5f ~,5f ~,5f~:^; ~}~}~%~}" labelled-expectancy-times))))

(defun last-expectancy-of-file (filepath &key (sample-rate 200.0d0))
  (let* ((events (.load filepath :format :text))
	 (times-in-seconds (.column events 0))
	 (times-as-rhythm (rhythm-of-onsets (pathname-name filepath) times-in-seconds :sample-rate sample-rate))
	 (expectancies-at-times (expectancies-of-rhythm times-as-rhythm :time-limit-expectancies nil))
    	 (expectancy-times-in-seconds (expectancies-in-seconds expectancies-at-times sample-rate))
	 (labelled-expectancy-times (label-expectancies expectancy-times-in-seconds events))
	 (expectancy-filepath (make-pathname :defaults filepath :type "last_expectancy")))
    ;; Write the structure to a file.
    (with-open-file (expectancy-file expectancy-filepath :direction :output :if-exists :supersede)
      (format expectancy-file "~{~{~d, ~,5f, ~,5f, ~d, ~d, ~:{~,5f ~,5f ~,5f~:^; ~}~}~%~}"
	      (last labelled-expectancy-times)))))

(defparameter *ricards-data* "/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/")

;; (walk-directory *ricards-data* #'print :test #'does-not-have-claps-file)

;; (walk-directory *ricards-data* #'clap-to-times-in-file :test #'does-not-have-claps-file)

;; (clap-to-times-in-file #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/RobertRich/Java Gourd 01.corneronsetsqrt-p1-f30-s10-g075")
;; (clap-to-times-in-file
;;  #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/KeithLeblanc/clever01.corneronsetsqrt-p1-f30-s10-g075")
;; (clap-to-times-in-file
;;  #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/KeithLeblanc/4-Down 1.corneronsetsqrt-p1-f30-s10-g075")

;;; Crashers!
;; (walk-directory "/Volumes/iDisk/Research/Data/RicardsOnsetTests/crashers/" #'clap-to-times-in-file :test #'does-not-have-claps-file)
;; (clap-to-times-in-file #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/Acid/drums1304.corneronsetsqrt-p1-f30-s10-g075")
;; (clap-to-times-in-file #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/KeithLeblanc/fourkick1.corneronsetsqrt-p1-f30-s10-g075")


;; (expectancies-at-times-in-file #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/RobertRich/Java Gourd 01.corneronsetsqrt-p1-f30-s10-g075")

;; (expectancies-at-times-in-file #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/Burning/3.corneronsetsqrt-p1-f30-s10-g075")

;; (walk-directory *ricards-data* #'expectancies-at-times-in-file :test #'does-not-have-expectancies-file)

;; (walk-directory *ricards-data* #'last-expectancy-of-file :test #'does-not-have-last-expectancy-file)
