;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Expectancy File I/O
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2007
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

;;; TODO this should produce XML
(defmethod exchange-format ((expectation-to-exchange expectation) sample-rate)
  "Returns the expectancy in the EmCAP interchange output format."
  (format nil "~{~,5f~^ ~};~:{~,5f,~,5f~:^ ~}" 
	  (expectation-in-seconds expectation-to-exchange sample-rate) 
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

(defun rhythm-of-emcap-onset-file (input-filepath sample-rate)
  (with-open-file (emcap-data-stream input-filepath)
    (read-emcap-onsets-from-stream emcap-data-stream (pathname-name input-filepath) sample-rate)))

(defun expectancies-at-times-in-file (input-filepath output-filepath &key (sample-rate 200.0d0))
  "Computes the expectancies at each onset in the file from the discrete onset times"
  (let* ((times-as-rhythm (rhythm-of-emcap-onset-file input-filepath sample-rate))
	 (expectancies-at-times (expectancies-of-rhythm times-as-rhythm :time-limit-expectancies nil)))
    (with-open-file (expectancy-file output-filepath :direction :output :if-exists :supersede)
      (write-expectancies-to-stream expectancies-at-times sample-rate expectancy-file))))

(defun last-expectancy-of-file (input-filepath output-filepath 
				&key (sample-rate 200.0d0)
				(expectancies-generator #'expectancies-of-rhythm-ridge-persistency))
  "Computes the expectancies at the last moment in the file from the discrete onset times"
  (format t "Using ~a~%" expectancies-generator)
  (let* ((times-as-rhythm (rhythm-of-emcap-onset-file input-filepath sample-rate))
	 ;; (last-time (last-onset-time times-as-rhythm))  ; take either last onset
	 (last-time (1- (duration-in-samples times-as-rhythm)))	; or last moment of window.
	 (expectancies-at-last-time (funcall expectancies-generator times-as-rhythm
					     :times-to-check (list last-time)
					     ;; :phase-correct-from (last-onset-time times-as-rhythm))))
					     ;; Do no phase correction when testing metrical expectancies.
					     :phase-correct-from nil))
	 ;; Weight the expectation by absolute duration of the rhythm.
	 (duration-weighted-expectancies (absolute-duration-confidence expectancies-at-last-time times-as-rhythm)))
    (with-open-file (expectancy-file output-filepath :direction :output :if-exists :supersede)
      (write-expectancies-to-stream duration-weighted-expectancies sample-rate expectancy-file))))

(defun last-expectancy-of-salience (saliency-filepath onset-times-filepath output-filepath
				    &key (sample-rate 200.0d0)) 
  "Computes the expectancies at the last moment in the file from the perceptual salience trace"
  (let* ((times-as-rhythm (perceptual-salience-rhythm saliency-filepath onset-times-filepath :sample-rate sample-rate))
	 (expectancies-at-times (expectancies-of-rhythm times-as-rhythm)))
    (with-open-file (expectancy-file output-filepath :direction :output :if-exists :supersede)
      (write-expectancies-to-stream (last expectancies-at-times) sample-rate expectancy-file))))
