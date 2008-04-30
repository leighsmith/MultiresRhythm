;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for testing using National Anthem data-base.
;;;;
;;;; In Common Lisp
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006
;;;;

(in-package :dorys)

;;; The *national-anthems* symbol is interned into :dorys package by loading.

(defun keyword-access (list keyword)
  (nth (1+ (position keyword list)) list))

(defmacro anthem-description (anthem)
  `(first ,anthem))

(defmacro anthem# (anthem-number)
  "Retrieves the anthem by number"
  `(nth ,anthem-number *national-anthems*))

;;; Made this a function so it can be exported and used
(defun anthem-named (anthem-symbol)
  (find anthem-symbol *national-anthems* :key #'caar :test #'eq))

(defmacro anthem-name (anthem)
  `(first (anthem-description ,anthem)))

(defmacro anthem-bar-duration (anthem)
  `(seventh (anthem-description ,anthem)))

(defmacro anthem-beat-duration (anthem)
  `(fifth (anthem-description ,anthem)))

(defmacro meter-signature (anthem)
  `(second (anthem-description ,anthem)))

(defun meter-numerator (meter-string)
  (read-from-string meter-string :start 0 :end (position #\/ meter-string)))

(defun meter-numerator-of-anthem (anthem)
  (meter-numerator (meter-signature anthem)))

;;; :start-at = the anacrusis duration from the start of the bar.
;;; Defined as a function, rather than as a macro to allow it to be used as a key to remove-if-not.
(defun anthem-start-at (anthem)
  "Returns the position in the bar where the upbeat starts, in grid units"
  (ninth (anthem-description anthem)))

(defun anthem-anacrusis-duration (anthem)
  "Returns the number of grid units of time (not the number of notes), before the downbeat"
  (if (> (anthem-start-at anthem) 0)
      (- (anthem-bar-duration anthem) (anthem-start-at anthem))
      0))

(defun anthems-of-meter (meter)
  (remove-if-not (lambda (x) (equal x meter)) *national-anthems* :key #'cadar))

;; Count the meters in the database. Should match Zaanen et al.
;; (length (anthems-of-meter "4/4"))
;; (length (anthems-of-meter "12/8"))
;; (length (anthems-of-meter "2/4"))
;; (length (anthems-of-meter "3/4"))
;; (length (anthems-of-meter "2/2"))

(defun crochets-of-anthems (anthems)
  "Returns the crotchet ratios of the intervals sorted in ascending order"
  (let ((b '())) 
    (dolist (anthem anthems (reverse b))
      (push (sort (mapcar (lambda (x) (/ x (float (anthem-beat-duration anthem) 1d0))) (second anthem)) #'<)
	    b))))

(defun number-of-bars-in-anthem (anthem)
  "Returns the rounded number of bars in an anthem (including anacrusis)"
  (round (/ (reduce #'+ (second anthem)) (anthem-bar-duration anthem))))

(defun bar-period-in-anthem-intervals (&key (anthems *national-anthems*))
  "Returns the counts of the given anthem matching the bar duration"
  (loop
     for anthem in anthems
     do (format t "~a~%" (anthem-name anthem))
     collect (gethash (anthem-bar-duration anthem) (make-histogram (second anthem)))))

(defun make-histogram-of-anthem-intervals (&key (anthems *national-anthems*))
  "Returns a histogram of intervals in the anthems as ratios to the beat duration"
  (let ((interval-histogram (make-histogram '())))
    (dolist (anthem anthems)
      (add-to-histogram interval-histogram
			(mapcar (lambda (x) (/ x (float (anthem-beat-duration anthem) 1d0)))
				(second anthem))))
    interval-histogram))

;; (print-elements-and-counts (make-histogram-of-anthem-intervals :anthems (anthems-of-meter "3/4")))

;; (bar-period-in-anthem-intervals :anthems (anthems-of-meter "4/4"))

;; (/ (count-if #'numberp (bar-period-in-anthem-intervals)) (float (length *national-anthems*)))

(defun evaluation-of-anthems (evaluation-function &key (anthems *national-anthems*))
  "Applies the evaluation function to each anthem in turn, returning those that fail and
printing the proportion that pass."
  (let* ((failing-anthems (loop
			    for anthem in anthems
			    do (format t "Evaluating ~a~%" (anthem-name anthem))
			    when (not (print (funcall evaluation-function anthem)))
			    collect anthem))
	 (number-failed (length failing-anthems)))
    (format t "~a ~d failed, correct ~f%~%" 
	    evaluation-function 
	    number-failed
	    (* (- 1.0 (/ (float number-failed) (length anthems))) 100.0))
    failing-anthems))

