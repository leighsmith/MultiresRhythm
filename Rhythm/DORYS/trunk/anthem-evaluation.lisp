;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for testing using National Anthem data-base.
;;;;
;;;; In nlisp (Matlab-like Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006
;;;;
;;;; See 
;;;;   author =  {Leigh M. Smith},
;;;;   title =   {A Multiresolution Time-Frequency Analysis and Interpretation of Musical Rhythm},
;;;;   school =  {Department of Computer Science, University of Western Australia},
;;;;   year =    1999,
;;;;   month =   {June},
;;;;   annote =  {\url{http://www.leighsmith.com/Research/Papers/MultiresRhythm.pdf}}
;;;;

(in-package :dorys)
(use-package :shoe)

;;; Calculate the tree directly from the anthem patterns.
(defparameter *anthem-tree* (shoe::make-patterns-tree *national-anthems* :mode :sequential-ratio))

;;; The older code would use a file, but really only for performance, which nowdays is unnecessary.
;; (defparameter *anthem-tree* (with-open-file (stream "/Volumes/iDisk/Research/Data/DesainHoning/anthems.tree") (read stream)))

;; Created from correct_fraction:make-correct-from-file.
(defun correct-from-anthem-tree (pattern t1 beat &key (allow-multiples t) (allow-sub-beats t))
  ;; (format t "pattern ~a t1 ~a beat ~a~%" pattern t1 beat)
  ;; (format t "normalized pattern ~a~%" (normalize pattern :sequential-ratio))
  ;; TODO should test that the name of the country matches.
  (not (null (shoe::correct-tree-beat? *anthem-tree* (or pattern '(1)) (mod t1 beat) beat 
				 allow-multiples allow-sub-beats))))

;;; TODO used-part-of-pattern
;;; (correct-tree-beat? anthem-tree '() 0 1)

(defun evaluate-lhl82-bar-period-of-anthem (anthem)
  (let ((beat-track (lhl82 (second anthem) :debug #'print-state-debug :trace nil))) ; :max-beat 30
    (format t "~%Beat tracking ~a~%" (string (caar anthem)))
    ;; (format t "official beat period ~a~%" (anthem-beat-duration anthem))
    (format t "official downbeat ~a official bar period ~a~%" 
	    (anthem-anacrusis-duration anthem)
	    (anthem-bar-duration anthem))
    (format t "computed downbeat ~a bar period ~a~%" (t1 beat-track) (beat beat-track))
    (= (beat beat-track) (anthem-bar-duration anthem))))

;;; (setf bad-bar-periods (evaluation-of-anthems #'evaluate-lhl82-bar-period-of-anthem))
;;; #<FUNCTION EVALUATE-LHL82-BAR-PERIOD-OF-ANTHEM> 62 failed, correct 40.952377%

(defun evaluate-lhl82-beat-period-of-anthem (anthem)
  (let ((beat-track (lhl82 (second anthem) :debug #'print-state-debug :trace nil))) ; :max-beat 30
    (format t "~%Beat tracking ~a~%" (string (caar anthem)))
    ;; (format t "official beat period ~a~%" (::anthem-beat-duration anthem))
    (format t "official downbeat ~a official beat period ~a~%" 
	    (anthem-anacrusis-duration anthem)
	    (anthem-beat-duration anthem))
    (format t "computed downbeat ~a beat period ~a~%" (t1 beat-track) (beat beat-track))
    (= (beat beat-track) (anthem-beat-duration anthem))))

;;; (setf bad-bar-periods (evaluation-of-anthems #'evaluate-lhl82-beat-period-of-anthem))

(defun evaluate-lhl82-downbeat-of-anthem (anthem)
  "Checks that the anthem tree checking works with the generated downbeat and the transcribed bar duration"
  (let* ((pattern (second anthem))
	 (beat-track (lhl82 pattern :debug #'print-state-debug :trace nil))) ; :max-beat 30
    (format t "~%Beat tracking ~a~%" (string (caar anthem)))
    (format t "official downbeat ~a official bar period ~a~%" 
	    (anthem-anacrusis-duration anthem)
	    (anthem-bar-duration anthem))
    (format t "computed downbeat ~a~%" (t1 beat-track))
    (format t "Strictly matches ~a~%" (= (t1 beat-track) (anthem-anacrusis-duration anthem)))
    (correct-from-anthem-tree pattern 
			      (t1 beat-track)
			      (anthem-bar-duration anthem))))

;;; (setf bad-downbeats (evaluation-of-anthems #'evaluate-lhl82-downbeat-of-anthem))
;;; #<FUNCTION EVALUATE-LHL82-DOWNBEAT-OF-ANTHEM> 29 failed, correct 72.38095%

(defun evaluate-lhl82-anthem (anthem)
  (let* ((pattern (second anthem))
	 (beat-track (lhl82 pattern :debug #'print-state-debug :trace nil))) ; :max-beat 30
    (format t "~%Beat tracking ~a~%" (string (caar anthem)))
    (format t "official downbeat ~a official bar period ~a~%" 
	    (anthem-anacrusis-duration anthem)
	    (anthem-bar-duration anthem))
    (format t "computed downbeat ~a bar period ~a~%" (t1 beat-track) (beat beat-track))
    (format t "Strictly matches ~a~%"
	    (and (= (t1 beat-track) (anthem-anacrusis-duration anthem))
		 (= (beat beat-track) (anthem-bar-duration anthem))))
    (format t "Checking pattern ~a~%" pattern)
    (correct-from-anthem-tree pattern (t1 beat-track) (beat beat-track))))

;;; (setf bad-both (evaluation-of-anthems #'evaluate-lhl82-anthem))
;;; #<FUNCTION EVALUATE-LHL82-ANTHEM> 38 failed, correct 63.809525%

;; (correct-strict-pulse pattern (t1 beat-track) (beat beat-track)) ; from D&H99 for strict checking.
;; (format t "correct metric ~a~%" 
;; (correct-metric pattern (t1 beat-track) (beat beat-track)))
;; (correct-metric pattern (t1 beat-track) (beat beat-track))))

(defun verify-lhl82-anthem (anthem)
  "Checks that the anthem tree checking works with the transcribed (not generated)
  downbeat and bar duration"
  (let* ((pattern (second anthem)))
    (format t "official downbeat ~a official bar period ~a~%" 
	    (anthem-anacrusis-duration anthem)
	    (anthem-bar-duration anthem))
    (format t "Checking pattern ~a with transcribed beat duration and phase~%" pattern)
    (correct-from-anthem-tree pattern 
			      (anthem-anacrusis-duration anthem)
			      (anthem-bar-duration anthem))))

;;; (setf none-bad (evaluation-of-anthems #'verify-lhl82-anthem))
;;; #<FUNCTION VERIFY-LHL82-ANTHEM> 0 failed, correct 100.0%


;;;; These are old and crufty tests:
;; (setf anthem-tree-check (make-correct-from-file "/Volumes/iDisk/Research/Data/DesainHoning/anthems.tree"))
;; (funcall anthem-tree-check (second (anthem-named 'vietnam)) 5 16)

;;; (evaluate-lhl82-bar-period-of-anthem (anthem-named 'yugoslavia))

;;; (correct-metric (second (anthem-named 'yugoslavia)) 0 12) => ?
;;; (check-metric-t1-beat (second (anthem-named 'yugoslavia)) 0 12 '(12 16 18 24) '(2 3))
;;; (correct-strict-pulse (second (anthem-named 'yugoslavia)) 0 12) => T
;;; (metric? (second (anthem-named 'yugoslavia)) 12 '(3 2 2))

;;; (correct-metric (second (anthem-named 'vietnam)) 5 16) => ?
;;; (check-metric-t1-beat (second (anthem-named 'vietnam)) 5 16 '(12 16 18 24) '(2 3)) => ?

;;; (check-metric-t1-beat (second (anthem-named 'yemen)) 6 24 '(12 16 18 24) '(2 3))
;;; (correct-strict-pulse (second (anthem-named 'yemen)) 6 24) => T

;;; (setf pattern (second (anthem-named 'australia)))

;;; (evaluate-lhl82-anthem (anthem-named 'yugoslavia))
;;; (evaluate-lhl82-anthem (anthem-named 'australia))
;;; (evaluate-lhl82-anthem (anthem-named 'america))

