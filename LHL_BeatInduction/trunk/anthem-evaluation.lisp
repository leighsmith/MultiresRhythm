;;; LMS

(in-package :shoe)
(use-package :multires-rhythm)

;;; Calculate the tree directly from the anthem patterns.
(defparameter *anthem-tree* (make-patterns-tree multires-rhythm::*national-anthems* :mode :sequential-ratio))

;;; The older code would use a file, but really only for performance, which nowdays is unnecessary.
;; (defparameter *anthem-tree* (with-open-file (stream "/Volumes/iDisk/Research/Data/DesainHoning/anthems.tree") (read stream)))

;; Created from correct_fraction:make-correct-from-file.
(defun correct-from-tree-file (pattern t1 beat &key (allow-multiples t) (allow-sub-beats t))
  (format t "pattern ~a t1 ~a beat ~a~%" pattern t1 beat)
  (format t "normalized pattern ~a~%" (normalize pattern :sequential-ratio))
  ;; TODO should test that the name of the country matches.
  (not (null (correct-tree-beat? *anthem-tree* (or pattern '(1)) (mod t1 beat) beat 
				 allow-multiples allow-sub-beats))))

;;; TODO used-part-of-pattern
;;; (correct-tree-beat? anthem-tree '() 0 1)

(defun evaluate-lhl82-bar-period-of-anthem (anthem)
  (let ((beat-track (lhl82 (second anthem) :debug #'print-state-debug :trace nil))) ; :max-beat 30
    (format t "~%Beat tracking ~a~%" (string (caar anthem)))
    ;; (format t "official beat period ~a~%" (multires-rhythm::anthem-beat-period anthem))
    (format t "official downbeat ~a official bar period ~a~%" 
	    (multires-rhythm::anthem-anacrusis-duration anthem)
	    (multires-rhythm::anthem-bar-duration anthem))
    (format t "computed downbeat ~a bar period ~a~%" (t1 beat-track) (beat beat-track))
    (= (beat beat-track) (multires-rhythm::anthem-bar-duration anthem))))

(defun evaluate-lhl82-downbeat-of-anthem (anthem)
  (let ((beat-track (lhl82 (second anthem) :debug #'print-state-debug :trace nil))) ; :max-beat 30
    (format t "~%Beat tracking ~a~%" (string (caar anthem)))
    (format t "official downbeat ~a~%" (multires-rhythm::anthem-anacrusis-duration anthem))
    (format t "computed downbeat ~a~%" (t1 beat-track))
    (= (t1 beat-track) (multires-rhythm::anthem-anacrusis-duration anthem))))

(defun evaluate-lhl82-anthem (anthem)
  (let* ((pattern (second anthem))
	 (beat-track (lhl82 pattern :debug #'print-state-debug :trace nil))) ; :max-beat 30
    (format t "~%Beat tracking ~a~%" (string (caar anthem)))
    (format t "official downbeat ~a official bar period ~a~%" 
	    (multires-rhythm::anthem-anacrusis-duration anthem)
	    (multires-rhythm::anthem-bar-duration anthem))
    (format t "computed downbeat ~a bar period ~a~%" (t1 beat-track) (beat beat-track))
    (format t "Strictly matches ~a~%"
	    (and (= (t1 beat-track) (multires-rhythm::anthem-anacrusis-duration anthem))
		 (= (beat beat-track) (multires-rhythm::anthem-bar-duration anthem))))
    (format t "Checking pattern ~a~%" pattern)
    ;; (correct-from-tree-file pattern (t1 beat-track) (beat beat-track))))
    (correct-from-tree-file pattern 
			    (multires-rhythm::anthem-anacrusis-duration anthem)
			    (multires-rhythm::anthem-bar-duration anthem))))

    ;; (correct-strict-pulse pattern (t1 beat-track) (beat beat-track)) ; from D&H99 for strict checking.
    ;; (format t "correct metric ~a~%" 
;;	    (correct-metric pattern (t1 beat-track) (beat beat-track)))
    ;; (correct-metric pattern (t1 beat-track) (beat beat-track))))


;; (setf anthem-tree-check (make-correct-from-file "/Volumes/iDisk/Research/Data/DesainHoning/anthems.tree"))
;; (funcall anthem-tree-check (second (multires-rhythm::anthem-named 'multires-rhythm::vietnam)) 5 16)

;;; (evaluate-lhl82-bar-period-of-anthem (multires-rhythm::anthem-named 'multires-rhythm::yugoslavia))

;;; (correct-metric (second (multires-rhythm::anthem-named 'multires-rhythm::yugoslavia)) 0 12) => ?
;;; (check-metric-t1-beat (second (multires-rhythm::anthem-named 'multires-rhythm::yugoslavia)) 0 12 '(12 16 18 24) '(2 3))
;;; (correct-strict-pulse (second (multires-rhythm::anthem-named 'multires-rhythm::yugoslavia)) 0 12) => T
;;; (metric? (second (multires-rhythm::anthem-named 'multires-rhythm::yugoslavia)) 12 '(3 2 2))

;;; (correct-metric (second (multires-rhythm::anthem-named 'multires-rhythm::vietnam)) 5 16) => ?
;;; (check-metric-t1-beat (second (multires-rhythm::anthem-named 'multires-rhythm::vietnam)) 5 16 '(12 16 18 24) '(2 3)) => ?

;;; (check-metric-t1-beat (second (multires-rhythm::anthem-named 'multires-rhythm::yemen)) 6 24 '(12 16 18 24) '(2 3))
;;; (correct-strict-pulse (second (multires-rhythm::anthem-named 'multires-rhythm::yemen)) 6 24) => T

;;; (setf pattern (second (multires-rhythm::anthem-named 'multires-rhythm::australia)))

;;; (evaluate-lhl82-anthem (multires-rhythm::anthem-named 'multires-rhythm::yugoslavia))
;;; (evaluate-lhl82-anthem (multires-rhythm::anthem-named 'multires-rhythm::australia))
;;; (evaluate-lhl82-anthem (multires-rhythm::anthem-named 'multires-rhythm::america))
;;; (evaluate-lhl82-anthem (multires-rhythm::anthem-named 'multires-rhythm::vietnam))
;;; (evaluate-lhl82-anthem (multires-rhythm::anthem-named 'multires-rhythm::argentine))
;;; (evaluate-lhl82-anthem (multires-rhythm::anthem-named 'multires-rhythm::yemen))
;;; (evaluate-lhl82-anthem (multires-rhythm::anthem-named 'multires-rhythm::vietnam))

;;; (setf bad-bar-periods (multires-rhythm::evaluation-of-anthems #'evaluate-lhl82-bar-period-of-anthem))
;;; (setf bad-downbeats (multires-rhythm::evaluation-of-anthems #'evaluate-lhl82-downbeat-of-anthem))
;;; (setf bad-both (multires-rhythm::evaluation-of-anthems #'evaluate-lhl82-anthem))
