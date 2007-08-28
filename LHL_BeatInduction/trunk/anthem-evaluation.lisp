;;; LMS
(in-package :shoe)
(use-package :multires-rhythm)

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

(defun evaluate-lhl82-both-of-anthem (anthem)
  (let ((beat-track (lhl82 (second anthem) :debug #'print-state-debug :trace nil))) ; :max-beat 30
    (format t "~%Beat tracking ~a~%" (string (caar anthem)))
    (format t "official downbeat ~a official bar period ~a~%" 
	    (multires-rhythm::anthem-anacrusis-duration anthem)
	    (multires-rhythm::anthem-bar-duration anthem))
    (format t "computed downbeat ~a bar period ~a~%" (t1 beat-track) (beat beat-track))
    (and (= (t1 beat-track) (multires-rhythm::anthem-anacrusis-duration anthem))
	 (= (beat beat-track) (multires-rhythm::anthem-bar-duration anthem)))))


;;; (evaluate-lhl82-bar-period-of-anthem (multires-rhythm::anthem-named 'multires-rhythm::yugoslavia))
;;; (setf bad-bar-periods (multires-rhythm::evaluation-of-anthems #'evaluate-lhl82-bar-period-of-anthem))
;;; (setf bad-downbeats (multires-rhythm::evaluation-of-anthems #'evaluate-lhl82-downbeat-of-anthem))
;;; (setf bad-both (multires-rhythm::evaluation-of-anthems #'evaluate-lhl82-both-of-anthem))
