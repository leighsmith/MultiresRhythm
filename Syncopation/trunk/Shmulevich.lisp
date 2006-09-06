;;; $Id: Shmulevich.lisp 4729 2006-03-24 21:28:27Z leigh $
;;;

(use-package :multires-rhythm)

;;; Taken from Shmulevich and Povel 2000, 35 patterns used in
;;; judgement of complexity experiment, together with the mean judged
;;; complexity measure on a scale between 1 = simple and 5 = complex.
;;; Each 1 represents a tone, 0 represents a rest, the smallest IOI is 200ms.
;;; These are arranged in order of Shmulevich's complexity calculations (which he
;;; conveniently didn't output).
;;;
(defparameter *shmulevich-patterns* 
  '((1.56 (1 1 1 1 1 0 0 1 1 0 1 0 1 0 0 0))
    (2.12 (1 1 1 0 1 0 1 1 1 0 0 1 1 0 0 0))
    (2.08 (1 0 1 1 1 0 1 1 1 0 0 1 1 0 0 0))
    (1.88 (1 0 1 0 1 1 1 1 1 0 0 1 1 0 0 0))
    (1.80 (1 0 0 1 1 0 1 0 1 1 1 1 1 0 0 0))
    (2.44 (1 1 1 0 1 1 1 0 1 1 0 0 1 0 0 0)) 
    (2.20 (1 0 1 1 1 1 0 1 1 0 0 1 1 0 0 0))
    (2.56 (1 1 0 0 1 1 1 1 1 0 1 0 1 0 0 0))
    (3.00 (1 1 0 0 1 0 1 1 1 0 1 1 1 0 0 0))
    (2.04 (1 0 1 1 1 0 1 1 1 1 0 0 1 0 0 0))
    (2.76 (1 1 1 0 1 1 0 0 1 1 0 1 1 0 0 0))
    (2.72 (1 1 0 1 1 1 1 0 1 0 0 1 1 0 0 0))
    (3.00 (1 1 0 1 1 0 1 1 1 1 0 0 1 0 0 0))
    (3.16 (1 1 0 0 1 1 0 1 1 0 1 1 1 0 0 0))
    (2.04 (1 0 0 1 1 1 0 1 1 1 0 1 1 0 0 0))
    (2.88 (1 1 0 1 1 1 1 0 1 1 0 0 1 0 0 0))
    (2.60 (1 1 0 1 1 1 0 1 1 1 0 0 1 0 0 0))
    (2.60 (1 1 0 1 1 1 0 0 1 1 0 1 1 0 0 0))
    (2.64 (1 1 0 0 1 1 0 1 1 1 1 0 1 0 0 0))
    (3.24 (1 1 0 0 1 1 0 1 1 1 1 0 1 0 0 0))
    (3.08 (1 1 1 1 1 0 1 1 0 1 0 0 1 0 0 0))
    (3.04 (1 1 1 1 0 1 0 0 1 1 1 0 1 0 0 0))
    (3.04 (1 1 1 0 0 1 1 0 1 1 1 0 1 0 0 0))
    (2.56 (1 0 1 1 1 0 0 1 0 1 1 1 1 0 0 0))
    (2.56 (1 0 1 0 0 1 1 1 1 0 1 1 1 0 0 0))
    (2.84 (1 1 1 1 0 1 0 1 0 0 1 1 1 0 0 0))
    (3.60 (1 1 0 1 1 1 0 1 0 0 1 1 1 0 0 0))
    (2.68 (1 1 0 1 0 0 1 1 1 0 1 1 1 0 0 0))
    (3.28 (1 0 1 1 1 1 0 1 0 0 1 1 1 0 0 0))
    (3.08 (1 0 0 1 1 1 1 1 0 1 1 0 1 0 0 0))
    (3.52 (1 1 1 1 0 1 1 1 0 0 1 0 1 0 0 0))
    (3.60 (1 1 1 1 0 0 1 1 0 1 1 0 1 0 0 0))
    (3.04 (1 1 0 1 1 1 1 0 0 1 1 0 1 0 0 0))
    (2.88 (1 1 0 1 0 0 1 1 1 1 1 0 1 0 0 0))
    (3.08 (1 0 1 0 0 1 1 1 0 1 1 1 1 0 0 0))))

(defun scale-to-shmulevich-ratings (list)
  "Scale down to 1-5 matching the users ratings in Shmulevich's data."
  (let ((max-value (apply #'max list)))
    (mapcar (lambda (x) (1+ (* (/ x max-value) 4.0))) list)))

(defun syncopation-measures (rating-and-patterns meter metric-salience)
  "Calculate and display syncopation measures"
  (loop ; over all Shmulevich patterns
     for (rating pattern) in rating-and-patterns
     for terminated-pattern = (append pattern '(1))
     with syncopation-list
     with syncopation-measure-of-pattern
     do
       (setf syncopation-list (calculate-syncopations-on-grid terminated-pattern meter metric-salience))
       (setf syncopation-measure-of-pattern (apply #'+ syncopation-list))
       (format t "Rhythm ~A Rated ~,2f Syncopation Measures: " terminated-pattern rating)
       (format t "(~{~,2f~^ ~}) ~,2f~%" syncopation-list syncopation-measure-of-pattern)
     collect syncopation-measure-of-pattern into all-syncopation-measures
     finally (return (scale-to-shmulevich-ratings all-syncopation-measures))))

;; TODO needs to repeat 4 times?
(defun multires-pattern-complexity (patterns)
  "Compute the complexity of the supplied rhythm patterns using MultiresRhythm"
  (loop ; over all Shmulevich patterns
     for pattern in patterns
     for terminated-pattern = (append pattern '(1))
     for pattern-number = 0 then (1+ pattern-number)
     for name = (format nil "shmulevich-pattern-~a" pattern-number)
     with test-rhythm
     with complexity
     do
       (setf test-rhythm 
	     (make-instance 'rhythm 
			    :name name
			    :description name ; TODO transliterate '-' for ' '.
			    :time-signal (rhythmic-grid-to-signal terminated-pattern :sample-rate 200)
			    :sample-rate 200))
       (setf complexity (rhythm-complexity test-rhythm))
       (format t "ridges of ~a is ~a~%" pattern complexity)
     collect complexity))

(defun syncopation-test (rating-and-patterns meter)
  (loop ; over the different metric salience methods.
     for metric-salience in (list #'lh-metric-salience #'pk-metric-salience #'pk-nonmuso-metric-salience)
     do
       (format t "~a: ~%" metric-salience)
     collect (syncopation-measures rating-and-patterns meter metric-salience)))

(defun plot-syncopation-comparisons (listener-rated-patterns meter &optional (figure-number 1))
  ;; Re-sort into increasing rating judgements.
  (let ((syncopation-test-results (syncopation-test listener-rated-patterns meter))
	(listener-ratings (mapcar #'first listener-rated-patterns)))
    (plot-initialise)
    (plot listener-ratings :labels '("Shmulevich Ratings") :figure-number 0)
    (plot-command "set key left")
    (plot (cons listener-ratings syncopation-test-results)
	:labels '("Listener Ratings" "Longuet-Higgins & Lee Measure" "P&K Measure (Musicians)" "P&K (Non-musicians)")
	:title "Comparison of Psychological and Modelled Syncopation Measures"
	:figure-number figure-number)))

(defun multiplot-syncopation-comparisons (listener-rated-patterns meter &optional (figure-number 1))
  ;; Re-sort into increasing rating judgements.
  (let ((syncopation-test-results (syncopation-test listener-rated-patterns meter))
	(listener-ratings (mapcar #'first listener-rated-patterns)))
    (plot-initialise)
    (plot-command "set style line 2 linetype 5")
    (plot-command "set title font \"Times,24\"")
    (plot-command "set xlabel \"Pattern number\" font \"Times,24\"")
    (plot-command "set ylabel \"Complexity\" font \"Times,24\"")
    (plot-command "set key left")
    (plot syncopation-test-results
	  :labels '("Longuet-Higgins & Lee Measure" "P&K Measure (Musicians)" "P&K (Non-musicians)")
	  :title "Comparison of Modelled Syncopation Measures"
	  :figure-number 0)
    (plot (list listener-ratings (first syncopation-test-results))
	  :labels '("Listener Ratings" "Longuet-Higgins & Lee Measure") 
	  :title "Comparison of Listener Ratings to LH&L Syncopation Measures"
	  :figure-number 1)
    (plot (cons listener-ratings (rest syncopation-test-results))
	  :labels '("Listener Ratings" "P&K Measure (Musicians)" "P&K (Non-musicians)")
	  :title "Comparison of Listener Ratings to P&K Syncopation Measures"
	  :figure-number 2)))


(defun rmse (observations model)
  "Produces a Root-Mean-Squared-Error measure between the two data sets"
  (sqrt (/ (loop
	      for data-value in observations
	      for model-value in model
	      sum (expt (- model-value data-value) 2)) (length observations))))

;; Doing it in nlisp: 
;; (sqrt (/ (.sum (.expt (.- (.iseq 1 10) (.iseq 4 14)) 2)) 10))


; (plot-syncopation-comparisons *shmulevich-patterns* '(2 2 2 2))
; (plot-syncopation-comparisons (sort *shmulevich-patterns* #'< :key #'first) '(2 2 2 2))
; (multiplot-syncopation-comparisons (sort *shmulevich-patterns* #'< :key #'first) '(2 2 2 2))

;;; "Just the patterns, Ma'am"...
; (multires-pattern-complexity (mapcar #'second *shmulevich-patterns*))

;; (setf res (syncopation-test (sort *shmulevich-patterns* #'< :key #'first) '(2 2 2 2)))
;; (rmse (mapcar #'first (sort *shmulevich-patterns* #'< :key #'first)) (first res))
;; 0.7488189
;; (rmse (mapcar #'first (sort *shmulevich-patterns* #'< :key #'first)) (second res))   
;; 0.76126724
;; (rmse (mapcar #'first (sort *shmulevich-patterns* #'< :key #'first)) (third res))
;; 1.0032296


;;; TODO 
;;; Reduce each pattern to 12 beats in order to test against triplet meters. Drop the last
;;; 4 beats but then re-instate the terminating beat, leaving 13 beats.

;;; To listen:
(defun play-shmulevich-pattern (pattern-number &key (repeat 1))
  (let* ((sorted-shmulevich-patterns (sort *shmulevich-patterns* #'< :key #'first))
	 (pattern (second (nth pattern-number sorted-shmulevich-patterns)))
	 (terminated-pattern (append (repeat-rhythm pattern repeat) '(1))))
    (format t "Playing Rhythm: ~a~%" pattern)
    (play-rhythm (onsets-to-iois (grid-to-onsets terminated-pattern))
		 :ioi 200)))

; (display-best-c-scores (sort *shmulevich-patterns* #'< :key #'first) 4.0)

; (plot-c-score-comparisons (sort *shmulevich-patterns* #'< :key #'first) 4.0)

;; The interesting rhythms
; (play-shmulevich-pattern 5 :repeat 4)
; (play-shmulevich-pattern 6 :repeat 4)
; (play-shmulevich-pattern 18 :repeat 4)
; (play-shmulevich-pattern 24 :repeat 4)

(defun save-shmulevich-pattern (pattern-number &key (repeat 1))
  (let* ((sorted-shmulevich-patterns (sort *shmulevich-patterns* #'< :key #'first))
	 (pattern (second (nth pattern-number sorted-shmulevich-patterns)))
	 (terminated-pattern (append (repeat-rhythm pattern repeat) (list 1))))
    (format t "Saving Rhythm: ~a~%" pattern)
    (save-score (format nil "/Users/leigh/shmulevich_~d.score" pattern-number) 
		(mapcar (lambda (x) (* x 0.2)) (onsets-to-iois (grid-to-onsets terminated-pattern)))
		:instrument "Midi"
		:description (format nil "Shmulevich ~d" pattern-number))))

