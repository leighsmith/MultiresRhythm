;;;; $Id$
;;;; 
;;;; Comparisons of models of complexity with Shmulevich and Povels reported complexity
;;;; listener judgements.
;;;;

(in-package :syncopation)
(shadow 'rhythm)
(use-package :nlisp)
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

(defparameter *sorted-shmulevich-patterns* (sort *shmulevich-patterns* #'< :key #'first))

(defun scale-to-shmulevich-ratings (list)
  "Scale down to 1-5 matching the users ratings in Shmulevich's data."
  (let ((max-value (apply #'max list)))
    (mapcar (lambda (x) (1+ (* (/ x max-value) 4.0))) list)))

(defun syncopation-of-pattern (pattern meter metric-salience)
  "Assumes metrical alignment of the pattern against the meter"
  (let ((syncopation-list (calculate-syncopations-on-grid pattern meter metric-salience)))
    (format t "Syncopations (~{~,2f~^ ~})~%" syncopation-list)
    (apply #'+ syncopation-list)))

;;; Perhaps the model should be the first local minima of the shifted meter, i.e the first
;;; minimal syncopation that presents itself. But this doesn't explain rhythms whose
;;; syncopation measure falls below human judgement of complexity. If syncopation is
;;; contributing to that complexity, then listeners must not be completely minimising the syncopation.
(defun first-minimum-syncopation (pattern meter metric-salience)
  "Calculates the syncopation of the pattern when rotated across each tatum (minimum metrical unit) left."
  (loop ; over all pattern rotations
     for shifted from 0 below (length pattern)
     for syncopations-of-pattern = (calculate-syncopations-on-grid pattern meter metric-salience)
     for syncopation-measure-of-pattern = (apply #'+ syncopations-of-pattern)
     with minimum-syncopation = 99 ; should be syncopation-measure-of-pattern
     while (< syncopation-measure-of-pattern minimum-syncopation)
     do
       (format t "Rotated Rhythm ~A Syncopation Measures: " pattern)
       (format t "~,2f (~{~,2f~^ ~})~%" syncopation-measure-of-pattern syncopations-of-pattern)
       (setf pattern (append (rest pattern) (list (first pattern)))) ; rotate the pattern
       (setf minimum-syncopation syncopation-measure-of-pattern)
     finally (return minimum-syncopation)))

(defun minimised-syncopation-by-event-shifting (pattern meter metric-salience)
  "Calculates the syncopation of the pattern when rotated across each tatum (minimum metrical unit) left."
  (loop ; over all pattern rotations
     for shifted from 0 below (length pattern)
     with syncopations-of-pattern
     with syncopation-measure-of-pattern
     do
       (setf syncopations-of-pattern (calculate-syncopations-on-grid pattern meter metric-salience))
       (setf syncopation-measure-of-pattern (apply #'+ syncopations-of-pattern))
       (format t "Rotated Rhythm ~A Syncopation Measures: " pattern)
       (format t "~,2f (~{~,2f~^ ~})~%" syncopation-measure-of-pattern syncopations-of-pattern)
       (setf pattern (append (rest pattern) (list (first pattern)))) ; rotate the pattern
     minimizing syncopation-measure-of-pattern))

;;; Very curious! A summation of syncopation measures across all rotations always adds to
;;; 90 when using the meter (2 2 2 2)?! This is probably because the meter establishes the
;;; maximum syncopation value and then the rotation guarantees the syncopation will be
;;; found together with all other possible values.
(defun averaged-syncopation-of-event-shifting (pattern meter metric-salience)
  "Calculates the syncopation of the pattern when rotated across each tatum (minimum metrical unit) left."
  (loop ; over all pattern rotations
     for shifted from 0 below (length pattern)
     with syncopations-of-pattern
     with syncopation-measure-of-pattern
     do
       (setf syncopations-of-pattern (calculate-syncopations-on-grid pattern meter metric-salience))
       (setf syncopation-measure-of-pattern (apply #'+ syncopations-of-pattern))
       (format t "Rotated Rhythm ~A Syncopation Measures: " pattern)
       (format t "~,2f (~{~,2f~^ ~})~%" syncopation-measure-of-pattern syncopations-of-pattern)
       (setf pattern (append (rest pattern) (list (first pattern)))) ; rotate the pattern
     summing syncopation-measure-of-pattern into total-syncopation
     finally (return (/ total-syncopation (length pattern)))))

(defun syncopation-measures (rating-and-patterns meter metric-salience &key (sync-func #'syncopation-of-pattern))
  "Calculate and display syncopation measures"
  (loop ; over all Shmulevich patterns
     for (rating pattern) in rating-and-patterns
     for syncopation-measure-of-pattern = (funcall sync-func pattern meter metric-salience)
     do (format t "Rhythm ~A Rated ~,2f Syncopation Measure: ~,2f~%" pattern rating syncopation-measure-of-pattern)
     collect syncopation-measure-of-pattern into all-syncopation-measures
     finally (return (scale-to-shmulevich-ratings all-syncopation-measures))))

;;; (syncopation-measures *sorted-shmulevich-patterns* '(2 2 2 2) #'lh-metric-salience)
;;; (syncopation-measures *sorted-shmulevich-patterns* '(2 2 2 2) #'lh-metric-salience :sync-func #'minimised-syncopation-by-event-shifting)
;;; (syncopation-measures *sorted-shmulevich-patterns* '(2 2 2 2) #'lh-metric-salience :sync-func #'averaged-syncopation-of-event-shifting)
;;; (syncopation-measures *sorted-shmulevich-patterns* '(2 2 2 2) #'lh-metric-salience :sync-func #'first-minimum-syncopation)

(defun multires-analyse-pattern (pattern-name pattern &key (sample-rate 200))
  (let* ((test-rhythm (make-instance 'multires-rhythm:rhythm 
				     :name pattern-name
				     :description pattern-name ; TODO transliterate '-' for ' '.
				     :time-signal (rhythmic-grid-to-signal pattern :sample-rate sample-rate)
				     :sample-rate sample-rate)))
    ;; TODO repeat rhythm
    ;; (setf complexity (rhythm-complexity test-rhythm))
    (multires-rhythm:clap-to-rhythm test-rhythm)))

;; (multires-analyse-pattern "shmulevich-18" (second (nth 18 *sorted-shmulevich-patterns*)))

;; TODO needs to repeat 4 times?
(defun multires-pattern-complexity (pattern-name patterns &key (sample-rate 200))
  "Compute the complexity of the supplied rhythm patterns using MultiresRhythm"
  (loop ; over all Shmulevich patterns
     for pattern in patterns
     for pattern-number = 0 then (1+ pattern-number)
     for name = (format nil "~a-pattern-~a" pattern-name pattern-number)
     with test-rhythm
     with complexity
     do
       (setf test-rhythm 
	     (make-instance 'multires-rhythm:rhythm 
			    :name name
			    :description name ; TODO transliterate '-' for ' '.
			    :time-signal (rhythmic-grid-to-signal pattern :sample-rate sample-rate)
			    :sample-rate sample-rate))
       (setf complexity (rhythm-complexity test-rhythm))
       (format t "ridges of ~a is ~a~%" pattern complexity)
     collect complexity))

#|
;; Baseline metric analysis on an unsyncopated rhythm
(rhythm-complexity (make-instance 'multires-rhythm:rhythm 
				  :name "isochronous"
				  :description "isochronous rhythm"
				  :time-signal (rhythmic-grid-to-signal '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) :sample-rate 200)
				  :sample-rate 200))
|#

(defun syncopation-test (rating-and-patterns meter)
  (loop ; over the different metric salience methods.
     for metric-salience in (list #'lh-metric-salience #'pk-metric-salience #'pk-nonmuso-metric-salience)
     do
       (format t "~a: ~%" metric-salience)
     collect (nlisp::list-to-array (syncopation-measures rating-and-patterns meter metric-salience
;							:sync-func  #'first-minimum-syncopation
))))

(defun ntimes (atom-to-duplicate count)
  (cond ((> count 1) (cons atom-to-duplicate (ntimes atom-to-duplicate (1- count))))
	(t (list atom-to-duplicate))))

(defun plot-syncopation-comparisons (listener-rated-patterns meter &optional (figure-number 1))
  ;; Re-sort into increasing rating judgements.
  (let ((syncopation-test-results (syncopation-test listener-rated-patterns meter))
	(listener-ratings (nlisp::list-to-array (mapcar #'first listener-rated-patterns))))
    (window 0)
    (plot listener-ratings nil :label "Shmulevich Ratings" :style "linespoints linewidth 2")
    (window figure-number)
    (plot-command "set key left")
    (nplot (cons listener-ratings syncopation-test-results) nil
	:legends '("Listener Ratings" "Longuet-Higgins & Lee Measure" "P&K Measure (Musicians)" "P&K (Non-musicians)")
	:styles (ntimes "linespoints linewidth 2" 4)
	:title "Comparison of Psychological and Modelled Syncopation Measures"
	:reset nil)))

(defun plot-test-results (the-window test-results legends title)
    (window the-window)
    (plot-command "set style line 1 linetype 5")
    (plot-command "set title font \"Times,24\"")
    (plot-command "set xlabel font \"Times,24\"")
    (plot-command "set ylabel font \"Times,24\"")
    (plot-command "set key left")
    (nplot test-results (.iseq 0 (1- (.array-total-size (car test-results))))
	  :legends legends
	  :xlabel "Pattern number"
	  :ylabel "Complexity"
	  :styles '("linespoints linewidth 2 pointsize 1.3"
		    "linespoints linetype 9 linewidth 2 pointsize 1.3"
		    "linespoints linewidth 2 pointsize 1.3"
		    "linespoints linewidth 2 pointsize 1.3")
	  :title title
	  :aspect-ratio 0.66
	  :reset nil))

;; (ntimes "linespoints linewidth 2" 4)

(defun multiplot-syncopation-comparisons (listener-rated-patterns meter)
  ;; Re-sort into increasing rating judgements.
  (let ((syncopation-test-results (syncopation-test listener-rated-patterns meter))
	(listener-ratings (nlisp::list-to-array (mapcar #'first listener-rated-patterns))))
    (plot-test-results 0 syncopation-test-results 
		       '("Longuet-Higgins & Lee Measure" "P&K Measure (Musicians)" "P&K (Non-musicians)")
		       "Comparison of Modelled Syncopation Measures")
    (plot-test-results 1 (list listener-ratings (first syncopation-test-results))
	  '("Listener Ratings" "Longuet-Higgins & Lee Measure")
	  "Comparison of Listener Ratings to LH&L Syncopation Measures")
    (plot-test-results 2 (cons listener-ratings (rest syncopation-test-results))
	  '("Listener Ratings" "P&K Measure (Musicians)" "P&K (Non-musicians)")
	  "Comparison of Listener Ratings to P&K Syncopation Measures")))

(defun rmse (observations model)
  "Produces a Root-Mean-Squared-Error measure between the two data sets"
  (sqrt (/ (loop
	      for data-value in observations
	      for model-value in model
	      sum (expt (- model-value data-value) 2)) (length observations))))

;; Doing it in nlisp: 
;; (sqrt (/ (.sum (.expt (.- (.iseq 1 10) (.iseq 4 14)) 2)) (length observations)))


; (plot-syncopation-comparisons *shmulevich-patterns* '(2 2 2 2))
; (plot-syncopation-comparisons *sorted-shmulevich-patterns* '(2 2 2 2))
; (multiplot-syncopation-comparisons *sorted-shmulevich-patterns* '(2 2 2 2))

;;; "Just the patterns, Ma'am"...
; (multires-pattern-complexity "shmulevich" (mapcar #'second *shmulevich-patterns*))

;; (setf res (syncopation-test *sorted-shmulevich-patterns* '(2 2 2 2)))
;; (rmse (mapcar #'first *sorted-shmulevich-patterns*) (first res))
;; 0.7488189
;; (rmse (mapcar #'first *sorted-shmulevich-patterns*) (second res))   
;; 0.76126724
;; (rmse (mapcar #'first *sorted-shmulevich-patterns*) (third res))
;; 1.0032296


;;; TODO 
;;; Reduce each pattern to 12 beats in order to test against triplet meters. Drop the last
;;; 4 beats but then re-instate the terminating beat, leaving 13 beats.

;;; To listen:
(defun play-shmulevich-pattern (pattern-number &key (repeat 1))
  (let* ((pattern (second (nth pattern-number *sorted-shmulevich-patterns*)))
	 (terminated-pattern (append (repeat-rhythm pattern repeat) '(1))))
    (format t "Playing Rhythm: ~a~%" pattern)
    (play-rhythm (onsets-to-iois (grid-to-onsets terminated-pattern))
		 :ioi 200)))

;; (play-shmulevich-pattern 2 :repeat 4)
 
;; (display-best-c-scores *sorted-shmulevich-patterns* 4.0)

;; (plot-c-score-comparisons *sorted-shmulevich-patterns* 4.0)

;; The interesting rhythms
; (play-shmulevich-pattern 5 :repeat 4)
; (play-shmulevich-pattern 6 :repeat 4)
; (play-shmulevich-pattern 18 :repeat 4)
; (play-shmulevich-pattern 24 :repeat 4)

(defun save-shmulevich-pattern (pattern-number &key (repeat 1))
  (let* ((pattern (second (nth pattern-number *sorted-shmulevich-patterns*)))
	 (terminated-pattern (append (repeat-rhythm pattern repeat) (list 1))))
    (format t "Saving Rhythm: ~a~%" pattern)
    (save-scorefile (format nil "/Users/leigh/shmulevich_~d.score" pattern-number) 
		(mapcar (lambda (x) (* x 0.2)) (onsets-to-iois (grid-to-onsets terminated-pattern)))
		:instrument "midi"
		:midi-channel 10
		:description (format nil "Shmulevich ~d" pattern-number))))

; (save-shmulevich-pattern 4 :repeat 4)
; (save-shmulevich-pattern 5 :repeat 4)
; (save-shmulevich-pattern 6 :repeat 4)
; (save-shmulevich-pattern 18 :repeat 4)
; (save-shmulevich-pattern 24 :repeat 4)
; (save-shmulevich-pattern 33 :repeat 4)

;;; (setf test-pattern (second (nth 18 *sorted-shmulevich-patterns*)))
;;; (calculate-syncopations-on-grid test-pattern '(2 2 2 2) #'lh-metric-salience) -> (3 2 4 0)
;;; (minimised-syncopation-by-event-shifting (second (nth 18 *sorted-shmulevich-patterns*)) '(2 2 2 2) #'lh-metric-salience)
;;; (first-minimum-syncopation (second (nth 18 *sorted-shmulevich-patterns*)) '(2 2 2 2) #'lh-metric-salience)
;;; (averaged-syncopation-of-event-shifting (second (nth 33 *sorted-shmulevich-patterns*)) '(2 2 2 2) #'lh-metric-salience)
