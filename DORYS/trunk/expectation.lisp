;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for evaluating generated expectation points from a given set of event times.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2009
;;;;
;;;; Evaluate by expanding the window of beats fed in, and compare whether the expectation
;;;; returned matches an annotated beat.
;;;;
(in-package :dorys)
(use-package :nlisp)
(use-package :multires-rhythm)

(defun eval-expectation-subset (full-rhythm subset-duration &key (prev-expectations) (precision-window 0.050d0))
  "Evaluate the expectations of a given subset of the rhythm"
  (let* ((onset-times (onsets-in-samples full-rhythm))
	 (sample-rate (sample-rate full-rhythm))
	 (window-in-samples (* precision-window sample-rate))
	 (subset-in-samples (floor (* subset-duration sample-rate)))
	 (subset-rhythm (mrr::subset-of-rhythm full-rhythm (list 0 subset-in-samples)))
	 ;; Compare both, and check if we need the absolute-duration-confidence weighting
	 ;; for expectations = (mrr::last-onset-expectations subset-rhythm)
	 (expectations (mrr::last-expectations subset-rhythm))
	 (expected-times (make-narray (mapcar #'mrr::expected-time expectations)))
	 ;; Limit the times compared to those between the last of the expected and the end of the window.
	 (compare-times (mrr::prune-outliers onset-times 
					     :lower-limit subset-in-samples
					     :upper-limit (+ (.max expected-times) window-in-samples))))
    (mrr::plot-expectations+rhythm subset-rhythm (append prev-expectations expectations))
    (format t "expected-times ~a~%compare-times ~a~%" expected-times compare-times)
    (list 
     (if (zerop (.length compare-times))
	 '(0.0 0.0 0.0d0)
	 (evaluate-beat-times expected-times compare-times window-in-samples))
     expectations)))

;;; Should compare against the rhythm times and the annotated beat times.
(defun evaluate-expectancy (full-rhythm &key 
			    (precision-window 0.050d0)
			    (expectation-increment 1.0d0)
			    (minimum-period 5.0d0)) ; in seconds, TODO This should probably be based on number of events.

  "Given a set of times in a rhythm, produce an evaluation as to how well an expected event is actually then played"
  (loop
     with onset-times = (mrr::onsets-in-seconds full-rhythm)
     with last-onset = (.last onset-times)
     with onsets-in-expectation-range = (mrr::prune-outliers onset-times 
							     :lower-limit minimum-period
							     :upper-limit (+ last-onset precision-window))
     ;; TODO should be incrementing by event times.
     ;; We loop up until the last period of the rhythm, so we are generating no
     ;; last expectations from the entire rhythm, only those expectations from the
     ;; penultimate period that exceeds the last onset of the rhythm.
     for subset-duration from minimum-period below last-onset by expectation-increment
     for (subset-evaluation expectations) = (eval-expectation-subset full-rhythm subset-duration 
								     :prev-expectations all-expectations
								     :precision-window precision-window)
     append expectations into all-expectations
     collect subset-evaluation into evaluations
     finally (mrr::plot-expectations+rhythm full-rhythm all-expectations)
     finally (format t "onsets-in-expectation-range ~a~%" onsets-in-expectation-range)
     ;; finally (format t "per segment: ~a~%" (make-narray evaluations))
     finally (return (evaluate-beat-times (make-narray (mapcar #'mrr::expected-time-seconds all-expectations))
					  onsets-in-expectation-range
					  precision-window))))

;;; Should compare against the rhythm times and the annotated beat times.
(defun evaluate-expectancy-old (rhythm-times &key 
			    (sample-rate 200.0d0)
			    (precision-window 0.050d0)
			    (title "anonymous rhythm") 
			    (minimum-period 5.0d0)) ; in seconds
  "Given a set of times in a rhythm, produce an evaluation as to how well an expected event is actually then played"
  (loop
     with onset-times = (.floor (.* rhythm-times sample-rate)) ; in samples
     with full-rhythm = (rhythm-of-onsets title rhythm-times)
     with minimum-duration = (floor (* minimum-period sample-rate)) ; TODO This should probably be based on number of events.
     with window-in-samples = (* precision-window sample-rate)
     with onsets-in-expectation-range = (mrr::prune-outliers onset-times 
							     :lower-limit minimum-duration
							     :upper-limit (+ (.last onset-times) window-in-samples))
     ;; TODO should be incrementing by event times.
     ;; We loop up until the last period of the rhythm, so we are generating no
     ;; last expectations from the entire rhythm, only those expectations from the
     ;; penultimate period that exceeds the last onset of the rhythm.
     for subset-duration from minimum-duration below (duration-in-samples full-rhythm) by (floor (* 1.0d0 sample-rate))
     for subset-rhythm = (mrr::subset-of-rhythm full-rhythm (list 0 (floor subset-duration)))
     ;; Compare both, and check if we need the absolute-duration-confidence weighting
     ;; for expectations = (mrr::last-onset-expectations subset-rhythm)
     for expectations = (mrr::last-expectations subset-rhythm)
     for expected-times = (make-narray (mapcar #'mrr::expected-time expectations))
     ;; Limit the times compared to those between the last of the expected and the end of the window.
     for compare-times = (mrr::prune-outliers onset-times 
					      :lower-limit subset-duration
					      :upper-limit (+ (.max expected-times) window-in-samples))
     append expectations into all-expectations
     do
       (mrr::plot-expectations+rhythm subset-rhythm all-expectations)
       (format t "expected-times ~a~%compare-times ~a~%" expected-times compare-times)
     ;; collect subset-duration into expectation-times
     ;; use with (make-narray (list expectation-times evaluations))
     unless (zerop (.length compare-times))
     collect (evaluate-beat-times expected-times compare-times window-in-samples) into evaluations
     finally (mrr::plot-expectations+rhythm full-rhythm all-expectations)
     finally (format t "onsets-in-expectation-range ~a~%" (./ onsets-in-expectation-range
							      (float sample-rate)))
     finally (progn
	       (setf *print-truncated-array-limit* 150)
	       (format t "all expectations ~a~%" (./ (make-narray (mapcar #'mrr::expected-time all-expectations)) 
						     sample-rate))
	       (setf *print-truncated-array-limit* 100))
     finally (format t "per segment: ~a~%" (make-narray evaluations))
     finally (return (evaluate-beat-times (make-narray (mapcar #'mrr::expected-time all-expectations))
					  onsets-in-expectation-range
					  window-in-samples))))

(defun eval-essen-expectation (filename)
  (format t "evaluation against all expectations: ~a~%"
	  (evaluate-expectancy (essen-rhythm filename) :minimum-period 3.0d0)))

;; (eval-essen-expectation "danmark3")
;; (eval-essen-expectation "italia04")
;; (eval-essen-expectation "elsass45")

(defun eval-expectation-of-times (rhythm-times &key (title "anonymous rhythm"))
  (format t "evaluation against all expectations: ~a~%"
	  (evaluate-expectancy (mrr::rhythm-of-onsets title rhythm-times :sample-rate 200))))

