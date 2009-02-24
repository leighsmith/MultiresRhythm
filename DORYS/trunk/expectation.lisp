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

;;; Should compare against the rhythm times and the annotated beat times.
(defun evaluate-expectancy (rhythm-times &key (sample-rate 200.0d0) (precision-window 0.050d0)
			    (title "anonymous rhythm"))
  "Given a set of times in a rhythm, produce an evaluation as to how well an expected event is actually then played"
  (loop
     with onset-times = (.floor (.* rhythm-times sample-rate))
     with full-rhythm = (rhythm-of-onsets title rhythm-times)
     with minimum-duration = (* 5 sample-rate) ; TODO This should probably be based on number of events.
     with window-in-samples = (* precision-window sample-rate)
     ;; TODO should be incrementing by event times.
     for subset-duration from minimum-duration below (duration-in-samples full-rhythm) by (floor (* 1.0d0 sample-rate))
     for subset-rhythm = (mrr::subset-of-rhythm full-rhythm (list 0 (floor subset-duration)))
     ;; Compare both, and check if we need the absolute-duration-confidence weighting
     ;; (last-onset-expectations subset-rhythm) & (last-expectations subset-rhythm)))
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
     finally (return (make-narray evaluations))))
   
(defun eval-essen-expectation (filename)
  (let* ((rhythm (essen-rhythm filename)))
    (evaluate-expectancy (mrr::onsets-in-seconds rhythm) :title filename)))
    
;; (eval-essen-expectation "danmark3")
;; (eval-essen-expectation "italia04")
;; (eval-essen-expectation "elsass45")
