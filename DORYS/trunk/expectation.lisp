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

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun eval-expectation-subset (full-rhythm subset-window onset-times
				&key (prev-expectations) (precision-window 0.050d0))
  "Evaluate the expectations of a given subset of the rhythm"
  (let* ((sample-rate (sample-rate full-rhythm))
	 (window-in-samples (* precision-window sample-rate))
	 (subset-in-samples (mapcar (lambda (edge) (floor (* edge sample-rate))) subset-window))
	 (subset-rhythm (mrr::subset-of-rhythm full-rhythm subset-in-samples))
	 ;; Compare both, and check if we need the absolute-duration-confidence weighting
	 ;; (expectations (mrr::last-onset-expectations subset-rhythm))
	 (expectations (mrr::offset-expectation (mrr:last-expectations subset-rhythm) (first subset-in-samples))) 
	 ;; Test with no knowledge
	 ;; (expectations (mrr::random-expectations subset-rhythm full-rhythm))
	 (expected-times (make-narray (mapcar #'mrr::expected-time expectations)))
	 ;; Limit the times compared to those between the last of the expected and the end of the window.
	 (compare-times (mrr::prune-outliers onset-times 
					     :lower-limit (second subset-in-samples)
					     :upper-limit (+ (.max expected-times) window-in-samples))))
    ;; instead of diag-plot so we display on the same window, for each iteration.
    (if (find 'accumulative-expectations *plotting*) 
	(mrr::plot-expectations+rhythm subset-rhythm (append prev-expectations expectations)
				       :rhythm-starts-at (first subset-in-samples)))
    (format t "expected-times ~a~%compare-times ~a~%" expected-times compare-times)
    (list 
     (if (zerop (.length compare-times))
	 '(0.0 0.0 0.0d0)
	 (evaluate-beat-times expected-times compare-times window-in-samples))
     expectations)))

;;; Should compare against the rhythm times and the annotated beat times.
(defun evaluate-expectancy (full-rhythm &key 
			    (onset-times (mrr::onsets-in-seconds full-rhythm))
			    (precision-window 0.050d0)
			    (expectation-increment 1.0d0)
			    (minimum-period 5.0d0)) ; in seconds, TODO This should probably be based on number of events.

  "Given a set of times in a rhythm, produce an evaluation as to how well an expected event is actually then played"
  (loop
     with last-onset = (.last onset-times)
     with onsets-in-expectation-range = (mrr::prune-outliers onset-times 
							     :lower-limit minimum-period
							     :upper-limit (+ last-onset precision-window))
     ;; TODO should be incrementing by event times.
     ;; We loop up until the last period of the rhythm, so we are generating no
     ;; last expectations from the entire rhythm, only those expectations from the
     ;; penultimate period that exceeds the last onset of the rhythm.
     for subset-duration from minimum-period below last-onset by expectation-increment
     for (subset-evaluation expectations) = (eval-expectation-subset full-rhythm 
								     (list 0.0d0 subset-duration)
								     (onsets-in-samples full-rhythm)
								     :prev-expectations all-expectations
								     :precision-window precision-window)
     append expectations into all-expectations
     collect subset-evaluation into evaluations
     finally (mrr::plot-expectations+rhythm full-rhythm all-expectations)
     finally (format t "onsets-in-expectation-range ~a~%" onsets-in-expectation-range)
     ;; finally (format t "all expectations ~a~%" (mrr::.sort (make-narray (mapcar #'mrr::expected-time-seconds all-expectations))))
     ;; finally (format t "per segment: ~a~%" (make-narray evaluations))
     finally (return (evaluate-beat-times (make-narray (mapcar #'mrr::expected-time-seconds all-expectations))
					  onsets-in-expectation-range
					  precision-window))))

;;; TODO we can probably merge this with evaluate-expectancy by using :onset-times beat-times
(defun evaluate-expectancy-on-beat (odf-rhythm beat-times &key 
				    (precision-window 0.050d0)
				    (expectation-increment 1.0d0)
				    (maximum-period 8.0d0) ; in seconds
				    (minimum-period 5.0d0)) ; in seconds, TODO This should probably be based on number of events.

  "Given a set of times in a rhythm, produce an evaluation as to how well an expected event is actually then played"
  (loop
     with last-onset = (.last beat-times)
     with onsets-in-expectation-range = (mrr::prune-outliers beat-times 
							     :lower-limit minimum-period
							     :upper-limit (+ last-onset precision-window))
     ;; TODO should be incrementing by event times.
     ;; We loop up until the last period of the rhythm, so we are generating no
     ;; last expectations from the entire rhythm, only those expectations from the
     ;; penultimate period that exceeds the last onset of the rhythm.
     for window-end from minimum-period below last-onset by expectation-increment
     for window = (list (max 0.0d0 (- window-end maximum-period)) window-end)
     for (subset-evaluation expectations) = (eval-expectation-subset odf-rhythm 
								     window
								     (.* beat-times (sample-rate odf-rhythm))
								     :prev-expectations all-expectations
								     :precision-window precision-window)
     append expectations into all-expectations
     collect subset-evaluation into evaluations
     finally (mrr::plot-expectations+rhythm odf-rhythm all-expectations)
     finally (format t "for window ~a onsets-in-expectation-range ~a~%" window onsets-in-expectation-range)
     ;; finally (format t "all expectations ~a~%" (mrr::.sort (make-narray (mapcar #'mrr::expected-time-seconds all-expectations))))
     ;; finally (format t "per segment: ~a~%" (make-narray evaluations))
     finally (return (evaluate-beat-times (make-narray (mapcar #'mrr::expected-time-seconds all-expectations))
					  onsets-in-expectation-range
					  precision-window))))

(defun eval-essen-expectation (filename)
  (let ((score (evaluate-expectancy (essen-rhythm filename) :minimum-period 3.0d0)))
    (format t "evaluation against all expectations of ~a: ~a~%" filename (print-prf score))
    score))

(defun eval-expectation-of-times (rhythm-times &key (title "anonymous rhythm"))
  (format t "evaluation against all expectations: ~a~%"
	  (evaluate-expectancy (mrr::rhythm-of-onsets title rhythm-times :sample-rate 200))))

(defun eval-quaero-expectation (annotation-filepath)
  (let* ((odf-filepath (merge-pathnames
			(make-pathname :directory '(:relative "Quaero_Selection" "Analysis")
				       :name (pathname-name (pathname-name annotation-filepath))
				       :type "odf")
			*rhythm-data-directory*))
	 (odf-rhythm (rhythm-from-ircam-odf odf-filepath :sample-rate 172.27d0))
	 (beat-times (mrr::read-ircam-annotation annotation-filepath))
	 (score (evaluate-expectancy-on-beat odf-rhythm beat-times :minimum-period 3.0d0)))
    (format t "evaluation against all expectations of ~a: ~a~%" annotation-filepath (print-prf score))
    score))

;; (eval-quaero-expectation (first (no-hidden-files (cl-fad:list-directory *quaero-selection-annotations-directory*)))) 


(defun test-phase-offset (rhythm)
  "Tests where the expectancies fall with an inserted space at the start of the rhythm"
  (format t "rhythm ~a~%onsets at samples ~a~%" rhythm (mrr::onsets-in-samples rhythm))
  (mrr:last-expectations rhythm :expectancies-generator #'mrr::expectancies-of-rhythm-ridge-persistency) 
  (mrr:last-expectations (mrr::append-rhythm (random-offset 100) rhythm) ; shift the start of the rhythm
				      :expectancies-generator #'mrr::expectancies-of-rhythm-ridge-persistency))

(defun test-phase-correction (rhythm)
  (format t "rhythm ~a~%onsets at samples ~a~%" rhythm (mrr::onsets-in-samples rhythm))
  ;; trim the end of the rhythm
  (let ((trimmed-rhythm (mrr::limit-rhythm rhythm :maximum-samples (- (mrr::duration-in-samples rhythm) (random 100)))))
    (mrr:last-expectations rhythm :expectancies-generator #'mrr::expectancies-of-rhythm-ridge-persistency) 
    ;; With a limited rhythm, the phase is the same for a highly persistent ridge upto the
    ;; region which is trimmed. Actually the phase zero points match even on low
    ;; persistency ridges. The projection from the zero phase value of the limited rhythm won't necessarily
    ;; match that of the longer rhythm since the phase warps in and out over the extra
    ;; region. Need to test what the projection point would be if we used the phase value,
    ;; not chasing.
    (mrr:last-expectations trimmed-rhythm)))

(defun test-isorhythm-expectancy ()
  "This is the simplest case, testing what expectancies appear from an isochronous rhythm?"
  (let* ((iso-rhythm (rhythm-of-grid "Isochronous Rhythm" '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) :shortest-ioi 320))
	 (rhythm-offset (mrr::append-rhythm (random-offset 100) iso-rhythm)))
    ;; (pushnew 'weighted-beat-ridge *plotting*)
    ;; (pushnew 'scale-energy-profile *plotting*)
    ;; (pushnew 'unweighted-profile *plotting*)
    ;; (pushnew 'tempo-preference *plotting*)
    ;; Do no phase correction when testing metrical expectancies.
    ;; (expectancies-of-rhythm-integrator iso-rhythm 
    ;; 				       :times-to-check (list (1- (duration-in-samples iso-rhythm)))
    ;; 				       :phase-correct-from nil)
    (pushnew 'accumulative-expectations *plotting*)
    (evaluate-expectancy iso-rhythm :minimum-period 3.0d0)))

;; (test-phase-correction iso-rhythm)
;; (setf m (scaleogram-magnitude (scaleogram (analysis-of-rhythm iso-rhythm))))
;; (plot (.subseq (.column m 2048) 75 86) (.iseq 75 85) :aspect-ratio 0.66))

;; (pushnew 'accumulative-expectations *plotting*)
;; (eval-essen-expectation "danmark3")
;; (eval-essen-expectation "italia04")
;; (eval-essen-expectation "elsass45")

;;(defun eval-essen-rhythm (essen-performance)
;;  (> (eval-essen-expectation (first essen-performance)) 0.5d0))

(defun eval-all-perf-essen (corpus evaluation-function)
  (loop
     for track in corpus
     for scores-per-track = (funcall evaluation-function (first track))
     ;; TODO corpus-name, corpus precision-window
     do
       (format t "for ~a scores ~a~%" track (print-prf scores-per-track))
     collect scores-per-track into scores
     finally (return (mean-scores (make-narray scores)))))

;; (setf corpus-score (eval-all-perf-essen *essen-perf-meters* #'eval-essen-expectation))
;; (format t "~%for ~a mean scores ~a~%" corpus-name corpus-score)

;; Random is:
;; (0.17984628134853603d0 0.22395800061705326d0 0.19797422856095126d0)
;; (0.17229991298754976d0 0.21350901642117517d0 0.18943050419980734d0)
;; (0.17311589786391615d0 0.21034012163131482d0 0.18863627827819515d0)
;; (0.18561950446188197d0 0.2295361672086161d0  0.20378579632490562d0)

;;; (setf expectancy-set (eval-all-perf-essen performed-34 #'eval-essen-expectation))
;;; (setf all-expectations (reduce #'append expectancy-set))
;;; (cl-musickit:play-timed-notes (cl-musickit::note-list-of-part (part-of-melisma-file
;;; (make-pathname *essen-perform-directory* "deut0214.notes"))))

