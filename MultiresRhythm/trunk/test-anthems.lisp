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

(in-package :multires-rhythm)
(use-package :nlisp)

;;; National Anthem data-base
;;; The *national-anthems* symbol is interned into :multires-rhythm package by loading.
(load "/Volumes/iDisk/Research/Data/DesainHoning/national anthems.lisp")

(defmacro anthem# (anthem-number)
  "Retrieves the anthem by number"
  `(nth ,anthem-number *national-anthems*))

(defmacro anthem-named (anthem-symbol)
  `(find ,anthem-symbol *national-anthems* :key #'caar :test #'eq))

(defun anthem-duration-in-samples (anthem duration &key (shortest-ioi 50))
  "Returns the duration in samples, normalized to equal tempo"
  (let ((min-duration (/ (* shortest-ioi 2.0) (fifth (first anthem)))))
    (first (intervals-in-samples (list duration) :ioi min-duration))))

(defun anthem-rhythm (anthem &key (shortest-ioi 50))
  (let* ((anthem-descriptor (first anthem))
	 (anthem-name (symbol-name (first anthem-descriptor)))
	 (min-duration (/ (* shortest-ioi 2.0) (fifth anthem-descriptor)))
	 (anthem-iois (second anthem)))
    (iois-to-rhythm anthem-name anthem-iois :shortest-ioi min-duration)))

(defmacro anthem-bar-duration (anthem)
  `(seventh (first ,anthem)))

;;; :start-at = the anacrusis duration from the start of the bar.
(defun anthem-start-at (anthem)
  (ninth (first anthem)))

(defun anthem-anacrusis (anthem)
  (- (anthem-bar-duration anthem) (anthem-start-at anthem)))

(defun bar-scale (anthem scaleogram)
  "Compute the scale index that the anthem would activate on the scaleogram"
  (scale-from-period (anthem-duration-in-samples anthem (anthem-bar-duration anthem))
		     (voices-per-octave scaleogram)))

;;; TODO need to match anacrusis for phase
(defun bar-ridges-for-skeleton (anthem rhythm-skeleton rhythm-scaleogram)
  "Returns the ridges of the given skeleton matching the bar duration"
  (let* ((bar-scale-index (round (bar-scale anthem rhythm-scaleogram))))
    (format t "bar scale index ~a time-support ~a samples~%" 
	    bar-scale-index 
	    (time-support bar-scale-index (voices-per-octave rhythm-scaleogram)))
    (ridges-containing-scale rhythm-skeleton bar-scale-index)))

;;; Match known bar duration against a ridge & determine how much evidence
;;; there is for the bar duration against the time-frequency ridges. The critical question is
;;; how well bar duration matches against human perception of tactus given the same rhythms?
(defun bar-ridges-for-anthem (anthem &key (voices-per-octave 16))
  "Returns the ridges of the given anthem matching the bar duration"
  (let* ((anthem-rhythm (anthem-rhythm anthem))) 
    (multiple-value-bind (skeleton rhythm-scaleogram correlated-ridge-scale-peaks) 
	(skeleton-of-rhythm anthem-rhythm :voices-per-octave voices-per-octave)
      (let* ((matching-ridges (bar-ridges-for-skeleton anthem skeleton rhythm-scaleogram)))
	(plot-highlighted-ridges correlated-ridge-scale-peaks matching-ridges :title (name anthem-rhythm))
	matching-ridges))))

;; (bar-ridges-for-anthem (anthem-named 'australia))
;; (multiple-value-setq (skeleton scaleogram peaks) (skeleton-of-rhythm (anthem-rhythm (anthem# 3))))
;; (setf matching-ridges (bar-ridges-for-skeleton (anthem# 3) skeleton scaleogram))
;; (plot-highlighted-ridges peaks matching-ridges)

;; (defun bar-scale-for-anthem (anthem &key (tactus-selector #'select-longest-lowest-tactus))
;;   "Returns the scale of the given anthem matching the bar duration"
;;   (multiple-value-bind (computed-tactus rhythm-scaleogram)
;;       (tactus-for-rhythm (anthem-rhythm anthem) :tactus-selector tactus-selector)
;;     (format t "computed tactus ~a~%" computed-tactus)
;;     (bar-scale-number (bar-scale anthem rhythm-scaleogram))))

(defmacro clap-to-anthem (anthem)
  `(clap-to-rhythm (anthem-rhythm ,anthem)))

;; (time (clap-to-anthem (anthem# 32))
