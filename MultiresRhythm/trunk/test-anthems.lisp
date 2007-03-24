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

(defparameter *anthem-analysis-path* "/Users/leigh/Data/Anthems")

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

(defun bar-scale (anthem voices-per-octave)
  "Compute the scale index that the anthem would activate on the scaleogram"
  (scale-from-period (anthem-duration-in-samples anthem (anthem-bar-duration anthem))
		     voices-per-octave))

;;; TODO need to match anacrusis for phase
(defun bar-ridges-for-skeleton (anthem rhythm-skeleton)
  "Returns the ridges of the given skeleton matching the bar duration"
  (let* ((bar-scale-index (round (bar-scale anthem (voices-per-octave rhythm-skeleton)))))
    (format t "bar scale index ~a time-support ~a samples~%" 
	    bar-scale-index 
	    (time-support bar-scale-index (voices-per-octave rhythm-skeleton)))
    (ridges-containing-scale rhythm-skeleton bar-scale-index)))

(defun canonical-bar-ridge (anthem rhythm-scaleogram)
  (make-monotone-ridge (round (bar-scale anthem (voices-per-octave rhythm-scaleogram)))
		       (duration-in-samples rhythm-scaleogram)))
  
;;; Match known bar duration against a ridge & determine how much evidence
;;; there is for the bar duration against the time-frequency ridges. The critical question is
;;; how well bar duration matches against human perception of tactus given the same rhythms?
(defun bar-ridges-for-anthem (anthem &key (voices-per-octave 16))
  "Returns the ridges of the given anthem matching the bar duration"
  (let* ((anthem-rhythm (anthem-rhythm anthem))) 
    (multiple-value-bind (skeleton rhythm-scaleogram correlated-ridge-scale-peaks) 
	(skeleton-of-rhythm anthem-rhythm :voices-per-octave voices-per-octave)
      (let* ((matching-ridges (bar-ridges-for-skeleton anthem skeleton)))
;;	(plot-highlighted-ridges rhythm-scaleogram matching-ridges (scaleogram-magnitude rhythm-scaleogram) :title (name anthem-rhythm))
;;	(plot-highlighted-ridges rhythm-scaleogram matching-ridges correlated-ridge-scale-peaks :title (name anthem-rhythm))
	(plot-highlighted-ridges-of-rhythm rhythm-scaleogram 
					   matching-ridges 
					   correlated-ridge-scale-peaks
					   anthem-rhythm :title (name anthem-rhythm))
	(plot-highlighted-ridges-of-rhythm rhythm-scaleogram 
					   (list (canonical-bar-ridge anthem rhythm-scaleogram))
					   correlated-ridge-scale-peaks
					   anthem-rhythm)
	matching-ridges))))

;; (bar-ridges-for-anthem (anthem-named 'australia))
;; (bar-ridges-for-anthem (anthem-named 'america))
;; (bar-ridges-for-anthem (anthem-named 'france))
;; (bar-ridges-for-anthem (anthem-named 'nepal))
;; (bar-ridges-for-anthem (anthem-named 'sweden))
;; (bar-ridges-for-anthem (anthem-named 'netherlands))

;; (multiple-value-setq (skeleton scaleogram peaks) (skeleton-of-rhythm (anthem-rhythm (anthem# 3))))
;; (setf matching-ridges (bar-ridges-for-skeleton (anthem# 3) skeleton))
;; (plot-highlighted-ridges scaleogram matching-ridges peaks)
;; (plot-highlighted-ridges scaleogram matching-ridges (scaleogram-magnitude scaleogram))
;; (plot-highlighted-ridges-of-rhythm scaleogram matching-ridges peaks (anthem-rhythm (anthem# 3)))

;; (plot-highlighted-ridges-of-rhythm scaleogram 
;; 				   (list (canonical-bar-ridge (anthem# 3) scaleogram))
;; 				   peaks
;; 				   (anthem-rhythm (anthem# 3)))

;; (defun bar-scale-for-anthem (anthem &key (tactus-selector #'select-longest-lowest-tactus))
;;   "Returns the scale of the given anthem matching the bar duration"
;;   (multiple-value-bind (computed-tactus rhythm-scaleogram)
;;       (tactus-for-rhythm (anthem-rhythm anthem) :tactus-selector tactus-selector)
;;     (format t "computed tactus ~a~%" computed-tactus)
;;     (bar-scale-number (bar-scale anthem (voices-per-octave rhythm-scaleogram)))))

(defmacro clap-to-anthem (anthem)
  `(clap-to-rhythm (anthem-rhythm ,anthem)))

;; (time (clap-to-anthem (anthem# 32))

(defun generate-anthem-skeletons (&key (count 8) ; (length *national-anthems*)
				  (anthem-path *anthem-analysis-path*))
  "Generates and writes the skeleton of the numbered anthems"
  (dotimes (anthem-index count)
    (let* ((anthem-rhythm (anthem-rhythm (anthem# anthem-index))))
      (if (not (probe-file (make-pathname :directory anthem-path :name (name anthem-rhythm) :type "skeleton")))
	  (generate-and-write-rhythm anthem-rhythm anthem-path)))))

(defun bar-ridges-for-anthem-file (anthem)
  "Returns the ridges of the given anthem matching the bar duration"
  (let* ((anthem-rhythm (anthem-rhythm anthem))) 
    (multiple-value-bind (skeleton rhythm-scaleogram) 
	(read-mra-from-file (make-pathname :directory *anthem-analysis-path* :name (name anthem-rhythm)))
      (let* ((matching-ridges (bar-ridges-for-skeleton anthem skeleton)))
	(plot-cwt+ridges rhythm-scaleogram matching-ridges anthem-rhythm :title (name anthem-rhythm))
	(values matching-ridges rhythm-scaleogram)))))

;; (let ((anthem-rhythm (anthem-rhythm anthem)))
;;   (multiple-value-bind (matching-ridges scaleogram) (bar-ridges-for-anthem-file anthem)
;;     (plot-cwt+ridges scaleogram matching-ridges anthem-rhythm :title (name anthem-rhythm))))

(defun error-span (period vpo)
  (let ((scale (round (scale-from-period period vpo))))
    (- (time-support (1+ scale) vpo) (time-support (1- scale) vpo))))

;;; trawls over entire skeleton. This may not be enough, since it doesn't properly measure
;;; the degree of continuity between scales. Perhaps we should expand the count test to
;;; either side of the scale also.
(defun count-scale-in-skeleton (skeleton scale)
  "Returns the number of samples that lie on the given scale"
  (loop
     for ridge-candidate in skeleton
     sum (count-contains-scale ridge-candidate scale)))

(defun assess-skeleton-for-bar (anthem skeleton)
  "The ratio constitutes the degree to which the bar duration is present in the scaleogram for the given rhythm."
  (let* ((bar-scale-index (round (bar-scale anthem (voices-per-octave skeleton)))))
    (format t "Count for bar scale ~a = ~a, scale ~a = ~a, ~a = ~a~%"
	    bar-scale-index
	    (count-scale-in-skeleton skeleton bar-scale-index)
	    (1+ bar-scale-index)
	    (count-scale-in-skeleton skeleton (1+ bar-scale-index))
	    (1- bar-scale-index)
	    (count-scale-in-skeleton skeleton (1- bar-scale-index)))
    (/ (count-scale-in-skeleton skeleton bar-scale-index) (duration-in-samples skeleton))))

(defun bars-in-anthem-skeletons (&key (count 8) ; (length *national-anthems*)
				 (anthem-path *anthem-analysis-path*))
  "Returns the ridges of the given anthem matching the bar duration"
  (loop
     for anthem-index from 0 below count
     for anthem = (anthem# anthem-index)
     for anthem-rhythm = (anthem-rhythm anthem)
     do (format t "Reading ~a~%" (name anthem-rhythm))
     collect
       (let* ((skeleton (read-skeleton-from-file (make-pathname :directory anthem-path 
								:name (name anthem-rhythm)
								:type "skeleton"))))
	 (assess-skeleton-for-bar anthem skeleton))))

;;; Really good candidate to replace with a BLAS routine...
(defun .partial-sum (a &key (dimension 1)) 
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (let* ((result-length (.array-dimension a dimension))
	 (rows (.array-dimension a 0)) ; HACK!
	 (r (make-double-array result-length)) ; (make-ninstance a result-length)
	 (a-val (val a))
	 (r-val (val r)))
    (dotimes (j result-length)
      (dotimes (i rows)
	(declare (type fixnum i j))
	(setf (aref r-val j) (+ (aref r-val j) (aref a-val i j))))) 
    r))

;;; Just sum over all scales, across time, either the magnitude or correlated ridge peaks.
(defun ridge-persistency-for-anthem (anthem &key (voices-per-octave 16))
  (let* ((anthem-rhythm (anthem-rhythm anthem))) 
    (multiple-value-bind (skeleton rhythm-scaleogram correlated-ridge-scale-peaks) 
	(skeleton-of-rhythm anthem-rhythm :voices-per-octave voices-per-octave)
      (./ (.partial-sum (.transpose correlated-ridge-scale-peaks) :dimension 1)
	  (duration-in-samples rhythm-scaleogram))))

(defun ridge-persistency (scale-peaks)
  ""
  (./ (.partial-sum (.transpose scale-peaks) :dimension 1)
      (.array-dimension scale-peaks 1))))

;; (plot america-ridge-persistency nil)
