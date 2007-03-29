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

(defmacro anthem-name (anthem)
  `(caar ,anthem))

(defmacro anthem-bar-duration (anthem)
  `(seventh (first ,anthem)))

(defmacro anthem-beat-period (anthem)
  `(fifth (first ,anthem)))

(defun anthem-quaver-duration (anthem &key (shortest-ioi 50))
  "Returns the duration of a quaver (eighth note) in samples"
  (/ (* shortest-ioi 2.0) (anthem-beat-period anthem)))

(defun anthem-duration-in-samples (anthem duration &key (shortest-ioi 50))
  "Returns the duration in samples, normalized to equal tempo"
  (let ((min-duration (/ (* shortest-ioi 2.0) (anthem-beat-period anthem))))
    (first (intervals-in-samples (list duration) :ioi min-duration))))

(defun anthem-rhythm (anthem &key (shortest-ioi 50))
  (let* ((anthem-descriptor (first anthem))
	 (anthem-name (symbol-name (first anthem-descriptor)))
	 (min-duration (/ (* shortest-ioi 2.0) (anthem-beat-period anthem)))
	 (anthem-iois (second anthem)))
    (iois-to-rhythm anthem-name anthem-iois :shortest-ioi min-duration)))

;;; :start-at = the anacrusis duration from the start of the bar.
(defun anthem-start-at (anthem)
  (ninth (first anthem)))

(defun anthem-anacrusis (anthem)
  (- (anthem-bar-duration anthem) (anthem-start-at anthem)))

(defun bar-scale (anthem voices-per-octave)
  "Compute the scale index that the anthem would activate on the scaleogram for the bar"
  (scale-from-period (anthem-duration-in-samples anthem (anthem-bar-duration anthem))
		     voices-per-octave))

;;; TODO should make both bar-scale and beat-scale the same function.
(defun beat-scale (anthem voices-per-octave)
  "Compute the scale index that the anthem would activate on "
  (scale-from-period (anthem-duration-in-samples anthem (anthem-beat-period anthem))
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

(defun generate-anthem-skeletons (&key (count 20) ; (length *national-anthems*)
				  (anthem-path *anthem-analysis-path*))
  "Generates and writes the skeleton of the numbered, time limited, anthems"
  (dotimes (anthem-index count)
    ;; Limits the maximum length of the rhythm to make the computation time manageable.
    (let* ((anthem-rhythm (limit-rhythm (anthem-rhythm (anthem# anthem-index)))))
      (format t "Processing ~a~%" (name anthem-rhythm))
      (if (not (probe-file (make-pathname :directory anthem-path :name (name anthem-rhythm) :type "skeleton")))
	  (skeleton-of-rhythm-cached anthem-rhythm :cache-directory anthem-path)))))

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

;; (defun assess-skeleton-for-bar (anthem skeleton)
;;   "The ratio constitutes the degree to which the bar duration is present in the scaleogram for the given rhythm."
;;   (let* ((bar-scale-index (round (bar-scale anthem (voices-per-octave skeleton)))))
;;     (format t "Count for bar scale ~a = ~a, scale ~a = ~a, ~a = ~a~%"
;; 	    bar-scale-index
;; 	    (count-scale-in-skeleton skeleton bar-scale-index)
;; 	    (1+ bar-scale-index)
;; 	    (count-scale-in-skeleton skeleton (1+ bar-scale-index))
;; 	    (1- bar-scale-index)
;; 	    (count-scale-in-skeleton skeleton (1- bar-scale-index)))
;;     (/ (count-scale-in-skeleton skeleton bar-scale-index) (duration-in-samples skeleton))))

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
;;; TODO This is pretty slow, due to the n^2 .partial-sum function.
(defun ridge-persistency (scale-peaks)
  "Returns the normalised measure of contribution of each scale in the correlated scale peaks"
  (./ (.partial-sum (.transpose scale-peaks) :dimension 1)
      (.array-dimension scale-peaks 1)))

(defun plot-ridge-persistency-for-anthem (anthem &key (voices-per-octave 16))
  (let* ((anthem-rhythm (anthem-rhythm anthem))) 
    (multiple-value-bind (skeleton rhythm-scaleogram correlated-ridge-scale-peaks) 
	(skeleton-of-rhythm anthem-rhythm :voices-per-octave voices-per-octave)
     (nplot (list (ridge-persistency correlated-ridge-scale-peaks)
		  (ridge-persistency (scaleogram-magnitude rhythm-scaleogram))
		  (ridge-persistency (make-ridge-plane skeleton))) 
	    nil
	    :legends '("ridge persistency" "magnitude persistency" "skeleton persistency")
	    :title (format nil "Ridge persistency for ~a" (name anthem-rhythm))))))

;; (setf america-ridge-persistency (ridge-persistency-for-anthem (anthem-named 'america)))
;; (plot australia-ridge-persistency nil)
;; (bar-scale (anthem-named 'america) 16)
;; (.aref america-ridge-persistency 99)

;; (time-support (.+ (.find prominent-scales) 1) (voices-per-octave skeleton))
;; (.* x ridge-persistence)

;; (plot-image #'magnitude-image (list correlated-ridge-scale-peaks) '((1.0 0.5) (0.0 0.6))
;;	    (axes-labelled-in-seconds rhythm-scaleogram 200 4))
;; (nplot (list (ridge-persistency correlated-ridge-scale-peaks) x) nil)

(defun assess-skeleton-for-bar (anthem skeleton)
  "The ratio constitutes the degree to which the bar duration is present in the scaleogram for the given rhythm."
  (let* ((bar-scale-index (round (bar-scale anthem (voices-per-octave skeleton))))
	 (ridge-plane (make-ridge-plane skeleton))
	 (ridge-persistence (ridge-persistency ridge-plane))
	 ;; The threshold below should be calculated using a variance measure.
	 (prominent-ridge-profiles (.> ridge-persistence 0.25))
	 ;; (nplot (list ridge-persistence prominent-ridge-profiles) nil)
	 (prominent-scale-indices (.+ (.find prominent-ridge-profiles) 1)))
;;     (format t "Count for bar scale ~a = ~a prominent-scale-indices ~a~%"
;; 	    bar-scale-index
;; 	    (/ (count-scale-in-skeleton skeleton bar-scale-index) (duration-in-samples skeleton))
    (format t "bar scale = ~a, prominent-scale-indices ~a, matches ~a~%" 
	    bar-scale-index prominent-scale-indices (numberp (find bar-scale-index (val prominent-scale-indices))))
    (format t "relative presence of bar ridge ~f~%" (.aref ridge-persistence (1- bar-scale-index)))
    (.aref ridge-persistence (1- bar-scale-index))))

(defun bars-in-anthem-skeletons (&key (count 8) ; (length *national-anthems*)
				 (anthem-path *anthem-analysis-path*))
  "Returns the ridges of the given anthem matching the bar duration"
  (loop
     for anthem-index from 0 below count
     for anthem = (anthem# anthem-index)
     for anthem-rhythm = (anthem-rhythm anthem)
     do (format t "Reading ~a~%" (name anthem-rhythm))
     collect
       (let* ((skeleton-pathname (make-pathname :directory anthem-path :name (name anthem-rhythm) :type "skeleton"))
	      (skeleton (read-skeleton-from-file skeleton-pathname)))
	 (assess-skeleton-for-bar anthem skeleton))))

;; Perhaps it's better to keep the error so we don't miscount.
;; (probe-file (make-pathname :directory anthem-path :name (name anthem-rhythm) :type "skeleton"))

;; Need a routine to create a labelling based on country.
(defun label-country (count)
  (loop
     for anthem-index from 0 below count
     collect (list (string (anthem-name (anthem# anthem-index))) anthem-index)))

;; Rotate the xtics & drop the font size.
(defun plot-anthem-bar-presence (count)
  (let ((bar-ratios (bars-in-anthem-skeletons :count count)))
    ;; (plot-command (format nil "set xtics (~{~{\"~a\" ~5d~}~^, ~})~%" (label-country count)))
    (plot (nlisp::list-to-array bar-ratios) nil 
	  :style "boxes fill solid border 9" 
	  :label "Proportion of bar duration present in scaleogram of anthem"
	  :aspect-ratio 0.66
	  :reset nil)))

