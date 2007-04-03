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
;(load "/Volumes/iDisk/Research/Data/DesainHoning/national anthems.lisp")

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

;;; 1 crochet = 100 samples => 120 BPM
(defun anthem-minimum-duration (anthem &key (crochet-duration 100))
  "Returns the duration of the smallest interval in the anthem, in samples, given a fixed tempo."
  (/ crochet-duration (anthem-beat-period anthem)))

(defun anthem-duration-in-samples (anthem duration)
  "Returns the duration in samples, normalized to equal tempo"
  (first (intervals-in-samples (list duration) :ioi (anthem-minimum-duration anthem))))

(defun anthem-rhythm (anthem)
  "Returns a rhythm instance from the anthem"
  (let* ((anthem-descriptor (first anthem))
	 (anthem-name (symbol-name (first anthem-descriptor)))
	 (anthem-iois (second anthem)))
    (iois-to-rhythm anthem-name anthem-iois :shortest-ioi (anthem-minimum-duration anthem))))

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

(defun canonical-bar-ridge (anthem rhythm-scaleogram)
  (make-monotone-ridge (round (bar-scale anthem (voices-per-octave rhythm-scaleogram)))
		       (duration-in-samples rhythm-scaleogram)))
  
;;; Match known bar duration against a ridge & determine how much evidence
;;; there is for the bar duration against the time-frequency ridges. The critical question is
;;; how well bar duration matches against human perception of tactus given the same rhythms?
;;; TODO need to match anacrusis for phase
(defun bar-ridges-for-anthem (anthem &key (voices-per-octave 16))
  "Returns the ridges of the given anthem matching the bar duration"
  (let* ((anthem-rhythm (anthem-rhythm anthem))
	 (bar-scale-index))
    (multiple-value-bind (rhythm-skeleton rhythm-scaleogram correlated-ridge-scale-peaks) 
	(skeleton-of-rhythm-cached anthem-rhythm 
				   :voices-per-octave voices-per-octave 
				   :cache-directory *anthem-analysis-path*)
      (declare (ignore correlated-ridge-scale-peaks))
      (setf bar-scale-index (round (bar-scale anthem (voices-per-octave rhythm-skeleton))))
      (format t "bar scale index ~a time-support ~a samples~%" 
	      bar-scale-index 
	      (time-support bar-scale-index (voices-per-octave rhythm-skeleton)))
      (values (ridges-containing-scale rhythm-skeleton bar-scale-index)
	      rhythm-scaleogram
	      correlated-ridge-scale-peaks))))

;; (bar-ridges-for-anthem (anthem-named 'australia))
;; (bar-ridges-for-anthem (anthem-named 'america))
;; (bar-ridges-for-anthem (anthem-named 'france))
;; (bar-ridges-for-anthem (anthem-named 'nepal))
;; (bar-ridges-for-anthem (anthem-named 'sweden))
;; (bar-ridges-for-anthem (anthem-named 'netherlands))

;; (plot-highlighted-ridges scaleogram matching-ridges peaks)
;; (plot-highlighted-ridges scaleogram matching-ridges (scaleogram-magnitude scaleogram))
;; (plot-highlighted-ridges-of-rhythm scaleogram matching-ridges peaks (anthem-rhythm (anthem# 3)))

;; (plot-highlighted-ridges-of-rhythm scaleogram 
;; 				   (list (canonical-bar-ridge (anthem# 3) scaleogram))
;; 				   peaks
;; 				   (anthem-rhythm (anthem# 3)))

(defun plot-bar-ridges-of-anthem (anthem)
  (let ((anthem-rhythm (anthem-rhythm anthem)))
    (multiple-value-bind (matching-ridges rhythm-scaleogram correlated-ridge-scale-peaks) 
	(bar-ridges-for-anthem anthem)
      (plot-highlighted-ridges-of-rhythm rhythm-scaleogram 
					 matching-ridges 
					 correlated-ridge-scale-peaks
					 anthem-rhythm :title (name anthem-rhythm))
      (plot-highlighted-ridges-of-rhythm rhythm-scaleogram 
					 (list (canonical-bar-ridge anthem rhythm-scaleogram))
					 correlated-ridge-scale-peaks
					 anthem-rhythm :title (name anthem-rhythm)))))

;;	(plot-highlighted-ridges rhythm-scaleogram matching-ridges (scaleogram-magnitude rhythm-scaleogram) :title (name anthem-rhythm))
;;	(plot-highlighted-ridges rhythm-scaleogram matching-ridges correlated-ridge-scale-peaks :title (name anthem-rhythm))

;; (defun bar-scale-for-anthem (anthem &key (tactus-selector #'select-longest-lowest-tactus))
;;   "Returns the scale of the given anthem matching the bar duration"
;;   (multiple-value-bind (computed-tactus rhythm-scaleogram)
;;       (tactus-for-rhythm (anthem-rhythm anthem) :tactus-selector tactus-selector)
;;     (format t "computed tactus ~a~%" computed-tactus)
;;     (bar-scale-number (bar-scale anthem (voices-per-octave rhythm-scaleogram)))))

(defmacro clap-to-anthem (anthem)
  `(clap-to-rhythm (anthem-rhythm ,anthem)))

;; (time (clap-to-anthem (anthem# 32))

(defun generate-anthem-skeletons (&key (count (length *national-anthems*)) 
				  (anthem-path *anthem-analysis-path*)
				  (length-limit 16384))
  "Generates and writes the skeleton of the numbered, time limited, anthems"
  (dotimes (anthem-index count)
    ;; Limits the maximum length of the rhythm to make the computation time manageable.
    (let* ((anthem-rhythm (limit-rhythm (anthem-rhythm (anthem# anthem-index))
					:maximum-samples length-limit)))
      (format t "Processing ~a~%" (name anthem-rhythm))
      (if (not (probe-file (make-pathname :directory anthem-path :name (name anthem-rhythm) :type "skeleton")))
	  (skeleton-of-rhythm-cached anthem-rhythm :cache-directory anthem-path)))))

;;; (generate-anthem-skeletons :anthem-path "/Users/leigh/Data/Anthems" :length-limit 16384)
;;; Limits to the first 48 semi-quavers, matching Zaanen
;;; (generate-anthem-skeletons :anthem-path "/Users/leigh/Data/ShortAnthems" :length-limit (* 48 25))

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
	 (rows (.array-dimension a 0)) ; TODO HACK!
	 (r (make-double-array result-length)) ; (make-ninstance a result-length)
	 (a-val (val a))
	 (r-val (val r)))
    (dotimes (j result-length)
      (dotimes (i rows)
	(declare (type fixnum i j))
	(setf (aref r-val j) (+ (aref r-val j) (aref a-val i j))))) 
    r))

;;; Just sum over all scales, across time, either the magnitude or correlated ridge peaks.
;;; TODO This is pretty slow, due to the n^2 .partial-sum function and the .transpose.
(defun ridge-persistency (scale-peaks)
  "Returns the normalised measure of contribution of each scale in the correlated scale peaks"
  (if scale-peaks
      (./ (.partial-sum (.transpose scale-peaks) :dimension 1)
	  (.array-dimension scale-peaks 1))
      nil))

(defun plot-ridge-persistency-for-anthem (anthem &key (voices-per-octave 16))
  (let* ((anthem-rhythm (anthem-rhythm anthem))
	 (bar-scale-index)
	 (beat-scale-index)) 
    (reset-plot)
    (multiple-value-bind (rhythm-skeleton rhythm-scaleogram) ;  correlated-ridge-scale-peaks
	(skeleton-of-rhythm-cached anthem-rhythm 
				   :voices-per-octave voices-per-octave 
				   :cache-directory *anthem-analysis-path*)
      (setf bar-scale-index (round (bar-scale anthem (voices-per-octave rhythm-skeleton))))
      (setf beat-scale-index (round (beat-scale anthem (voices-per-octave rhythm-skeleton))))
      (plot-command (format nil "set arrow 1 to ~d,0.5 from ~d,0.7 ls 3" bar-scale-index bar-scale-index))
      (plot-command (format nil "set arrow 2 to ~d,0.5 from ~d,0.7 ls 4" beat-scale-index beat-scale-index))
      (plot-command "show arrow 1")
      (plot-command "show arrow 2")
      (nplot (list (ridge-persistency (scaleogram-magnitude rhythm-scaleogram))
		   ;; (ridge-persistency correlated-ridge-scale-peaks) 
		   (ridge-persistency (make-ridge-plane rhythm-skeleton)))
	     nil
	     :reset nil
	     :aspect-ratio 0.66
	     :legends '("magnitude persistency" "skeleton persistency") ; "ridge persistency"
	  ;   :styles '("lines linestyle 1" "lines linestyle 3")
	     :title (format nil "Ridge persistency for ~a" (name anthem-rhythm))))))

;; (plot-ridge-persistency-for-anthem (anthem-named 'america))
;; (plot-ridge-persistency-for-anthem (anthem-named 'netherlands))

;;; "The axis of non-metricality": Anthems that have low bar-ridge measures:
;; (plot-ridge-persistency-for-anthem (anthem-named 'australia))
;; (plot-ridge-persistency-for-anthem (anthem-named 'greenland))
;; (plot-ridge-persistency-for-anthem (anthem-named 'jordan))
;; (plot-ridge-persistency-for-anthem (anthem-named 'vatican))
;; (plot-ridge-persistency-for-anthem (anthem-named 'luxembourg))

;; (bar-scale (anthem-named 'america) 16)
;; (.aref america-ridge-persistency 99)
;; (time-support (.+ (.find prominent-scales) 1) (voices-per-octave skeleton))
;; (.* x ridge-persistence)
;; (plot-image #'magnitude-image (list correlated-ridge-scale-peaks) '((1.0 0.5) (0.0 0.6))
;;	    (axes-labelled-in-seconds rhythm-scaleogram 200 4))
;; (nplot (list (ridge-persistency correlated-ridge-scale-peaks) x) nil)

(defun beat-persistency-of-skeleton (anthem skeleton)
  "The ratio constitutes the degree to which the beat duration is present in the scaleogram for the given rhythm."
  (let* ((beat-scale-index (round (beat-scale anthem (voices-per-octave skeleton))))
	 (ridge-persistence (ridge-persistency (make-ridge-plane skeleton))))
    (format t "beat scale = ~a~%" beat-scale-index)
    (.aref ridge-persistence (1- beat-scale-index))))

(defun bar-persistency-of-skeleton (anthem skeleton)
  "The ratio constitutes the degree to which the bar duration is present in the scaleogram for the given rhythm."
  (let* ((bar-scale-index (round (bar-scale anthem (voices-per-octave skeleton))))
	 (ridge-persistence (ridge-persistency (make-ridge-plane skeleton))))
    (format t "bar scale = ~a~%" bar-scale-index)
    (.aref ridge-persistence (1- bar-scale-index))))

(defun skeleton-has-prominent-scale (skeleton scale-index)
  "Returns T or NIL if the bar duration is strongly present in the scaleogram for the given rhythm."
  (let* ((ridge-plane (make-ridge-plane skeleton))
	 (ridge-persistence (ridge-persistency ridge-plane))
	 ;; The threshold below should be calculated using a variance measure.
	 (prominent-ridge-profiles (.> ridge-persistence 0.25))
	 ;; (nplot (list ridge-persistence prominent-ridge-profiles) nil)
	 (prominent-scale-indices (.+ (.find prominent-ridge-profiles) 1))
	 (duration-present (numberp (find scale-index (val prominent-scale-indices)))))
;;     (format t "Count for scale ~a = ~a prominent-scale-indices ~a~%"
;; 	    scale-index
;; 	    (/ (count-scale-in-skeleton skeleton scale-index) (duration-in-samples skeleton))
    (format t "prominent-scale-indices ~a~%periods ~a~%" 
	    prominent-scale-indices 
	    (time-support prominent-scale-indices (voices-per-octave skeleton)))
    duration-present))

(defun assess-anthem (anthem &key (anthem-path *anthem-analysis-path*))
  (format t "Reading ~a~%" (anthem-name anthem))
  (let* ((anthem-rhythm (anthem-rhythm anthem))
	 (skeleton-pathname (make-pathname :directory anthem-path :name (name anthem-rhythm) :type "skeleton"))
	 (skeleton (read-skeleton-from-file skeleton-pathname))
	 (beat-persistency (beat-persistency-of-skeleton anthem skeleton))
	 (bar-persistency (bar-persistency-of-skeleton anthem skeleton)))
    (format t "bar prominently in skeleton ~a~%" 
	    (skeleton-has-prominent-scale skeleton (round (bar-scale anthem (voices-per-octave skeleton)))))
    (format t "relative presence of beat ridge ~f~%" beat-persistency)
    (format t "relative presence of bar ridge ~f~%" bar-persistency)
    bar-persistency))

(defun bars-in-anthem-skeletons (&key (count (length *national-anthems*)))
  "Returns the ridges of the given anthem matching the bar duration"
  (loop
     for anthem-index from 0 below count
     for anthem = (anthem# anthem-index)
     collect (assess-anthem anthem)))

(defun label-country (count)
  "Routine to create a labelling based on country."
  (loop
     for anthem-index from 0 below count
     collect (list (string (anthem-name (anthem# anthem-index))) anthem-index)))

(defun plot-anthem-bar-presence (&key (count (length *national-anthems*)))
  (let ((bar-ratios (bars-in-anthem-skeletons :count count)))
    (reset-plot)
    ;; TODO Rotate the xtics & drop the font size.
    ;; (plot-command (format nil "set xtics (~{~{\"~a\" ~5d~}~^, ~})~%" (label-country count)))
    (plot (nlisp::list-to-array bar-ratios) nil 
	  :style "boxes fill solid border 9" 
	  :label "Proportion of bar duration present in scaleogram of anthem"
	  :aspect-ratio 0.66
	  :reset nil)))

;; (plot-anthem-bar-presence)
;; (setf bar-ratios (bars-in-anthem-skeletons :count count))
;; (mean bar-ratios)
;; (.save-to-octave-file bar-ratios "/Users/leigh/Data/anthem-bar-ratios.octave")
;;
;; (setf beat-ratios (bars-in-anthem-skeletons :count count))
;; (mean beat-ratios)
;; (.save-to-octave-file beat-ratios "/Users/leigh/Data/anthem-beat-ratios.octave")

(defun make-histogram (data)
  "Returns each unique value in data along with the count of it's occurance in a hash-table"
  (let ((element-count-hash (make-hash-table :size (length data))))
    (map nil (lambda (element) (incf (gethash element element-count-hash 0))) data)
    element-count-hash))

(defun relative-interval-counts (anthem)
  (maphash (lambda (key count) (format t "duration ~a count ~a~%" key count)) (make-histogram (second anthem))))

;;; Low combined value for beat and bar values.
;; (assess-anthem (anthem-named 'ghana))
;; (relative-interval-counts (anthem-named 'ghana))
;; (plot-ridge-persistency-for-anthem (anthem-named 'ghana))
;; (plot-rhythm (anthem-rhythm (anthem-named 'ghana)))
;; (plot-bar-ridges-of-anthem (anthem-named 'ghana))
;; (time-support 116 16) = 12 * 50 = 608

;; Count the meters in the database. Should match Zaanen et al.
;; (count "4/4" *national-anthems* :test #'equal :key #'cadar)
;; (count "12/8" *national-anthems* :test #'equal :key #'cadar)
;; (count "2/4" *national-anthems* :test #'equal :key #'cadar)
;; (count "3/4" *national-anthems* :test #'equal :key #'cadar)
;; (count "2/2" *national-anthems* :test #'equal :key #'cadar)
