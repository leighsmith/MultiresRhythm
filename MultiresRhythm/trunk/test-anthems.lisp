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

;;; :start-at = the anacrusis duration from the start of the bar.
;;; Defined as a function, rather than as a macro to allow it to be used as a key to remove-if-not.
(defun anthem-start-at (anthem)
  "Returns the position in the bar where the upbeat starts, in grid units"
  (ninth (first anthem)))

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

(defun anthem-anacrusis-duration (anthem)
  "Returns the number of grid units of time (not the number of notes), before the downbeat"
  (- (anthem-bar-duration anthem) (anthem-start-at anthem)))

(defun anthem-interval-scale (anthem voices-per-octave interval)
  "Compute the scale index that the interval in the given anthem would activate on its scaleogram"
  (scale-from-period (anthem-duration-in-samples anthem interval) voices-per-octave))

;; (anthem-interval-scale (anthem-named 'australia) 16 (anthem-beat-period (anthem-named 'australia)))

(defun bar-scale (anthem voices-per-octave)
  "Compute the scale index that the anthem would activate on the scaleogram for the bar"
  (scale-from-period (anthem-duration-in-samples anthem (anthem-bar-duration anthem))
		     voices-per-octave))

;;; TODO should make both bar-scale and beat-scale the same function.
(defun beat-scale (anthem voices-per-octave)
  (scale-from-period (anthem-duration-in-samples anthem (anthem-beat-period anthem))
		     voices-per-octave))

(defun canonical-bar-ridge (anthem rhythm-scaleogram)
  (make-monotone-ridge (round (bar-scale anthem (voices-per-octave rhythm-scaleogram)))
		       (duration-in-samples rhythm-scaleogram)))
  
;;; Match known bar duration against a ridge & determine how much evidence
;;; there is for the bar duration against the time-frequency ridges. The critical question is
;;; how well bar duration matches against human perception of tactus given the same rhythms?
;;; TODO need to match anacrusis for phase
(defun bar-ridges-for-anthem (anthem &key (voices-per-octave 16) (anthem-path *anthem-analysis-path*))
  "Returns the ridges of the given anthem matching the bar duration"
  (let* ((anthem-rhythm (anthem-rhythm anthem))
	 (bar-scale-index))
    (multiple-value-bind (rhythm-skeleton rhythm-scaleogram correlated-ridge-scale-peaks) 
	(skeleton-of-rhythm-cached anthem-rhythm 
				   :voices-per-octave voices-per-octave 
				   :cache-directory anthem-path)
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

;; (plot-highlighted-ridges rhythm-scaleogram matching-ridges (scaleogram-magnitude rhythm-scaleogram) :title (name anthem-rhythm))
;; (plot-highlighted-ridges rhythm-scaleogram matching-ridges correlated-ridge-scale-peaks :title (name anthem-rhythm))

;; (defun bar-scale-for-anthem (anthem &key (tactus-selector #'select-longest-lowest-tactus))
;;   "Returns the scale of the given anthem matching the bar duration"
;;   (multiple-value-bind (computed-tactus rhythm-scaleogram)
;;       (tactus-for-rhythm (anthem-rhythm anthem) :tactus-selector tactus-selector)
;;     (format t "computed tactus ~a~%" computed-tactus)
;;     (bar-scale-number (bar-scale anthem (voices-per-octave rhythm-scaleogram)))))

(defun anacrusis-notes (anthem)
  "Return the sample times of notes of the anacrusis, prior to the downbeat"
  (onsets-in-samples (subset-of-rhythm (anthem-rhythm anthem)
				       (list t (* (anthem-anacrusis-duration anthem)
						  (anthem-minimum-duration anthem))))))

(defun downbeat-number (anthem)
  "Return the number of notes of the anacrusis, prior to the downbeat"
  (.length (anacrusis-notes anthem)))

(defun clap-to-anthem (anthem)
  (clap-to-rhythm (anthem-rhythm anthem) :start-from-beat (downbeat-number anthem)))

;; (time (clap-to-anthem (anthem# 32))
;; (save-rhythm-and-claps (anthem-rhythm (anthem-named 'america)) (clap-to-anthem (anthem-named 'america)))

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
;;; For the Ghanian anthem, which has a period which exceeds the maximum wavelet period.
;;; (generate-anthem-skeletons :anthem-path "/Users/leigh/Data/ShortAnthems" :length-limit 4096)


(defun error-span (scale vpo)
  "Returns the error in duration from neighbour scales"
  (let ((scale-index (round scale)))
    (- (time-support (1+ scale-index) vpo) (time-support (1- scale-index) vpo))))

;; (error-span (bar-scale (first (anthems-of-meter "4/4")) 16) 16)
;; (error-span (beat-scale (first (anthems-of-meter "4/4")) 16) 16)
;; (error-span (bar-scale (first (anthems-of-meter "3/4")) 16) 16)
;; (error-span (beat-scale (first (anthems-of-meter "3/4")) 16) 16)
;; (error-span (bar-scale (first (anthems-of-meter "2/2")) 16) 16)
;; (error-span (beat-scale (first (anthems-of-meter "2/2")) 16) 16)
;; (error-span (bar-scale (first (anthems-of-meter "12/8")) 16) 16)
;; (error-span (beat-scale (first (anthems-of-meter "12/8")) 16) 16)
;; (error-span (bar-scale (first (anthems-of-meter "2/4")) 16) 16)
;; (error-span (beat-scale (first (anthems-of-meter "2/4")) 16) 16)

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

;;; Just sum over all scales, across time, either the magnitude or correlated ridge peaks.
;;; TODO This is pretty slow, due to the n^2 .partial-sum function and the .transpose.
(defun ridge-persistency (scale-peaks)
  "Returns the normalised measure of contribution of each scale in the correlated scale peaks"
  (if scale-peaks
      (./ (.partial-sum (.transpose scale-peaks) :dimension 1)
	  (.array-dimension scale-peaks 1))
      nil))

(defun ridge-persistency-of-anthem (anthem &key (anthem-path *anthem-analysis-path*))
  (format t "Reading ~a~%" (anthem-name anthem))
  (let* ((anthem-rhythm (anthem-rhythm anthem))
	 (skeleton-pathname (make-pathname :directory anthem-path :name (name anthem-rhythm) :type "skeleton"))
	 (skeleton (read-skeleton-from-file skeleton-pathname)))
    (ridge-persistency (make-ridge-plane skeleton))))

;; (bar-scale (anthem-named 'america) 16)
;; (.aref america-ridge-persistency 99)
;; (.* x ridge-persistence)
;; (plot-image #'magnitude-image (list correlated-ridge-scale-peaks) '((1.0 0.5) (0.0 0.6))
;;	    (axes-labelled-in-seconds rhythm-scaleogram 200 4))
;; (nplot (list (ridge-persistency correlated-ridge-scale-peaks) x) nil)

;; (defun persistency-of-period-in-skeleton (anthem skeleton period)
;;   "Returns the proportion of the degree to which the given period duration is present in the skeleton for the given anthem."
;;   (let* ((period-scale (round (anthem-interval-scale anthem (voices-per-octave skeleton) period)))
;; 	 (ridge-persistence (ridge-persistency (make-ridge-plane skeleton))))
;;     (format t "anthem interval ~a, scaleogram scale = ~a, duration ~a samples~%" 
;; 	    period period-scale (time-support period-scale (voices-per-octave skeleton)))
;;     (.aref ridge-persistence (1- period-scale))))

(defun persistency-of-period-in-skeleton (anthem skeleton period)
  "Returns the proportion of the degree to which the given period duration is present in the skeleton for the given anthem."
  (let* ((period-scale (round (anthem-interval-scale anthem (voices-per-octave skeleton) period)))
	 (ridge-plane (make-ridge-plane skeleton)))
    (format t "anthem interval ~a, scaleogram scale = ~a, duration ~a samples~%" 
	    period period-scale (time-support period-scale (voices-per-octave skeleton)))
    ;; persistency-of-scale-in-plane?
    (./ (.sum (.or (.row ridge-plane (1- period-scale))
		   (.row ridge-plane period-scale)
		   (.row ridge-plane (1+ period-scale))))
	(duration-in-samples skeleton))))

(defun skeleton-has-prominent-scale (skeleton scale-index &key (threshold 0.25))
  "Returns T or NIL if the bar duration is strongly present in the scaleogram for the given rhythm."
  (let* ((ridge-plane (make-ridge-plane skeleton))
	 (ridge-persistence (ridge-persistency ridge-plane))
	 ;; The threshold below should be calculated using a variance measure.
	 (prominent-ridge-profiles (.> ridge-persistence threshold))
	 ;; (nplot (list ridge-persistence prominent-ridge-profiles) nil)
	 (prominent-scale-indices (.+ (.find prominent-ridge-profiles) 1))
	 (duration-present (numberp (find scale-index (val prominent-scale-indices))))
	 (periods (time-support prominent-scale-indices (voices-per-octave skeleton))))
;;     (format t "Count for scale ~a = ~a prominent-scale-indices ~a~%"
;; 	    scale-index
;; 	    (/ (count-scale-in-skeleton skeleton scale-index) (duration-in-samples skeleton))
    (format t "prominent-scale-indices ~a~%periods ~a~%crochets ~a~%" 
	    prominent-scale-indices periods (./ periods 100.0))
    duration-present))

(defun assess-anthem-for-period (anthem period &key (anthem-path *anthem-analysis-path*))
  (format t "Reading ~a~%" (anthem-name anthem))
  (let* ((anthem-rhythm (anthem-rhythm anthem))
	 (skeleton-pathname (make-pathname :directory anthem-path :name (name anthem-rhythm) :type "skeleton"))
	 (skeleton (read-skeleton-from-file skeleton-pathname)))
    (persistency-of-period-in-skeleton anthem skeleton period)))

;; (assess-anthem-for-period (anthem-named 'greenland) 20)
;; (assess-anthem-for-period (anthem-named 'greenland) 12)

(defun assess-anthem (anthem &key (anthem-path *anthem-analysis-path*))
  (format t "Reading ~a~%" (anthem-name anthem))
  (let* ((anthem-rhythm (anthem-rhythm anthem))
	 (skeleton-pathname (make-pathname :directory anthem-path :name (name anthem-rhythm) :type "skeleton"))
	 (skeleton (read-skeleton-from-file skeleton-pathname))
	 (beat-persistency (persistency-of-period-in-skeleton anthem skeleton (anthem-beat-period anthem)))
	 (bar-persistency (persistency-of-period-in-skeleton anthem skeleton (anthem-bar-duration anthem))))
    (format t "bar prominently in skeleton ~a~%" 
	    (skeleton-has-prominent-scale skeleton (round (bar-scale anthem (voices-per-octave skeleton)))))
    (format t "relative presence of beat ridge ~f~%" beat-persistency)
    (format t "relative presence of bar ridge ~f~%" bar-persistency)
    bar-persistency))

(defun bars-in-anthem-skeletons (&key (anthems *national-anthems*))
  "Returns the ridges of the given anthem matching the bar duration"
  (loop
     for anthem in anthems
     collect (assess-anthem-for-period anthem (anthem-bar-duration anthem))))

(defun beats-in-anthem-skeletons (&key (anthems *national-anthems*))
  "Returns the ridges of the given anthem matching the bar duration"
  (loop
     for anthem in anthems
     collect (assess-anthem-for-period anthem (anthem-beat-period anthem))))

(defun label-country (anthems)
  "Routine to create a labelling based on country."
  (loop
     for anthem-index from 0 below (length anthems) by 5
     collect (list (string (anthem-name (anthem# anthem-index))) anthem-index)))

;; (setf non-bar-ratios (nlisp::list-to-array (loop for anthem in *national-anthems* collect (assess-anthem-for-period anthem 7))))
;; (mean non-bar-ratios)
;; (stddev non-bar-ratios)

;; (setf bar-ratios (bars-in-anthem-skeletons :count count))
;; (mean bar-ratios)
;; (.save-to-octave-file bar-ratios "/Users/leigh/Data/anthem-bar-ratios.octave")
;;
;; (setf beat-ratios (bars-in-anthem-skeletons :count count))
;; (mean beat-ratios)
;; (.save-to-octave-file beat-ratios "/Users/leigh/Data/anthem-beat-ratios.octave")

(defun make-histogram (data)
  "Returns each unique value in data along with the count of it's occurrence in a hash-table"
  (let ((element-count-hash (make-hash-table :size (length data))))
    (map nil (lambda (element) (incf (gethash element element-count-hash 0))) data)
    element-count-hash))

(defun display-interval-counts (histogram-hash-table)
  (maphash (lambda (key count) (format t "duration ~a count ~a~%" key count)) histogram-hash-table))

;; (display-interval-counts (make-histogram (second (anthem-named 'america))))

(defun make-histogram-of-anthem-intervals (&key (anthems *national-anthems*))
  "Returns each unique value in data along with the count of it's occurrence in a hash-table"
  (let ((element-count-hash (make-hash-table)))
    (dolist (anthem anthems)
      (map nil
	   (lambda (element) (incf (gethash element element-count-hash 0)))
	   (mapcar (lambda (x) (/ x (float (anthem-beat-period anthem) 1d0))) (second anthem))))
    element-count-hash))

(defun histogram-counts (histogram-hash-table intervals)
  (nlisp::list-to-array (loop for interval across (val intervals)
			   collect (float (gethash interval histogram-hash-table)))))

(defun histogram-intervals (histogram-hash-table)
  "Returns a sorted n-array of histogram intervals from the hash-table"
  (let ((all-keys '()))
    (maphash #'(lambda (key value) (push key all-keys)) histogram-hash-table)
    (nlisp::list-to-array (sort all-keys #'<))))

;; (display-interval-counts (make-histogram-of-anthem-intervals :anthems (anthems-of-meter "3/4")))

(defun crochets-of-anthems (anthems)
  "Returns the crotchet ratios of the intervals sorted in ascending order"
  (let ((b '())) 
    (dolist (anthem anthems (reverse b))
      (push (sort (mapcar (lambda (x) (/ x (float (anthem-beat-period anthem) 1d0))) (second anthem)) #'<)
	    b))))

(defun check-integrity-of-anthems (anthems)
  "Returns any anthems which have intervals longer than more than 4 crochets.
Ghana (12/8) and Malaya (repeated intervals of 5) are fine."
  (let* ((highest-intervals (nlisp::list-to-array (mapcar (lambda (x) (first (last x))) (crochets-of-anthems anthems))))
	 (suspicious (.find (.> highest-intervals 4))))
    (loop 
       for anthem-index across (val suspicious)
       do (format t "~a~%" (nth anthem-index anthems)))))

(defun bar-period-in-anthem-intervals (&key (anthems *national-anthems*))
  "Returns the ridges of the given anthem matching the bar duration"
  (loop
     for anthem in anthems
     do (format t "~a~%" (anthem-name anthem))
     collect (gethash (anthem-bar-duration anthem) (make-histogram (second anthem)))))

;; (bar-period-in-anthem-intervals :anthems (anthems-of-meter "4/4"))

;; (/ (count-if #'numberp (bar-period-in-anthem-intervals)) (float (length *national-anthems*)))

;; (time-support 116 16) = 12 * 50 = 608

(defun relative-interval-occurrence-for-anthem (anthem &key 
						(vpo 16) (crochet-duration 100) (max-time-limit 16384))
  "Returns an narray of scales with the relative occurrence of each interval over the duration of the anthem rhythm"
  (let* ((scale-histogram (make-double-array (number-of-scales-for-period max-time-limit :voices-per-octave vpo)))
	 (duration-histogram (make-histogram (second anthem)))
	 (intervals (histogram-intervals duration-histogram))
	 ;; Multiply the intervals by the counts to measure the time over which each interval spans.
	 (spans (.* intervals (histogram-counts duration-histogram intervals)))
	 (relative-occurrence (./ spans (.sum spans))) ; make it a relative measure wrt time.
	 (crochet-ratios (./ intervals (float (anthem-beat-period anthem) 1d0))))
    ;; TODO .floor should be .round
    (setf (.arefs scale-histogram (.floor (scale-from-period (.* crochet-ratios crochet-duration) vpo)))
	  relative-occurrence)))

(defun anthems-of-meter (meter)
  (remove-if-not (lambda (x) (equal x meter)) *national-anthems* :key #'cadar))

;; Count the meters in the database. Should match Zaanen et al.
;; (length (anthems-of-meter "4/4"))
;; (length (anthems-of-meter "12/8"))
;; (length (anthems-of-meter "2/4"))
;; (length (anthems-of-meter "3/4"))
;; (length (anthems-of-meter "2/2"))

(defun pad-end-to-length (vector length)
  (let ((pad-length (- length (.length vector))))
    (.concatenate vector (make-double-array pad-length))))

(defun average-ridge-persistency (&key (anthems *national-anthems*) (max-time-limit 16384))
  "Sum the ridge persistencies over each meter and plot the average ridges."
  (loop
     for anthem in anthems
     ;; Since some rhythms are shorter than the maximum, we need to pad the ridge-persistency responses.
     with total-persistency = (make-double-array (number-of-scales-for-period max-time-limit))
     do (setf total-persistency (.+ total-persistency (pad-end-to-length (ridge-persistency-of-anthem anthem)
						   (.length total-persistency))))
     finally (return (./ total-persistency (length anthems)))))

;; (defun scale-histogram-of-anthems (anthems &key (vpo 16) (crochet-duration 100))
;;   "Returns an narray of scales based on histogram counts"
;;   (let* ((anthem-duration (duration-in-samples (anthem-rhythm (first anthems))))
;; 	 (scale-histogram (make-double-array (number-of-scales-for-period anthem-duration :voices-per-octave vpo)))
;; 	 (histogram-hash-table (make-histogram-of-anthem-intervals :anthems anthems))
;; 	 (intervals (histogram-intervals histogram-hash-table)))
;;     ;; TODO should be .round
;;     (setf (.arefs scale-histogram (.floor (scale-from-period (.* intervals crochet-duration) vpo)))
;; 			       (.normalise (histogram-counts histogram-hash-table intervals)))
;;     scale-histogram))

;;; TODO need to time limit histograms
(defun scale-histogram-of-anthems (anthems &key (vpo 16) (max-time-limit 16384))
  "Returns an narray of scales based on histogram counts"
  (loop
     for anthem in anthems
     ;; Since some rhythms are shorter than the maximum, we need to pad the ridge-persistency responses.
     with scale-histogram = (make-double-array (number-of-scales-for-period max-time-limit :voices-per-octave vpo))
     do (setf scale-histogram (.+ scale-histogram (relative-interval-occurrence-for-anthem anthem)))
     finally (return (./ scale-histogram (length anthems)))))

(defun x-axis-pad (rhythmic-beats)
    (mapcar (lambda (x) (list (concatenate 'string "\\n" (first x)) (second x))) rhythmic-beats))

;;;
;;; Plotting routines
;;;

(defun plot-bar-ridges-of-anthem (anthem)
  "Plot the magnitude"
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
					 anthem-rhythm :title (name anthem-rhythm))
      (plot-scale-energy+peaks-at-time rhythm-scaleogram 500 correlated-ridge-scale-peaks))))

(defun plot-scaleogram-skeleton-of-anthem (anthem)
  "Plot the ridges in greyscale and the highlighted ridges in red."
  (multiple-value-bind (matching-ridges rhythm-scaleogram correlated-ridge-scale-peaks) 
      (bar-ridges-for-anthem anthem)
    (declare (ignore matching-ridges))
    (let* ((anthem-rhythm (anthem-rhythm anthem))
	   (time-axis-decimation 4)
	   (formatting "set title font \"Times,20\"~%set xlabel font \"Times,20\"~%set ylabel font \"Times,20\"")
	   (axes-labels (axes-labelled-in-seconds rhythm-scaleogram (sample-rate anthem-rhythm) time-axis-decimation))
	   (highlighted-ridges (list (canonical-bar-ridge anthem rhythm-scaleogram))))
      (format t "Plotting images~%")
      (plot-images (list (list #'magnitude-image 
			       (list (scaleogram-magnitude rhythm-scaleogram))
			       '((1.0 1.0) (0.0 0.3))
			       (concatenate 'string axes-labels formatting))
			 (list #'highlighted-ridges-image
			       (list (mapcar #'copy-object highlighted-ridges)  correlated-ridge-scale-peaks)
			       '((1.0 1.0) (0.0 0.0))
			       (concatenate 'string axes-labels formatting))) ; ':xlabel "Time (Seconds)"
		   :title (name anthem-rhythm)
		   :time-axis-decimation time-axis-decimation))))

;; (plot-scaleogram-skeleton-of-anthem (anthem-named 'tunisia))
;; (plot-scaleogram-skeleton-of-anthem (anthem-named 'vietnam))
;; (plot-scaleogram-skeleton-of-anthem (anthem-named 'america))

(defun plot-anthem-bar-presence (&key (anthems *national-anthems*))
  (let ((bar-ratios (nlisp::list-to-array (bars-in-anthem-skeletons :anthems anthems))))
    (format t "Average bar presence ratio ~4,3f (~4,3f) in ~a~%" 
	    (mean bar-ratios) (stddev bar-ratios) *anthem-analysis-path*)
    (reset-plot)
    (plot-command "set title font \"Times,24\"")
    (plot-command "set xlabel font \"Times,24\"")
    (plot-command "set ylabel font \"Times,24\"")
    ;; TODO Rotate the xtics & drop the font size.
    (plot-command "set xtics rotate (~{~{\"~a\" ~5d~}~^, ~})~%" (label-country anthems))
    ;; (plot-command "set xtics 0,10,~d~%" (length anthems))
    (plot bar-ratios nil ; (.iseq 0 (1- (length anthems)))
	  :style "boxes fill solid border 9" 
	  :title (format nil "Bar presence of ~a" *anthem-analysis-path*) ; "for ~4,2f second excerpts"
	  :label nil
	  :ylabel "Proportion of Bar Duration Present in Skeleton"
	  :xlabel "Anthem" ; or "Anthem Number"
	  :aspect-ratio 0.66
	  :reset nil)))

;; (plot-anthem-bar-presence)

(defun plot-ridge-persistency-for-anthems (anthems description &key (vpo 16) (crochet-duration 100))
  (let* ((arp (average-ridge-persistency :anthems anthems :max-time-limit 16384)) ; (* 48 25)
	 (anthem-bar-scale (bar-scale (first anthems) vpo))
	 (prominent-scale-indices (.+ (.find (.> arp 0.20)) 1)))
    (format t "anthem bar scale ~a prominent scale indices ~a~%period ~a~%"
	    anthem-bar-scale prominent-scale-indices (time-support prominent-scale-indices vpo))
    (reset-plot)
    (plot-command "set title font \"Times,24\"")
    (plot-command "set xlabel ,-1 font \"Times,24\"")
    (plot-command "set ylabel font \"Times,24\"")
    (plot-command "set xtics border (~{~{\"~a\" ~d~}~^, ~}) font \"Sonata,28\"~%" 
		  (x-axis-pad (label-scale-as-rhythmic-beat vpo crochet-duration)))
    (plot arp nil 
	  :title (format nil "Time-Frequency Scale Presence For ~d Anthems ~a"
			 (length anthems) description)
	  :style "lines linetype 3 linewidth 2"
	  :xlabel "Dilation Scales in Units of Musical Time"
	  :ylabel "Proportion of Bar Duration Present in Skeleton"
	  :label nil
	  :aspect-ratio 0.66
	  :reset nil)))

;; (plot-ridge-persistency-for-anthems (anthems-of-meter "4/4") "in 4/4 Meter")

(defun plot-histogram-of-anthems (anthems description &key (crochet-duration 100) (vpo 16))
  "Plots a comparison between the histogram of intervals and the multiresolution ridge presence"
  (reset-plot)
  (plot-command "set title font \"Times,24\"")
  (plot-command "set xlabel ,-1 font \"Times,24\"")
  (plot-command "set ylabel font \"Times,24\"")
  (plot-command "set xtics border (~{~{\"~a\" ~d~}~^, ~}) font \"Sonata,28\"~%" 
		(x-axis-pad (label-scale-as-rhythmic-beat vpo crochet-duration)))
  (let ((bar-scale-index (round (bar-scale (first anthems) vpo))))
    (plot-command "set arrow 1 from ~d,0.4 to ~d,0.3" bar-scale-index bar-scale-index)
    (plot-command "show arrow 1"))
  (nplot (list (scale-histogram-of-anthems anthems) (average-ridge-persistency :anthems anthems))
	 nil
	 :styles '("boxes fill solid border 9" "lines linetype 3 linewidth 2")
	 :legends '("Relative Frequency of Occurrence of Intervals" "Time-Frequency Scale Presence")
	 :xlabel "Dilation Scales in Units of Musical Time"
	 :ylabel "Proportion of Interval Present"
	 :title (format nil "Skeleton Scale Presence vs. Occurrence of Intervals For ~d Anthems ~a"
			(length anthems) description)
	 :reset nil
	 :aspect-ratio 0.66))

;; (plot-histogram-of-anthems (anthems-of-meter "4/4") "in 4/4 Meter")
;; (plot-histogram-of-anthems (anthems-of-meter "3/4") "in 3/4 Meter")

(defun plot-ridge-persistency-for-anthem (anthem &key (voices-per-octave 16) (anthem-path *anthem-analysis-path*))
  (let* ((anthem-rhythm (anthem-rhythm anthem))
	 (bar-scale-index)
	 (beat-scale-index)) 
    (reset-plot)
    (multiple-value-bind (rhythm-skeleton rhythm-scaleogram) ;  correlated-ridge-scale-peaks
	(skeleton-of-rhythm-cached anthem-rhythm 
				   :voices-per-octave voices-per-octave 
				   :cache-directory anthem-path)
      (setf bar-scale-index (round (bar-scale anthem (voices-per-octave rhythm-skeleton))))
      (setf beat-scale-index (round (beat-scale anthem (voices-per-octave rhythm-skeleton))))
      (plot-command "set arrow 1 from ~d,0.3 to ~d,0.2" bar-scale-index bar-scale-index)
      (plot-command "set arrow 2 from ~d,0.3 to ~d,0.2" beat-scale-index beat-scale-index)
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

(defun inspect-anthem (anthem)
  (assess-anthem anthem :anthem-path "/Users/leigh/Data/Anthems")
  (relative-interval-counts anthem)
  (plot-ridge-persistency-for-anthem anthem)
  (window)
  (plot-rhythm (anthem-rhythm anthem))
  (window)
  (plot-bar-ridges-of-anthem anthem))

;;; Low combined value for beat and bar values.
;;; (inspect-anthem (anthem-named 'ghana))
