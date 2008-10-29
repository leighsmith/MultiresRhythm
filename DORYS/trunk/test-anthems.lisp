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

(in-package :dorys)
(use-package :nlisp)
(use-package :multires-rhythm)

(defparameter *anthem-analysis-path* "/Users/leigh/Data/Anthems")

;;; 1 crochet = 100 samples => 120 BPM
(defun anthem-minimum-duration (anthem &key (crochet-duration 100))
  "Returns the duration of the smallest interval in the anthem, in samples, given a fixed tempo."
  (/ crochet-duration (anthem-beat-duration anthem)))

(defun anthem-duration-in-samples (anthem duration)
  "Returns the duration in samples, normalized to equal tempo"
  (first (intervals-in-samples (list duration) :ioi (anthem-minimum-duration anthem))))

(defun anthem-rhythm (anthem)
  "Returns a rhythm instance from the anthem"
  (let* ((anthem-descriptor (first anthem))
	 (anthem-name (symbol-name (first anthem-descriptor)))
	 (anthem-iois (second anthem)))
    (iois-to-rhythm anthem-name anthem-iois :shortest-ioi (anthem-minimum-duration anthem))))

(defun anthem-interval-scale (anthem voices-per-octave interval)
  "Compute the scale index that the interval in the given anthem would activate on its scaleogram"
  (scale-from-period (anthem-duration-in-samples anthem interval) voices-per-octave))

;; (anthem-interval-scale (anthem-named 'australia) 16 (anthem-beat-duration (anthem-named 'australia)))

(defun bar-scale (anthem voices-per-octave)
  "Compute the scale index that the anthem would activate on the scaleogram for the bar"
  (anthem-interval-scale anthem voices-per-octave (anthem-bar-duration anthem)))

(defun beat-scale (anthem voices-per-octave)
  (anthem-interval-scale anthem voices-per-octave (dorys::anthem-beat-duration anthem)))

(defun canonical-bar-ridge (anthem rhythm-scaleogram)
  (make-monotone-ridge (round (bar-scale anthem (voices-per-octave rhythm-scaleogram)))
		       (duration-in-samples rhythm-scaleogram)))
  
;;; Match known bar duration against a ridge & determine how much evidence
;;; there is for the bar duration against the time-frequency ridges. The critical question is
;;; how well bar duration matches against human perception of tactus given the same rhythms?
;;; TODO need to match anacrusis for phase
(defun bar-ridges-for-anthem (anthem &key (voices-per-octave 16) (anthem-path *anthem-analysis-path*))
  "Returns the ridges of the given anthem matching the transcribed bar duration"
  (let* ((anthem-rhythm (anthem-rhythm anthem))
	 (bar-scale-index)
	 (analysis (analysis-of-rhythm-cached anthem-rhythm 
					      :voices-per-octave voices-per-octave 
					      :cache-directory anthem-path))
	 (rhythm-skeleton (skeleton analysis)))
    (setf bar-scale-index (round (bar-scale anthem (voices-per-octave rhythm-skeleton))))
    (format t "bar scale index ~a time-support ~a samples~%" 
	    bar-scale-index 
	    (time-support bar-scale-index (voices-per-octave rhythm-skeleton)))
    (values (ridges-containing-scale rhythm-skeleton bar-scale-index) analysis)))

;; (bar-ridges-for-anthem (anthem-named 'australia))
;; (bar-ridges-for-anthem (anthem-named 'america))
;; (bar-ridges-for-anthem (anthem-named 'france))
;; (bar-ridges-for-anthem (anthem-named 'nepal))
;; (bar-ridges-for-anthem (anthem-named 'sweden))
;; (bar-ridges-for-anthem (anthem-named 'netherlands))

;; (plot-cwt+skeleton-of analysis matching-ridges (anthem-rhythm (anthem# 3)))
;; (plot-cwt+skeleton-of analysis (list (canonical-bar-ridge (anthem# 3) scaleogram)) (anthem-rhythm (anthem# 3)))

;; (defun bar-scale-for-anthem (anthem &key (tactus-selector #'select-longest-lowest-tactus))
;;   "Returns the scale of the given anthem matching the bar duration"
;;   (multiple-value-bind (computed-tactus rhythm-analysis)
;;       (tactus-for-rhythm (anthem-rhythm anthem) :tactus-selector tactus-selector)
;;     (format t "computed tactus ~a~%" computed-tactus)
;;     (bar-scale-number (bar-scale anthem (voices-per-octave (scaleogram rhythm-analysis))))))

(defun anacrusis-notes (anthem)
  "Return the sample times of notes of the anacrusis, prior to the downbeat"
  (let ((anacrusis-duration (anthem-anacrusis-duration anthem)))
    (if (zerop anacrusis-duration)
	(make-integer-array '(0))
	(multires-rhythm::onsets-in-samples 
	 (multires-rhythm::subset-of-rhythm (anthem-rhythm anthem)
					    (list t (1- (* anacrusis-duration
							   (anthem-minimum-duration anthem)))))))))

(defun anacrusis-count (anthem)
  "Return the number of notes of the anacrusis, prior to the downbeat"
  (.length (anacrusis-notes anthem)))

(defun downbeat-number (anthem)
  "Return the 0-base index of the note in the anthem rhythm that the downbeat begins on"
  ;; The downbeat is the next beat after the anacrusis, but we are 0 based, so returning
  ;; the count alone is sufficient.
  (anacrusis-count anthem))

(defun clap-to-anthem (anthem)
  "Uses the transcribed downbeat to begin clapping from"
  (clap-to-rhythm (anthem-rhythm anthem) :start-from-beat (downbeat-number anthem)))

;; (time (clap-to-anthem (anthem-named 'america)))
;; (accompany-rhythm (anthem-rhythm (anthem-named 'america)))

(defun generate-anthem-skeletons (&key (anthems *national-anthems*)
				  (anthem-path *anthem-analysis-path*)
				  (length-limit 16384))
  "Generates and writes the skeleton of the numbered, time limited, anthems"
  (dolist (anthem anthems)
    ;; Limits the maximum length of the rhythm to make the computation time manageable.
    (let* ((anthem-rhythm (limit-rhythm (anthem-rhythm anthem)
					:maximum-samples length-limit)))
      (format t "Processing ~a~%" (name anthem-rhythm))
      (if (not (probe-file (make-pathname :directory anthem-path :name (name anthem-rhythm) :type "skeleton")))
	  (analysis-of-rhythm-cached anthem-rhythm :cache-directory anthem-path)))))

;;; (generate-anthem-skeletons :anthem-path "/Users/leigh/Data/Anthems" :length-limit 16384)
;;; Limits to the first 48 semi-quavers, matching Zaanen
;;; (generate-anthem-skeletons :anthem-path "/Users/leigh/Data/ShortAnthems" :length-limit (* 48 25))
;;; For the Ghanian anthem, which has a period which exceeds the maximum wavelet period.
;;; (generate-anthem-skeletons :anthem-path "/Users/leigh/Data/ShortAnthems" :length-limit 4096)


(defun error-span (scale vpo)
  "Returns the error (i.e. difference) in duration from neighbour scales"
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


#|
;; This is the "right" way to do this, but it requires reading the entire analysis file.
(defun skeleton-of-anthem (anthem &key (voices-per-octave 16) (anthem-path *anthem-analysis-path*))
  "Returns the skeleton of the given anthem."
  (format t "Reading ~a~%" (anthem-name anthem))
  (let* ((anthem-rhythm (anthem-rhythm anthem))
	 (analysis (analysis-of-rhythm-cached anthem-rhythm 
					      :voices-per-octave voices-per-octave 
					      :cache-directory anthem-path)))
    (skeleton analysis)))
|#

;;; Should rename this as skeleton-of-rhythm-cached
(defun skeleton-of-anthem (anthem &key (voices-per-octave 16) (anthem-path *anthem-analysis-path*))
  "Returns the skeleton of the given anthem. We do this by reading the skeleton file only,
since it's small and fast to read rather than the entire analysis files."
  (format t "Reading ~a~%" (anthem-name anthem))
  (let* ((anthem-rhythm (anthem-rhythm anthem))
	 (skeleton-pathname (make-pathname :directory anthem-path :name (multires-rhythm::name anthem-rhythm) :type "skeleton")))
    (multires-rhythm::read-skeleton-from skeleton-pathname)))

(defun ridge-persistency-of-anthem (anthem &key (anthem-path *anthem-analysis-path*))
  (ridge-persistency-of (skeleton-of-anthem anthem :anthem-path anthem-path)))

;; (bar-scale (anthem-named 'america) 16)
;; (.aref america-ridge-persistency 99)
;; (.* x ridge-persistence)
;; (plot-image #'magnitude-image (list correlated-ridge-scale-peaks) '((1.0 0.5) (0.0 0.6))
;;	    (axes-labelled-in-seconds rhythm-scaleogram 200 4))
;; (nplot (list (scale-persistency correlated-ridge-scale-peaks) x) nil)

;; (defun persistency-of-period-in-skeleton (anthem skeleton period)
;;   "Returns the proportion of the degree to which the given period duration is present in the skeleton for the given anthem."
;;   (let* ((period-scale (round (anthem-interval-scale anthem (voices-per-octave skeleton) period)))
;; 	 (ridge-persistence (ridge-persistency-of-skeleton skeleton))))
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
  (let* ((ridge-persistence (ridge-persistency-of-skeleton skeleton))
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
	 (skeleton (read-skeleton-from skeleton-pathname)))
    (persistency-of-period-in-skeleton anthem skeleton period)))

;; (assess-anthem-for-period (anthem-named 'greenland) 20)
;; (assess-anthem-for-period (anthem-named 'greenland) 12)

(defun assess-anthem (anthem &key (anthem-path *anthem-analysis-path*))
  (format t "Reading ~a~%" (anthem-name anthem))
  (let* ((anthem-rhythm (anthem-rhythm anthem))
	 (skeleton-pathname (make-pathname :directory anthem-path :name (name anthem-rhythm) :type "skeleton"))
	 (skeleton (read-skeleton-from-file skeleton-pathname))
	 (beat-persistency (persistency-of-period-in-skeleton anthem skeleton (anthem-beat-duration anthem)))
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
     collect (assess-anthem-for-period anthem (anthem-beat-duration anthem))))

(defun label-country (anthems)
  "Routine to create a labelling based on country."
  (loop
     for anthem-index from 0 below (length anthems) by 5
     collect (list (string (anthem-name (anthem# anthem-index))) anthem-index)))

;; (setf non-bar-ratios (make-narray (loop for anthem in *national-anthems* collect (assess-anthem-for-period anthem 7))))
;; (mean non-bar-ratios)
;; (stddev non-bar-ratios)

;; (setf bar-ratios (bars-in-anthem-skeletons :count count))
;; (mean bar-ratios)
;; (.save bar-ratios "/Users/leigh/Data/anthem-bar-ratios.octave" :format :octave)
;;
;; (setf beat-ratios (bars-in-anthem-skeletons :count count))
;; (mean beat-ratios)
;; (.save beat-ratios "/Users/leigh/Data/anthem-beat-ratios.octave" :format :octave)

(defun check-integrity-of-anthems (anthems)
  "Returns any anthems which have intervals longer than more than 4 crochets.
Ghana (12/8) and Malaya (repeated intervals of 5) are fine."
  (let* ((highest-intervals (make-narray (mapcar (lambda (x) (first (last x))) (crochets-of-anthems anthems))))
	 (suspicious (.find (.> highest-intervals 4))))
    (loop 
       for anthem-index across (val suspicious)
       do (format t "~a~%" (nth anthem-index anthems)))))

(defun relative-interval-occurrence-for-anthem (anthem &key 
						(vpo 16) (crochet-duration 100) (max-time-limit 16384))
  "Returns an narray of scales with the relative occurrence of each interval over the duration of the anthem rhythm"
  (let* ((scale-histogram (make-double-array (number-of-scales-for-period max-time-limit :voices-per-octave vpo)))
	 (duration-histogram (multires-rhythm::make-histogram (second anthem)))
	 (intervals (make-narray (multires-rhythm::get-histogram-elements duration-histogram)))
	 ;; Multiply the intervals by the counts to measure the time over which each interval spans.
	 (spans (.* intervals (make-narray (multires-rhythm::get-histogram-counts duration-histogram))))
	 (relative-occurrence (./ spans (* (.sum spans) 1d0))) ; make it a relative measure wrt time.
	 (crochet-ratios (./ intervals (float (anthem-beat-duration anthem) 1d0))))
    ;; TODO .floor should be .round
    (setf (.arefs scale-histogram (.floor (scale-from-period (.* crochet-ratios crochet-duration) vpo)))
	  relative-occurrence)))

;;; TODO perhaps this can just use the average-ridge-persistency (of rhythms) function?
(defun average-ridge-persistency-of-anthems (anthems &key (max-time-limit 16384))
  "Sum the ridge persistencies over each meter and return the average ridges."
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
;; 	 (histogram-hash-table (multires-rhythm::make-histogram-of-anthem-intervals :anthems anthems))
;; 	 (intervals (make-narray (get-histogram-elements histogram-hash-table))))
;;     ;; TODO should be .round
;;     (setf (.arefs scale-histogram (.floor (scale-from-period (.* intervals crochet-duration) vpo)))
;; 			       (.normalise (get-histogram-counts histogram-hash-table intervals)))
;;     scale-histogram))

;;; TODO need to time limit histograms
(defun scale-histogram-of-anthems (anthems &key (vpo 16) (max-time-limit 16384))
  "Returns an narray of scales based on histogram counts"
  (loop
     for anthem in anthems
     ;; Since some rhythms are shorter than the maximum, we need to pad the scale-persistency responses.
     with scale-histogram = (make-double-array (number-of-scales-for-period max-time-limit :voices-per-octave vpo))
     do (setf scale-histogram (.+ scale-histogram (relative-interval-occurrence-for-anthem anthem)))
     finally (return (./ scale-histogram (length anthems)))))

(defun x-axis-pad (rhythmic-beats)
    (mapcar (lambda (x) (list (concatenate 'string "\\n" (first x)) (second x))) rhythmic-beats))

(defun meter-of-anthem (anthem &key (voices-per-octave 16) (anthem-path *anthem-analysis-path*))
  (let* ((anthem-rhythm (anthem-rhythm anthem))
	 ;;(bar-scale-index)
	 ;;(beat-scale-index)
	 (analysis (analysis-of-rhythm-cached anthem-rhythm 
					      :voices-per-octave voices-per-octave 
					      :cache-directory anthem-path))
	 ;; (rhythm-skeleton (skeleton analysis))
	 (selected-tactus (choose-tactus anthem-rhythm analysis :tactus-selector #'create-weighted-beat-ridge)))
    ;; update the sample rate of the analysis to match the rhythm, since it wasn't stored.
    (setf (sample-rate analysis) (sample-rate anthem-rhythm)) 
    (format t "meter of ~a is ~a~%" (name anthem-rhythm) (meter-of-analysis analysis selected-tactus))))

;;;
;;; Plotting routines
;;;

(defun plot-bar-ridges-of-anthem (anthem)
  "Plot the magnitude"
  (multiple-value-bind (matching-ridges rhythm-analysis) 
      (bar-ridges-for-anthem anthem)
    (let ((anthem-rhythm (anthem-rhythm anthem))
	  (rhythm-scaleogram (scaleogram rhythm-analysis))
	  (correlated-ridge-scale-peaks (ridge-peaks rhythm-analysis)))
      (plot-cwt+skeleton-of rhythm-analysis matching-ridges anthem-rhythm :title (name anthem-rhythm))
      (plot-cwt+skeleton-of rhythm-analysis (list (canonical-bar-ridge anthem rhythm-scaleogram))
			    anthem-rhythm :title (name anthem-rhythm))
      (plot-scale-energy+peaks-at-time rhythm-scaleogram 500 correlated-ridge-scale-peaks))))

(defun plot-scaleogram-skeleton-of-anthem (anthem)
  "Plot the ridges in greyscale and the highlighted ridges in red."
  (multiple-value-bind (matching-ridges rhythm-analysis) (bar-ridges-for-anthem anthem)
    (declare (ignore matching-ridges))
    (let* ((highlighted-ridges (list (canonical-bar-ridge anthem (scaleogram rhythm-analysis)))))
      (format t "Plotting images~%")
      (plot-cwt+skeleton-of rhythm-analysis highlighted-ridges (anthem-rhythm anthem)))))

;; (plot-scaleogram-skeleton-of-anthem (anthem-named 'tunisia))
;; (plot-scaleogram-skeleton-of-anthem (anthem-named 'vietnam))
;; (plot-scaleogram-skeleton-of-anthem (anthem-named 'america))
;; (plot-scaleogram-skeleton-of-anthem (anthem-named 'australia))

(defun plot-anthem-bar-presence (&key (anthems *national-anthems*))
  (let ((bar-ratios (make-narray (bars-in-anthem-skeletons :anthems anthems))))
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
  (let* ((arp (average-ridge-persistency-of-anthems anthems :max-time-limit 16384)) ; (* 48 25)
	 (anthem-bar-scale (bar-scale (first anthems) vpo))
	 (prominent-scale-indices (.+ (.find (.> arp 0.20)) 1)))
    (format t "anthem bar scale ~a prominent scale indices ~a~%period ~a~%"
	    anthem-bar-scale prominent-scale-indices (time-support prominent-scale-indices vpo))
    (reset-plot)
    (plot-command "set title font \"Times,24\"")
    (plot-command "set xlabel offset 0,-1 font \"Times,24\"")
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
;; (plot-ridge-persistency-for-anthems (anthems-of-meter "3/4") "in 3/4 Meter")
;; (plot-ridge-persistency-for-anthems (anthems-of-meter "2/4") "in 2/4 Meter")
;; (plot-ridge-persistency-for-anthems (anthems-of-meter "2/2") "in 2/2 Meter")

(defun plot-histogram-of-anthems (anthems description &key (crochet-duration 100) (vpo 16))
  "Plots a comparison between the histogram of intervals and the multiresolution ridge presence"
  (let* ((bar-scale-index (round (bar-scale (first anthems) vpo)))
	 (average-ridge-presence (average-ridge-persistency-of-anthems anthems))
	 (interval-histogram (scale-histogram-of-anthems anthems))
	 ;; Scales the histogram to match the highest average ridge presence
	 (histogram-scaling (/ (.max average-ridge-presence) (.max interval-histogram))))
    (reset-plot)
    (plot-command "set title font \"Times,24\"")
    (plot-command "set xlabel offset 0,-1 font \"Times,24\"")
    ;; (plot-command "set y2label 'Proportion of Interval Present'")
    (plot-command "set ylabel font \"Times,24\"")
    (plot-command "set xtics border (~{~{\"~a\" ~d~}~^, ~}) font \"Sonata,28\"~%" 
		  (x-axis-pad (label-scale-as-rhythmic-beat vpo crochet-duration)))
    (plot-command "set arrow 1 from ~d,0.40 to ~d,0.35" bar-scale-index bar-scale-index)
    (plot-command "show arrow 1")
    (nplot (list (.* interval-histogram histogram-scaling) average-ridge-presence)
	   nil
	   :styles '("boxes fill solid border 9" "lines linetype 3 linewidth 2")
	   :legends '("Relative Frequency of Occurrence of Intervals" "Time-Frequency Scale Presence")
	   :xlabel "Dilation Scales in Units of Musical Time"
	   :ylabel "Proportion of Interval Present"
	   :title (format nil "Skeleton Scale Presence vs. Occurrence of Intervals For ~d Anthems ~a"
			  (length anthems) description)
	   :reset nil
	   :aspect-ratio 0.66)))

;; (plot-histogram-of-anthems (anthems-of-meter "4/4") "in 4/4 Meter")
;; (plot-histogram-of-anthems (anthems-of-meter "3/4") "in 3/4 Meter")
;; (plot-histogram-of-anthems (anthems-of-meter "2/4") "in 2/4 Meter")
;; (plot-histogram-of-anthems (anthems-of-meter "2/2") "in 2/2 Meter")

;; To scale the histogram to match the 
;; (setf arp (average-ridge-persistency-of-anthems (anthems-of-meter "2/2")))
;; (setf h (scale-histogram-of-anthems (anthems-of-meter "2/2")))
;; (setf scaling (/ (.max arp) (.max h)))

(defun plot-ridge-persistency-for-anthem (anthem &key (voices-per-octave 16) (anthem-path *anthem-analysis-path*))
  (let* ((anthem-rhythm (anthem-rhythm anthem))
	 (bar-scale-index)
	 (beat-scale-index)
	 (analysis (analysis-of-rhythm-cached anthem-rhythm 
					      :voices-per-octave voices-per-octave 
					      :cache-directory anthem-path))
	 (rhythm-skeleton (skeleton analysis)))
    (reset-plot)
    (setf bar-scale-index (round (bar-scale anthem (voices-per-octave rhythm-skeleton))))
    (setf beat-scale-index (round (beat-scale anthem (voices-per-octave rhythm-skeleton))))
    (plot-command "set arrow 1 from ~d,0.3 to ~d,0.2" bar-scale-index bar-scale-index)
    (plot-command "set arrow 2 from ~d,0.3 to ~d,0.2" beat-scale-index beat-scale-index)
    (plot-command "show arrow 1")
    (plot-command "show arrow 2")
    (nplot (list (scale-persistency (scaleogram-magnitude (scaleogram analysis)))
		 ;; (scale-persistency correlated-ridge-scale-peaks) 
		 (ridge-persistency-of-skeleton rhythm-skeleton))
		 nil
		 :reset nil
		 :aspect-ratio 0.66
		 :legends '("magnitude persistency" "skeleton persistency") ; "ridge persistency"
		 ;; :styles '("lines linestyle 1" "lines linestyle 3")
		 :title (format nil "Ridge persistency for ~a" (name anthem-rhythm)))))

;; (plot-ridge-persistency-for-anthem (anthem-named 'america))
;; (plot-ridge-persistency-for-anthem (anthem-named 'netherlands))

;;; "The axis of non-metricality": Anthems that have low bar-ridge measures:
;; (plot-ridge-persistency-for-anthem (anthem-named 'australia))
;; (plot-ridge-persistency-for-anthem (anthem-named 'greenland))
;; (plot-ridge-persistency-for-anthem (anthem-named 'jordan))
;; (plot-ridge-persistency-for-anthem (anthem-named 'vatican))
;; (plot-ridge-persistency-for-anthem (anthem-named 'luxembourg))

(defun plot-anthem-anacrusis-profiles (anthem &key (length-limit 16384))
  "Plots the profiles of the first first-notes number of notes in the rhythm"
  ;; retrieve the first set of onset times of the anacrusis.
  (let* ((analysis (analysis-of-rhythm (limit-rhythm (anthem-rhythm anthem) :maximum-samples length-limit)))
	 (scaleogram (scaleogram analysis))
	 (description (anthem-name anthem))
	 (anacrusis-note-times (anacrusis-notes anthem))
	 (times-in-samples (nlisp::array-to-list anacrusis-note-times))
	 (local-pc (local-phase-congruency (scaleogram-magnitude scaleogram) (scaleogram-phase scaleogram)))
	 (lpc-at-time (./ (.partial-sum local-pc) (.array-dimension local-pc 0)))
	 (pc (phase-congruency (scaleogram-magnitude scaleogram) (scaleogram-phase scaleogram)))
	 (ridges-on-notes (mapcar (lambda (time) (ridges-at-time (skeleton analysis) time)) times-in-samples)))
    (format t "times-in-samples ~a~%" times-in-samples)
    (format t "number of ridges at notes ~a~%" (mapcar #'length ridges-on-notes))
    (format t "phase congruency at notes ~a~%" (.arefs pc anacrusis-note-times))
    (format t "summed local phase congruency at notes ~a~%" (.arefs lpc-at-time anacrusis-note-times))
    (format t "ridge ratios ~a~%" (ridge-ratios-at-time (first ridges-on-notes) (caar ridges-on-notes) (first times-in-samples)))
    (plot-cwt+skeleton-of analysis '() (anthem-rhythm anthem) :title description)
    (window)
    (nplot (list pc) nil :title (format nil "phase congruency of ~a" description) :aspect-ratio 0.66)
    (window)
    (let ((bar-scale-index (- (number-of-scales scaleogram)
			      (round (bar-scale anthem (voices-per-octave scaleogram)))))
	  (beat-scale-index (- (number-of-scales scaleogram)
			       (round (beat-scale anthem (voices-per-octave
							  scaleogram))))))
      (format t "beat scale index ~a, bar scale index ~a~%" beat-scale-index bar-scale-index)
      (reset-plot)
      (plot-command "set arrow 1 from ~d,0.2 to ~d,0.15" bar-scale-index bar-scale-index)
      (plot-command "set arrow 2 from ~d,0.2 to ~d,0.15" beat-scale-index beat-scale-index)
      (plot-command "show arrow 1")
      (plot-command "show arrow 2")
      (plot-scale-energy-at-times scaleogram times-in-samples :description description))
    ridges-on-notes))

;; (setf american-ridges (plot-anthem-anacrusis-profiles (anthem-named 'america)))
;; (setf australian-ridges (plot-anthem-anacrusis-profiles (anthem-named 'australia)))
;; (anthem-anacrusis (plot-anthem-named-profiles 'vietnam))
;; (anthem-anacrusis (plot-anthem-named-profiles 'finland))

(defun accented-anthem-rhythm (anthem)
  "Accents the anthem rhythm according to the transcribed anacrusis and measure"
  (accent-rhythm (anthem-rhythm anthem) 
		 (anthem-duration-in-samples anthem (anthem-anacrusis-duration anthem))
		 (anthem-duration-in-samples anthem (anthem-bar-duration anthem))))

(defun plot-scaleogram-skeleton-of-anthem-accented (anthem &key (voices-per-octave 16))
  "Plot the ridges in greyscale and the highlighted ridges in red."
  (let* ((anthem-rhythm (accented-anthem-rhythm anthem))
	 (analysis (analysis-of-rhythm anthem-rhythm :voices-per-octave voices-per-octave))
	 (rhythm-scaleogram (scaleogram analysis))
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
			     (list (mapcar #'copy-object highlighted-ridges) (ridge-peaks analysis))
			     '((1.0 1.0) (0.0 0.0))
			     (concatenate 'string axes-labels formatting))) ; ':xlabel "Time (Seconds)"
		 :title (name anthem-rhythm)
		 :time-axis-decimation time-axis-decimation)))

;; (plot-scaleogram-skeleton-of-anthem-accented (anthem-named 'america))

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

;;; TODO  :anthem-path "/Users/leigh/Data/ShortAnthems"
;; (beat-period-of-rhythm (anthem-rhythm (anthem-named 'australia)) (skeleton-of-anthem (anthem-named 'australia)))

(defun evaluate-beat-period-of-anthem (anthem &key (anthem-path *anthem-analysis-path*))
  "Compare the computed beat level against the transcribed anthem beat period."
  (let* ((skeleton (skeleton-of-anthem anthem :anthem-path anthem-path))
	 (vpo (voices-per-octave skeleton))
	 (beat-period (beat-period-of-rhythm (anthem-rhythm anthem) skeleton))
	 (beat-scale (scale-from-period beat-period vpo))
	 (official-beat-scale (beat-scale anthem vpo))
	 (official-beat-period (time-support official-beat-scale vpo)))
    (format t "beat-scale ~a, beat-period ~a~%" beat-scale beat-period)
    (format t "official beat-scale ~a, official beat period ~a~%" 
	    official-beat-scale official-beat-period)
    (format t "ratio of beat period to official beat period ~a~%" (/ beat-period official-beat-period))
    (<= (abs (- (round beat-scale) (round official-beat-scale))) 1))) ; within +/- 1 scale of the official beat scale.

;; (evaluate-beat-period-of-anthem (anthem-named 'australia))
;; (evaluate-beat-period-of-anthem (anthem-named 'australia) :anthem-path "/Users/leigh/Data/ShortAnthems")

;;; TODO Probably could tune (skew) the tempo weighting envelope.
;;; (setf bad-beat-periods (evaluation-of-anthems #'evaluate-beat-period-of-anthem))
;;; #<FUNCTION EVALUATE-BEAT-PERIOD-OF-ANTHEM> 12 failed, correct 88.57143%
;;;
;;; However many anthems which are not correct are producing beat estimates which are half the length.
;;; Perhaps try shorter time periods from ShortAnthems?

;; (let ((r (anthem-rhythm (anthem-rhythm anthem)))
;;       (beat-period (beat-period-of-rhythm r (skeleton-of-anthem anthem :anthem-path anthem-path))))
;;   (find-downbeat r beat-period)

;;; This assumes that no meter changes occur in the intervening period, so the Netherlands
;;; is wrongly evaluated, for example.
;;; Conservative doesn't work for Tunisia. Opportune does.
(defun evaluate-downbeat-of-anthem-old (anthem)
  "Returns T if the downbeat chosen aligns on a measure boundary with the official downbeat"
  (let* ((official-downbeat (downbeat-number anthem))
	 (vpo 16)			; should be determined from scaleogram.
	 (official-beat-period (time-support (beat-scale anthem vpo) vpo))
	 (beat-period (beat-period-of-rhythm (anthem-rhythm anthem) (skeleton-of-anthem anthem)))
	 (found-downbeat (find-downbeat (anthem-rhythm anthem) beat-period :strategy #'is-greater-rhythmic-period))
	 ;; (found-downbeat (find-downbeat (anthem-rhythm anthem) beat-period :strategy #'<))
	 ;; (found-downbeat (find-downbeat (anthem-rhythm anthem) official-beat-period :strategy #'is-greater-rhythmic-period))
	 ;; (found-downbeat (find-downbeat (anthem-rhythm anthem) official-beat-period :strategy #'<))
	 (bar-duration (anthem-bar-duration anthem))
	 (total-duration-between)	; between official & found downbeats.
	 (position-within-bar))
    (format t "official downbeat ~a found downbeat ~a~%" official-downbeat found-downbeat)
    (format t "official beat period ~a computed beat period ~a~%" official-beat-period beat-period)
    (cond ((not found-downbeat)
	   (progn
	     (format t "Early exit, found downbeat is nil, couldn't find beat period~%")
	     nil))
	  ((< found-downbeat official-downbeat)
	   (progn
	     (format t "Early exit, found downbeat before official downbeat~%")
	     nil))
	  (t 
	   (progn
	     (setf total-duration-between (reduce #'+ (second anthem) :start official-downbeat :end found-downbeat))
	     (setf position-within-bar (mod total-duration-between bar-duration))
	     (format t "bar-duration ~a total-duration-between ~a position within bar ~a~%" 
 		     bar-duration total-duration-between position-within-bar)
	     (zerop position-within-bar))))))

;;; (setf bad-downbeats (evaluation-of-anthems #'evaluate-downbeat-of-anthem-old))
;;; #<FUNCTION EVALUATE-DOWNBEAT-OF-ANTHEM-OLD> 32 failed, correct 69.52381%

(defun evaluate-downbeat-of-anthem-recent (anthem)
  "Returns T if the downbeat chosen aligns on a measure boundary with the official downbeat"
  (let* ((vpo 16)			; should be determined from scaleogram.
	 (pattern (second anthem))
	 (official-downbeat (anthem-anacrusis-duration anthem))
	 (official-bar-duration (anthem-bar-duration anthem)) ; in anthem rhythmic units
	 (official-beat-period (time-support (beat-scale anthem vpo) vpo)) ; in samples.
	 ;; official-beat-period & beat-period's contribution are the same in this case.
	 (rhythm (anthem-rhythm anthem))
	 (beat-period (multires-rhythm::beat-period-of-rhythm rhythm (skeleton-of-anthem anthem)))
	 (found-downbeat-number (multires-rhythm::find-downbeat rhythm beat-period :strategy #'multires-rhythm::is-greater-rhythmic-period))
	 (found-anacrusis-duration (reduce #'+ (subseq (second anthem) 0 found-downbeat-number))))
    (format t "official downbeat ~a found downbeat ~a~%" official-downbeat found-anacrusis-duration)
    (format t "official beat period ~a computed beat period ~a~%" official-beat-period beat-period)
    (format t "Strictly matches ~a~%" (= found-anacrusis-duration (anthem-anacrusis-duration anthem)))
    (if (not found-downbeat-number)
	(format t "Early exit, found downbeat is nil, couldn't find beat period~%")
	(correct-from-anthem-tree pattern found-anacrusis-duration official-bar-duration))))

#|
(defun evaluate-downbeat-of-rhythm (rhythm pattern beat-period bar-period official-downbeat-duration)
  "Compares the found downbeat against the ground truth value"
;;  (let* ((found-downbeat-number (multires-rhythm::find-downbeat rhythm beat-period :strategy #'multires-rhythm::is-greater-rhythmic-period))
  (let* ((found-downbeat-number (multires-rhythm::find-downbeat-new rhythm beat-period))
	 (found-anacrusis-duration (reduce #'+ (subseq pattern 0 found-downbeat-number)))
	 (minimum-anacrusis-duration (mod found-anacrusis-duration bar-period)))
    (format t "Official downbeat duration ~a, found downbeat at event #~a, duration of anacrusis ~a~%"
	    official-downbeat-duration found-downbeat-number found-anacrusis-duration)
    (if (not found-downbeat-number)
	(progn (format t "Early exit, found downbeat is nil, couldn't find beat period~%") nil)
	(= minimum-anacrusis-duration official-downbeat-duration))))
|#

(defun evaluate-downbeat-of-rhythm (rhythm pattern beat-period bar-period official-downbeat-duration)
  "Compares the found downbeat against the ground truth value"
;;  (let* ((found-downbeat-number (multires-rhythm::find-downbeat-new rhythm beat-period))
  (let* ((found-downbeat-number (multires-rhythm::find-downbeat-scores rhythm beat-period))
	 (found-anacrusis-duration (reduce #'+ (subseq pattern 0 found-downbeat-number))))
    (format t "Official downbeat duration ~a, found downbeat at event #~a, duration of anacrusis ~a~%"
	    official-downbeat-duration found-downbeat-number found-anacrusis-duration)
    (if (not found-downbeat-number)
	(progn (format t "Early exit, found downbeat is nil, couldn't find beat period~%") nil)
	(= found-anacrusis-duration official-downbeat-duration))))

(defun evaluate-downbeat-of-anthem (anthem)
  "Returns T if the downbeat chosen aligns on a measure boundary with the official downbeat"
  (let* ((vpo 16)			; should be determined from scaleogram.
	 (pattern (second anthem))
	 (official-downbeat (anthem-anacrusis-duration anthem))
	 (official-bar-duration (anthem-bar-duration anthem)) ; in anthem rhythmic units
	 (official-beat-period (time-support (beat-scale anthem vpo) vpo)) ; in samples.
	 (rhythm (anthem-rhythm anthem))
	 ;; official-beat-period & beat-period's contribution are the same in this case.
	 (beat-period (multires-rhythm::beat-period-of-rhythm rhythm (skeleton-of-anthem anthem)))
	 (matched (evaluate-downbeat-of-rhythm rhythm pattern official-beat-period official-bar-duration official-downbeat)))
    (format t "official beat period ~a computed beat period ~a~%" official-beat-period beat-period)
    (format t "Strictly matches ~a~%" matched)
    matched))

;; To get anthems with an anacrusis:
;; (setf anthems-with-anacrusis (remove-if #'zerop *national-anthems* :key #'anthem-start-at))
;; How many rhythms have an anacrusis?
;; (/ (length (remove-if #'zerop *national-anthems* :key #'anthem-start-at)) (length *national-anthems*)))

;; (plot-anacrusis-profiles (anthem-rhythm (anthem-named 'netherlands)) (downbeat-number (anthem-named 'netherlands))


;;; (setf bad-downbeats (evaluation-of-anthems #'evaluate-downbeat-of-anthem-recent))
;;; #<FUNCTION EVALUATE-DOWNBEAT-OF-ANTHEM> 28 failed, correct 73.333336%

;;; (setf bad-downbeats (evaluation-of-anthems #'evaluate-downbeat-of-anthem))


;;; Perhaps split into bar-period-of-rhythm and downbeat-of-rhythm?
(defun bar-period-and-phase-of-rhythm (performed-rhythm &key (tactus-selector #'select-longest-lowest-tactus))
  "Returns the period and phase of the rhythm in samples. Need to convert back to rhythmicly useful units."
  (let* ((rhythm-analysis (analysis-of-rhythm-cached performed-rhythm :cache-directory *anthem-analysis-path*))
	 (vpo (voices-per-octave (scaleogram rhythm-analysis)))
	 (computed-tactus (funcall tactus-selector performed-rhythm rhythm-analysis))
	 (tactus-list (if (not (listp computed-tactus)) (list computed-tactus) computed-tactus))
	 (meter-factor (meter-of-analysis rhythm-analysis tactus-list))
	 (beat-period (beat-period-of-rhythm performed-rhythm (skeleton rhythm-analysis)))
	 ;; Return which beat is the downbeat.
	 (found-downbeat (find-downbeat performed-rhythm beat-period :strategy #'is-greater-rhythmic-period))
	 ;; Compare the average against the actual scales.
	 ;; (tactus-scales (make-narray (scales-in-ridges tactus-list)))
	 ;; (tactus-scales (average-scale-in-ridges tactus-list))
	 (tactus-scales (modal-scale-in-ridges (first tactus-list))) ;; TODO kludged as first.
	 (likely-bar-periods (.* (time-support tactus-scales vpo) meter-factor)))
    (format t "range of scales ~a periods (samples) ~a meter factor ~a~%"
	    tactus-scales likely-bar-periods meter-factor)
    (format t "downbeat on note # ~a, samples ~a~%"
	    found-downbeat (onset-time-of-note performed-rhythm found-downbeat))
    (values likely-bar-periods (onset-time-of-note performed-rhythm found-downbeat))))

;; Evaluate using the same method as anthem-evaluation.lisp in LHL_BeatInduction
(defun evaluate-mrr-with-tree-on-anthem (anthem)
  "Evaluate using the anthem tree method in shoe"
  (multiple-value-bind (bar-period-candidates downbeat) 
      (bar-period-and-phase-of-rhythm (anthem-rhythm anthem) :tactus-selector #'create-weighted-beat-ridge)
    (let* ((pattern (second anthem))
	   (official-bar-period (anthem-bar-duration anthem))
	   ;; official-downbeat is interval from first note to downbeat.
	   (official-downbeat (anthem-anacrusis-duration anthem)) 
	   (minimum-interval-in-samples (anthem-minimum-duration anthem))
	   (bar-period-in-note-units (.round (./ bar-period-candidates minimum-interval-in-samples)))
	   (downbeat-in-note-units (floor downbeat minimum-interval-in-samples)))
    (format t "official anacrusis duration ~a official bar period ~a~%" official-downbeat official-bar-period)
    (format t "computed anacrusis duration ~a computed bar period ~a~%" downbeat-in-note-units bar-period-in-note-units)
    (correct-from-anthem-tree pattern downbeat-in-note-units bar-period-in-note-units))))

;; (evaluate-mrr-with-tree-on-anthem (anthem-named 'australia))
;; (evaluate-mrr-with-tree-on-anthem (anthem-named 'america))
;; (evaluate-mrr-with-tree-on-anthem (anthem-named 'yugoslavia))
;; (evaluate-mrr-with-tree-on-anthem (anthem-named 'vietnam))
;; (evaluate-mrr-with-tree-on-anthem (anthem-named 'yemen))

;;; (setf bad-beat-or-phase (evaluation-of-anthems #'evaluate-mrr-with-tree-on-anthem))
;; #<FUNCTION EVALUATE-MRR-WITH-TREE-ON-ANTHEM> 69 failed, correct 34.285713%

;; Evaluate using just the beat period (not the bar period) and phase.
(defun evaluate-mrr-beat-period-phase-of-anthem (anthem)
  (and (evaluate-beat-period-of-anthem anthem)
       (evaluate-downbeat-of-anthem anthem)))

;; (evaluate-mrr-beat-period-phase-of-anthem (anthem-named 'australia))
;;; Of course, this is not really valid, how do we check LHL82 using the same measure?
;;; (setf bad-beat-or-phase (evaluation-of-anthems #'evaluate-multires-beat-period-phase-of-anthem))

;;; Evaluate with bar-period generated with meter-division and meter-of-analysis
(defun meter-factor-of-anthem (anthem)
  "Returns if the meter factor of the rhythm matches (in an overly strict fashion) the official meter."
  (let* ((anthem-rhythm (anthem-rhythm anthem))
	 (rhythm-analysis (analysis-of-rhythm-cached anthem-rhythm :cache-directory *anthem-analysis-path*))
	 (computed-tactus (create-weighted-beat-ridge anthem-rhythm rhythm-analysis))
	 (tactus-list (if (not (listp computed-tactus)) (list computed-tactus) computed-tactus))
	 (meter-factor (meter-division rhythm-analysis tactus-list))
	 (official-meter-signature (second (first anthem)))
	 (official-meter-numerator (meter-numerator official-meter-signature)))
    (format t "official meter ~a official meter numerator ~a computed meter factor ~a~%" 
	    official-meter-signature official-meter-numerator meter-factor)
    (zerop (mod official-meter-numerator meter-factor))))

;;; (setf bad-meter-periods (evaluation-of-anthems #'meter-factor-of-anthem))

(defun save-anthem-rhythms (&key (anthems *national-anthems*))
  "Save all anthems as scorefiles for audition"
  (dolist (anthem anthems)
    (save-rhythm (anthem-rhythm anthem)
		 :directory "/Users/leigh/Research/Data/Anthems/MIDI")))

;; foreach f (*.score); convertscore -m `basename $f score`midi $f; end

(defun strip-anthem-of-anacrusis (anthem)
  "Returns the IOIs with the anacrusis removed."
  (onsets-to-iois (remove-if #'minusp 
			     (iois-to-onsets (second anthem) 
					     (- (anthem-anacrusis-duration anthem))))))

;;; Only test anthems that are measured to minimum IOI of 16ths.
;;; (setf waltz-anthems-16ths (remove-if-not (lambda (a) (equal (anthem-beat-duration a) 4)) (anthems-of-meter "3/4")))
;;; (setf common-anthems-16ths (remove-if-not (lambda (a) (equal (anthem-beat-duration a) 4)) (anthems-of-meter "4/4")))

;;; The profile is not quite canonical for the anthems, when looked at across all anthems.
;;; 3/4
;; (plot-metrical-profile (mapcar #'strip-anthem-of-anacrusis waltz-anthems-16ths) "3/4" "")
;;; 4/4
;; (plot-metrical-profile (mapcar #'strip-anthem-of-anacrusis common-anthems-16ths) "4/4" "")

;;; and individually
(defun each-metrical-profile (anthems-in-16ths meter)
  (let ((anthem-number -1))
    (dolist (anthem-iois (mapcar #'strip-anthem-of-anacrusis anthems-in-16ths))
      (format t "anthem ~a~%" (nth (incf anthem-number) anthems-in-16ths))
      (plot-metrical-profile (list anthem-iois) meter)
      (read-line))))

(defun metrical-profile-of-anthem (anthem meter)
  (let ((anthem-rhythm (anthem-rhythm anthem))
	(anthem-iois (strip-anthem-of-anacrusis anthem)))
    (format t "iois ~a~%" anthem-iois)
    ;; 25 samples per semiquaver.
    ;; (onsets-in-samples anthem-rhythm)
    ;; (plot-ridge-persistency-for-anthems (list anthem) (anthem-name anthem))
    (plot-metrical-profile (list anthem-iois) meter)
    ;; We get a nice peak at 300.4 corresponding to 3 crotchets.
    (ridge-persistency-of anthem-rhythm)))

;; (metrical-profile-of-anthem (anthem-named 'america) '(3 2 2))

;;; Compare frequency of intervals against the generated versions.
;; (multires-rhythm::get-histogram (interval-histogram (list (second (anthem-named 'america)))))

;;; Most anthems don't demonstrate a canonical metrical profile, often lacking any beats
;;; falling on minor metrical positions.
;; (plot-ridge-persistency (average-ridge-persistency waltz-anthem-rhythms)
;;			(scaleogram-of-rhythm (first waltz-anthem-rhythms))
;;			"Average ridge persistency of waltz anthem rhythms")

;; (print-elements-and-counts (make-histogram (second (anthem-named 'america))))

