;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for generating expectations of metrical rhythms.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2007
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

;; (declare (optimize (debug 3))) ;; Allows sldb to find our problems quicker.

;; 20mS
(defun make-envelope (amplitude &key (sample-rate 200) (attack 0.020) (decay 0.0) (sustain 0.0) (release 0.020))
  (let* ((attack-samples (round (* attack sample-rate)))
	 (release-samples (round (* release sample-rate)))
	 (duration-samples (round (* sustain sample-rate)))
	 (sustain-samples (- duration-samples attack-samples release-samples)))
    (.concatenate (.rseq 0.0 amplitude attack-samples) 
		  (.rseq amplitude amplitude sustain-samples)
		  (.rseq amplitude 0.0 release-samples))))

(defun rhythm-of-part (name note-list &key 
		       (sample-rate 200) 
		       (attack 0.020) 
		       (release 0.020)
		       (make-envelope-p nil))
  "Generate a rhythm from a note-list specifying time, duration (both in seconds) & amplitude"
  (let* ((last-note (first (last note-list)))
	 (rhythm-length (ceiling (* (+ (first last-note) (second last-note)) sample-rate)))
	 (time-signal (make-double-array rhythm-length))
	 (attack-samples (round (* attack sample-rate)))
	 (release-samples (round (* release sample-rate))))
    (loop
       for (time duration amplitude) in note-list
       for onset-time-in-samples = (round (* time sample-rate))
       for duration-samples = (round (* duration sample-rate))
       for sustain-samples = (- duration-samples attack-samples release-samples)
       do (if make-envelope-p
	      ;; TODO replace with make-envelope if we want it.
	      (let ((envelope (.concatenate (.rseq 0.0 amplitude attack-samples) 
					    (.rseq amplitude amplitude sustain-samples)
					    (.rseq amplitude 0.0 release-samples))))
		(setf (.subarray time-signal (list 0 (list onset-time-in-samples 
							   (+ onset-time-in-samples duration-samples))))
		      envelope))
	      (setf (.aref time-signal onset-time-in-samples) (coerce amplitude 'double-float))))
    (make-instance 'rhythm
		   :name name
		   :description name
		   :time-signal time-signal
		   :sample-rate sample-rate)))

;; Read Temperley's Melisma "notefiles". These are described as:
;;  in "notelist" format, "Note [ontime] [offtime] [pitch]", where ontime and offtime
;; are in milliseconds and pitch is an integer, middle C = 60.
;; There are however, other lines in the file, which need pruning.
(defun part-of-melisma-file (filepath)
  "Reads the data file, creates a note-list"
  (with-open-file (input-stream filepath)
    (loop
       for note-type = (read input-stream nil)
       while note-type
       if (string-equal note-type "Note")
       collect
	 (let ((on-time (read input-stream))
	       (off-time (read input-stream))
	       (midi-note (read input-stream)))
	   ;; (format t "~a, ~a, ~a, ~a~%" note-type on-time off-time midi-note)
	   (list (/ on-time 1000.0d0) (/ (- off-time on-time) 1000.0d0) 1.0 midi-note))
       else
       do (read-line input-stream nil))))

(defun rhythm-of-melisma-file (filepath)
  "Returns a rhythm object from the melisma file"
  (let ((melisma-note-list (part-of-melisma-file filepath)))
    (rhythm-of-part (pathname-name filepath) melisma-note-list)))

(defun make-probe-rhythms (name weighted-onsets &key (probe-base-time 3.0d0) 
			    (probe-note-ratios '(1/6 1/4 1/3 1/2 2/3 3/4 5/6)))
  "Returns a list of rhythms with the probe notes inserted at each location from weighted onsets"
  (mapcar (lambda (probe-note-ratio)
	    ;; Insert in the correct location
	    (let* ((probe-note-time (+ probe-base-time probe-note-ratio))
		   (rhythm-name (format nil "~a with ~a probe" name probe-note-ratio)))
	      (rhythm-of-weighted-onsets rhythm-name (cons (list probe-note-time 1.0) weighted-onsets))))
	  probe-note-ratios))

(defun analyse-rhythms (rhythms)
  (mapcar (lambda (probe-rhythm)
	    (let* ((probe-analysis (analysis-of-rhythm probe-rhythm :padding #'causal-pad)))
	      (plot-cwt-of-rhythm (scaleogram probe-analysis) probe-rhythm)
	      probe-analysis)) rhythms))

#|

;;; Just needs the syncopation package loaded.
(defun metric-scale (meter &key (max-amp 1.0d0) (min-amp 0.12d0))
  "Derive from Longuet-Higgin's & Lee's metric salience measure (which is inverted in
   polarity) an amplitude weighting"
  (let* ((lhl-metric-salience (make-narray (syncopation::lh-metric-salience meter)))
	 (max-salience (.max lhl-metric-salience))
	 (min-salience (.min lhl-metric-salience))
	 (amp-scaling (/ (- max-salience min-salience) (- max-amp min-amp))))
    ;; TODO need to scale the minimum above 0.
    (.+ 1d0 (./ (.* 1d0 lhl-metric-salience) amp-scaling))))

(defun metrically-scaled-rhythm (meter bars tempo)
  "Returns a rhythm with weighted onsets matching the metrical structure"
  (let* ((metrical-weights (nlisp::array-to-list (metric-scale meter)))
	 (times (nlisp::array-to-list (.rseq 0 1.0d0 (1+ (reduce #'* meter))))))
    (rhythm-of-weighted-onsets "metrical scaling" (mapcar #'list times metrical-weights))))
|#

;;;; 

;;; Randomly generate metrical rhythms of a fixed meter, with no upbeats.
;;; TODO Need to specify tempo, either directly or with shortest-ioi.
(defun random-rhythm-of-meter (meter number-of-bars 
			       &key (quarter-note-IOI 0.600) 
			       (max-division-of-crotchet 4)
			       (sample-rate 200.0))
  (let* ((bar-length (reduce #'* meter))
	 (rhythm-name (format nil "~d bar random rhythm in ~a meter" number-of-bars meter))
	 (random-metrical-rhythm (shoe::pattern-from-grammar-long-segment 0 (* number-of-bars bar-length)
									  bar-length meter)))
    ;; Assume 16ths are the minimum division
    (values (iois-to-rhythm rhythm-name 
			    random-metrical-rhythm 
			    :shortest-ioi (/ (* sample-rate quarter-note-IOI) max-division-of-crotchet)
			    :sample-rate sample-rate)
	    random-metrical-rhythm)))

;; (setf random-44 (random-rhythm-of-meter '(2 2 2 2) 2))
;; (random-rhythm-of-meter '(3 2 2) 2)
;; (random-rhythm-of-meter '(3 2) 2)

(defun probed-random-rhythm-of-meter (meter)
  (let* ((metrical-rhythm (random-rhythm-of-meter meter 2))
	 (empty+probe-rhythms (make-probe-rhythms "empty+probe" '((0.0 1.0) 
								    (1.0 1.0)
								    (2.0 0.0)) :probe-base-time 1.0d0)))
    (mapcar (lambda (probe-rhythm) (append-rhythm metrical-rhythm probe-rhythm)) empty+probe-rhythms)))

;; (setf x (probed-random-rhythm-of-meter '(2 2 2 2)))

(defun random-offset (max-length)
  "Defines a random chunk of silence at the end of the rhythm to ensure it is not the
  regularity of the analysis region which biases the ridge."
  (let* ((region-length (random max-length)))
    (make-instance 'rhythm 
		   :name "offset"
		   :time-signal (make-double-array region-length))))

(defun make-metrical-set (candidate-meter size-of-rhythm-set number-of-bars)
  "Return a set of unique metrical rhythms"
  (loop ; repeat size-of-rhythm-set ;; needs to be 
     while (< (length metrical-interval-set) size-of-rhythm-set)
     for (rhythm intervals) = (multiple-value-list (random-rhythm-of-meter candidate-meter number-of-bars))
     when (not (member intervals metrical-interval-set :test #'equal))
     collect intervals into metrical-interval-set
     and collect rhythm into metrical-rhythm-set
     ;; and collect (append-rhythm rhythm (random-offset 100)) into metrical-rhythm-set
     ;; (append rhythm (rhythm-of-weighted-onsets "empty duration" '((0.0 1.0) (1.0 0.0))))
     finally (return (values metrical-rhythm-set metrical-interval-set))))

(defun write-rhythms (rhythms name)
  (loop
     for rhythm in rhythms
     for index = 1 then (1+ index)
     do (write-as-audio rhythm
			(make-pathname :directory "/Volumes/iDisk/Research/Data/RicardsOnsetTests/UnAccented/"
				       :name (format nil "~a-~d" name index)
				       :type "wav")
			#P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff")))

;;; Sum all values falling within the given bins ranges. Effectively integrate.
;;; Could sort by time value. Return the sum and the average of the confidences in the bins.
(defun time-bins (times values-at-times &key (bin-size 0.025))
  "Group the expectations into bins of time, specified by bin-size (in seconds). Returns
   the number of elements in each bin, the accumulated confidences and the boundaries."
  ;; round up to capture all times, including a perfectly dividing edge.
  (let* ((number-of-bins (1+ (floor (range times) bin-size)))
	 (last-bin (+ (.min times) (* (1- number-of-bins) bin-size))) ; for low edge of bin.
	 (bin-boundaries (.rseq (.min times) last-bin number-of-bins))
	 (accumulated-confidences (make-double-array number-of-bins))
	 (bin-counts (make-double-array number-of-bins)))
    (loop
       for bin-index from 0 below number-of-bins
       ;; determine which elements in times (& therefore values-at-times) are within the bin.
       for in-bin = (.and (.>= times (.aref bin-boundaries bin-index))
			  ;; catch the edge of last bin.
			  (.< times (if (= bin-index (1- number-of-bins))
					(+ last-bin bin-size) 
					(.aref bin-boundaries (1+ bin-index)))))
       for in-bin-count = (.sum in-bin)  ; number of occurances.
       do (progn
	    ;; (format t "from ~a to ~a ~a values~%" (.aref bin-boundaries (1- bin-index)) (.aref bin-boundaries bin-index) in-bin-count)
	    (setf (.aref accumulated-confidences bin-index) (.sum (.* in-bin values-at-times)))
	    (setf (.aref bin-counts bin-index) in-bin-count))
       finally (return (values bin-counts bin-boundaries accumulated-confidences)))))

(defun plot-expectancies (all-expectations title)
  (let ((expect-times (make-narray (mapcar (lambda (expect) (time-in-seconds expect 200.0)) all-expectations)))
	(expect-confidences (make-narray (mapcar (lambda (expect) (confidence expect)) all-expectations))))
    (window)
    (plot expect-confidences expect-times 
 	  :style "points" :xlabel "Time" :ylabel "Confidence" 
 	  :aspect-ratio 0.66
 	  :title title)
    (close-window)))

(defun plot-time-histogram (times title &key (bin-size 0.025))
  (multiple-value-bind (counts time-bins)
      (time-bins times (make-integer-array (.length times) :initial-element 1) :bin-size bin-size)
    (format t "count ~a~%time-bins ~a~%" counts time-bins)
    (window)
    (reset-plot)
    (plot-command "set title font \"Times,24\"")
    (plot-command "set xlabel font \"Times,24\"")
    (plot-command "set ylabel font \"Times,24\"")
    (plot-command "set xtics ~a out" (max bin-size 0.2))
    ;; Downsample the time bin lables.
    ;; (.arefs time-bins (.* (.iseq 0 (/ (.length time-bins) 2)) 2))
    (plot counts time-bins
	  :style "boxes fill solid 1.0 border -1"
	  :xlabel title
	  :ylabel "Occurrence"
	  :label (format nil "~a Occurrence" title)
	  :aspect-ratio 0.66
	  :reset nil
	  :title (format nil "Occurrence of ~a" title)))
  (close-window))

(defun plot-expectancies-histogram (times time-values title &key (bin-size 0.025))
  (multiple-value-bind (counts time-bins accumulated-time-values)
      (time-bins times time-values :bin-size bin-size)
    ;;(format t "prediction count ~a~%time-bins ~a~%accumulated-time-values ~a~%" 
    ;;	    counts time-bins accumulated-time-values)
    (let ((dividable-counts (.+ (.not counts) counts))
	  (minimum-tics 0.1))
      ;; (format t "averaged time-values ~a~%" (./ accumulated-time-values dividable-counts))
      (diag-plot 'interval-occurrence
	(plot-command "set xtics 0.1 out")
	(plot counts time-bins
	      :style "boxes fill solid 1.0 border -1"
	      :xlabel "Divisions of a Measure" 
	      :ylabel "Occurrence"
	      :label "Expected Interval Occurrence"
	      :aspect-ratio 0.66
	      :reset nil
	      :title (format nil "Occurrence of Interval Expectations for ~a" title)))
      (window)
      (reset-plot)
      (plot-command "set title font \"Times,22\"")
      (plot-command "set xlabel font \"Times,24\"")
      (plot-command "set ylabel font \"Times,24\"")
      (plot-command "set xtics ~a out" (max bin-size minimum-tics))
      (plot accumulated-time-values time-bins
	    :style "boxes fill solid 1.0 border -1"
	    :xlabel "Divisions of a Measure" 
	    :ylabel "Accumulated Confidence"
	    :label "Relative accumulated confidence"
	    :aspect-ratio 0.66
	    :reset nil
	    :title (format nil "Accumulated Expectation Confidences for ~a" title))
      (close-window)
      (window)
      (reset-plot)
      (plot-command "set title font \"Times,22\"")
      (plot-command "set xlabel font \"Times,24\"")
      (plot-command "set ylabel font \"Times,24\"")
      (plot-command "set xtics ~a out" (max bin-size minimum-tics))
      (plot (./ accumulated-time-values dividable-counts) time-bins
	    :style "boxes fill solid 1.0 border -1"
	    :xlabel "Divisions of a Measure" 
	    :ylabel "Average Confidence"
	    :label "Average Confidence per Measure Division"
	    :aspect-ratio 0.66
	    :reset nil
	    :title (format nil "Average Expectation Confidences for ~a" title))
      (close-window))))

;; (setf (.aref bins bin-index) (if (zerop in-bin-count) in-bin-count
;; 			     (/ (.sum (.* in-bin expect-time-values)) in-bin-count))))

(defun metrical-rhythm-expectancies (metrical-rhythms meter number-of-bars title
				     ;; #'last-onset-expectations last-expectations-no-integrate
				     &key (expectation-generator #'expectancies-of-rhythm-ridge-persistency))
  "Given a set of metrical rhythms of fixed length, plot the confidences of each expectation"
  (format t "~a~%" title)
  (let* ((expectancy-set (mapcar (lambda (rhythm) 
				   (last-expectations rhythm :expectancies-generator expectation-generator))
				 metrical-rhythms))
	 ;; (expectancy-set (mapcar (lambda (r) (last-expectations r :last-time (last-onset-time r))) metrical-rhythms))
	 ;; (expectancy-set (mapcar (lambda (r) (last-expectations r :last-time 799)) metrical-rhythms))
	 ;; Make all expectations into a single list for easy traversal.
	 (all-expectations (reduce #'append expectancy-set))
	 ;; Generate one more rhythm to determine the duration in samples and therefore
	 ;; the bar duration.
	 (rhythm-duration (duration-in-samples (random-rhythm-of-meter meter number-of-bars)))
	 (bar-duration-in-samples (/ rhythm-duration number-of-bars))
	 ;; (make-narray (mapcar (lambda (expect) (time-in-seconds expect sample-rate)) all-expectations))
	 (bar-times (make-narray (mapcar (lambda (expect) (/ (expected-time expect) bar-duration-in-samples)) all-expectations)))
	 (confidences (make-narray (mapcar (lambda (expect) (confidence expect)) all-expectations))))
    (format t "sorted by time:~%")
    (dolist (e (sort all-expectations #'< :key #'expected-time))
      (format t "~a ~,3f ~,3f~%" e 
	      (/ (expected-time e) bar-duration-in-samples) 
	      (- (expected-time e) rhythm-duration)))
    ;; (format t "bar-times ~a~%" bar-times) 
    (plot-expectancies-histogram bar-times confidences title)))

(defun plot-interval-expectancies-histogram (times time-values title &key (bin-size 0.025))
  (multiple-value-bind (counts time-bins accumulated-time-values)
      (time-bins times time-values :bin-size bin-size)
    (format t "prediction count ~a~%time-bins ~a~%accumulated-time-values ~a~%" 
    	    counts time-bins accumulated-time-values)
    (let ((dividable-counts (.+ (.not counts) counts))
	  (minimum-tics (round (.max time-bins) 20)))
      ;; (format t "averaged time-values ~a~%" (./ accumulated-time-values dividable-counts))
      (window)
      (reset-plot)
      (plot-command "set title font \"Times,24\"")
      (plot-command "set xlabel font \"Times,24\"")
      (plot-command "set ylabel font \"Times,24\"")
      (plot-command "set xtics out 0,~a" (max bin-size minimum-tics))
      (plot accumulated-time-values time-bins
	    :style "boxes fill solid 1.0 border -1"
	    :xlabel "Tatums (Units of Minimum IOI)" 
	    :ylabel "Accumulated Confidence"
	    :label "Relative accumulated confidence"
	    :aspect-ratio 0.66
	    :reset nil
	    :title (format nil "Accumulated Expectation Time-Values for ~a" title))
      (close-window)
      (window)
      (reset-plot)
      (plot-command "set title font \"Times,24\"")
      (plot-command "set xlabel font \"Times,24\"")
      (plot-command "set ylabel font \"Times,24\"")
      (plot-command "set xtics out 0,~a" (max bin-size minimum-tics))
      (plot (./ accumulated-time-values dividable-counts) time-bins
	    :style "boxes fill solid 1.0 border -1"
	    :xlabel "Tatums (Units of Minimum IOI)" 
	    :ylabel "Average Confidence"
	    :label "Average Confidence per Tatum"
	    :aspect-ratio 0.66
	    :reset nil
	    :title (format nil "Average Expectation Time-Values for ~a" title))
      (close-window))))

(defun intervals-of-performed-rhythms (rhythm
				     ;; #'last-onset-expectations last-expectations-no-integrate
				     &key (expectation-generator #'expectancies-of-rhythm-ridge-persistency))
  "Given a performed rhythm, plot the confidences of each expectation"
  (let* ((expectancies (last-expectations rhythm :expectancies-generator expectation-generator))
	 (rhythm-duration (duration-in-samples rhythm))
	 (minimum-ioi (.min (rhythm-iois-samples rhythm))) ; i.e. tatum
	 (interval-times (make-narray (mapcar 
				  (lambda (expect) (/ (- (expected-time expect) rhythm-duration) minimum-ioi)) 
				  expectancies)))
	 (confidences (make-narray (mapcar (lambda (expect) (confidence expect)) expectancies))))
    (format t "interval-times ~a~%" interval-times)
    (list interval-times confidences)))

(defun performed-rhythm-expectancies (performed-rhythms title
				     ;; #'last-onset-expectations last-expectations-no-integrate
				     &key (expectation-generator #'expectancies-of-rhythm-ridge-persistency))
  "Given a set of performed rhythms, plot the confidences of each expectation"
  (format t "~a~%" title)
  (loop 
     for rhythm in performed-rhythms
     for expectancies = (last-expectations rhythm :expectancies-generator expectation-generator)
     for rhythm-duration = (duration-in-samples rhythm)
     for tatum = (.min (rhythm-iois-samples rhythm))
     append (mapcar (lambda (expect) (/ (- (expected-time expect) rhythm-duration) tatum)) expectancies) into interval-ratios
     append (mapcar (lambda (expect) (confidence expect)) expectancies) into confidences
     finally (plot-interval-expectancies-histogram (make-narray interval-ratios) 
						   (make-narray confidences) 
						   title :bin-size 0.25)))

(defun random-metrical-rhythm-expectancies (candidate-meter &key (sample-size 40) (number-of-bars 2))
  "Given a meter, create a set of random metrical rhythms plot the confidences of each expectation"
  (let* ((random-metrical-rhythms (make-metrical-set candidate-meter sample-size number-of-bars))
	 (title (format nil "~a examples of ~a bars of ~a meter" sample-size number-of-bars candidate-meter)))
    (metrical-rhythm-expectancies random-metrical-rhythms candidate-meter number-of-bars title)))

;; (plot-expectancies all-expectations (format nil "Expectation Confidences for ~a examples of ~a bars of ~a meter"  sample-size number-of-bars candidate-meter))

(defun load-essen-rhythms (essen-scores)
  "Reads in the files from the score descriptions passed in, returns a list of rhythm instances."
  (loop 
     for (filename m meter d description) in essen-scores
     for filepath = (make-pathname :directory "/Volumes/iDisk/Research/Data/Temperley/essen-perf" 
				   :name filename 
				   :type "notes")
     collect (rhythm-of-melisma-file filepath)))

;;; (setf rs (load-essen-rhythms (subseq dorys::*essen-perf-meters* 0 5)))
;;; (setf performed-44 (dorys::essen-of-meter "4/4"))
;;; (setf performed-34 (dorys::essen-of-meter "3/4"))
;;; (setf performed-68 (dorys::essen-of-meter "6/8"))
;;; (setf expectancy-set (mapcar #'last-expectations (load-essen-rhythms performed-34)))
;;; (setf all-expectations (reduce #'append expectancy-set))

(defun plot-ridge-persistency (ridge-persistency scaleogram title)
  (format t "~a sorted prominent ridges of duration:~%~a~%" 
	  title (time-support (most-persistent-scales ridge-persistency) (voices-per-octave scaleogram)))
  (reset-plot)
  (plot-command "set xtics (~{~{\"~d\" ~5d~}~^, ~}) out~%" (label-scale-as-time-support scaleogram))
  (plot (.reverse ridge-persistency) 
	nil 
	:style "boxes fill solid 1.0 border -1"
	:aspect-ratio 0.66 
	:reset nil 
	:title title))

(defmethod ridge-persistency-of ((rhythm-to-analyse rhythm))
  "Return the ridge persistency (normalised occurrence) of a given rhythm"
  (let* ((analysis (analysis-of-rhythm rhythm-to-analyse :padding #'causal-pad)))
    (unweighted-ridge-persistency-of analysis)))

;;; TODO make this a defmethod?
;;; TODO factor into determining the arp from a list of ridge-persistency-of results.
(defun average-ridge-persistency (rhythms)
  "Sum the ridge persistencies over the list of rhythms and return the average ridges persistency measure."
  (loop
     for rhythm in rhythms
     ;; Since some rhythms are shorter than the maximum, we need to pad the ridge-persistency responses. 
     with max-time-limit = (.max (make-narray (mapcar #'duration-in-samples rhythms)))
     with max-dilation = (number-of-scales-for-period max-time-limit)
     with total-persistency = (make-double-array max-dilation)
     do (setf total-persistency (.+ total-persistency 
				    (pad-end-to-length (ridge-persistency-of rhythm) max-dilation)))
     finally (return (./ total-persistency (length rhythms)))))

(defun generate-metrical-profile (list-of-rhythm-iois meter)
  "Return a histogram of the metrical position of each onset within the given meter"
  (let* ((measure-length (reduce #'* meter))
	 (metrical-position-histogram (dorys::make-histogram '())))
    (dolist (rhythm-iois list-of-rhythm-iois)
      (let* ((metrical-positions (.mod (make-narray (iois-to-onsets rhythm-iois)) measure-length)))
	(dorys::add-to-histogram metrical-position-histogram (val metrical-positions))))
    (dorys::get-histogram metrical-position-histogram)))

(defun plot-metrical-profile (list-of-rhythm-iois meter title)
  "Plot the histogram of the IOIs according to their metrical position, given the meter"
  (multiple-value-bind (metrical-position counts) (generate-metrical-profile list-of-rhythm-iois meter)
    (let* ((meter-length (reduce #'* meter))
	   (all-metrical-positions (make-integer-array meter-length)))
      (setf (.arefs all-metrical-positions (make-narray metrical-position)) (make-narray counts))
      (reset-plot)
      (plot-command "set title font \"Times,24\"")
      (plot-command "set xlabel offset 0,-1 font \"Times,24\"")
      (plot-command "set ylabel font \"Times,24\"")
      (plot all-metrical-positions (.iseq 1 meter-length)
	    :style "boxes fill solid 1.0 border -1"
	    :xlabel "Semiquavers of a Measure" 
	    :ylabel "Occurrence"
	    :label "Metrical Profile"
	    :aspect-ratio 0.66
	    :reset nil
	    :title (format nil "Metrical Profile for ~a" title)))))

(defun interval-histogram (list-of-rhythm-iois)
  "Create a histogram on the intervals, not the metrical position"
  (let* ((interval-histogram (dorys:make-histogram '())))
    (dolist (rhythm-iois list-of-rhythm-iois)
      (dorys:add-to-histogram interval-histogram rhythm-iois))
    interval-histogram))

(defun plot-interval-comparison (interval-histogram average-ridge-persistency description
				&key (crochet-duration 120) (vpo 16))
  "Plots a comparison between the histogram of intervals and the multiresolution ridge persistency"
  (format t "histogram of intervals ~a~%" (multiple-value-list (dorys:get-histogram interval-histogram)))
  (let* ((interval-counts (make-narray (dorys:get-histogram-counts interval-histogram)))
	 ;; Scale the histogram to match the highest average ridge persistency
	 (histogram-scaling (/ (.max average-ridge-persistency) 
			       (.max interval-counts))))
    (reset-plot)
    (plot-command "set title font \"Times,24\"")
    (plot-command "set xlabel offset 0,-1 font \"Times,24\"")
    ;; (plot-command "set y2label 'Proportion of Interval Present'")
    (plot-command "set ylabel font \"Times,24\"")
    (plot-command "set xtics border (~{~{\"~a\" ~d~}~^, ~}) font \"Sonata,28\"~%" 
		  (x-axis-pad (label-scale-as-rhythmic-beat vpo crochet-duration)))
    (nplot (list (.* interval-counts histogram-scaling) average-ridge-persistency)
	   nil
	   :styles '("boxes fill solid border 9" "lines linetype 3 linewidth 2")
	   :legends '("Relative Frequency of Occurrence of Intervals" "Time-Frequency Scale Persistency")
	   :xlabel "Dilation Scales in Units of Musical Time"
	   :ylabel "Proportion of Interval Present"
	   :title (format nil "Skeleton Scale Persistency vs. Occurrence of Intervals For ~a" description)
	   :reset nil
	   :aspect-ratio 0.66)))

;; (plot-interval-comparison (interval-histogram rhythm-iois) (average-ridge-persistency rhythms)

(defun plot-interval-histogram (interval-histogram description)
  "Plots a comparison between the histogram of intervals and the multiresolution ridge persistency"
  (format t "histogram of intervals ~a~%" (multiple-value-list (dorys:get-histogram interval-histogram)))
  (multiple-value-bind (elements counts) (dorys:get-histogram interval-histogram)
    (let* ((element-counts (make-narray counts))
	   (intervals (make-narray elements))
	   (largest-interval (.max intervals))
	   (interval-counts (make-integer-array largest-interval)))
      (setf (.arefs interval-counts (.- intervals 1)) element-counts)
      (reset-plot)
      (plot-command "set title font \"Times,24\"")
      (plot-command "set xlabel offset 0,-1 font \"Times,24\"")
      ;; (plot-command "set y2label 'Proportion of Interval Present'")
      (plot-command "set ylabel font \"Times,24\"")
      (plot interval-counts (.iseq 1 largest-interval)
	    :style "boxes fill solid border 9"
	    :label "Relative Frequency of Occurrence of Intervals"
	    :xlabel "Intervals in semiquavers"
	    :ylabel "Proportion of Interval Present"
	    :title (format nil "Occurrence of Intervals For ~a" description)
	    :reset nil
	    :aspect-ratio 0.66))))

;; (plot-interval-histogram (interval-histogram rhythm-iois) "")

(defun plot-profile-and-persistency-of-rhythm (rhythm-to-plot iois-of-rhythm meter number-of-bars)
  (format t "rhythm ~a is:~%~a~%" (name rhythm-to-plot) iois-of-rhythm)
  (let* ((analysis (analysis-of-rhythm rhythm-to-plot :padding #'causal-pad))
	 (ridge-persistency (ridge-persistency-of (skeleton analysis))))
    (write-as-audio rhythm-to-plot
		    #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/test_sound.wav"
		    #P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff")
    ;; (plot-interval-histogram (interval-histogram (list iois-of-rhythm)) (name rhythm-to-plot)) 
    (plot-time-histogram (make-narray iois-of-rhythm) 
			 (format nil "Intervals in ~a" (name rhythm-to-plot)) :bin-size 1)
    (window)
    (plot-rhythm rhythm-to-plot :time-in-seconds t)
    (close-window)
    (window)
    (plot-cwt+skeleton-of analysis '() rhythm-to-plot)
    (close-window)
    (window)
    (plot-metrical-profile (list iois-of-rhythm) meter (format nil "random rhythm in ~a meter" meter))
    (close-window)
    (window)
    (plot-ridge-persistency ridge-persistency
			    (scaleogram analysis)
			    (format nil "Ridge persistency of sample random rhythm in ~a meter"
				    (name rhythm-to-plot)))
    (close-window)
    (metrical-rhythm-expectancies (list rhythm-to-plot) meter number-of-bars 
				  (format nil "~a (RP)" (name rhythm-to-plot))
				  :expectation-generator #'expectancies-of-rhythm-ridge-persistency)
;;     (metrical-rhythm-expectancies (list rhythm-to-plot) meter number-of-bars
;; 				  (format nil "~a (ending peaks)" (name rhythm-to-plot))
;; 				  :expectation-generator #'expectancies-of-rhythm)
    (metrical-rhythm-expectancies (list rhythm-to-plot) meter number-of-bars
				  (format nil "~a (integration)" (name rhythm-to-plot))
				  :expectation-generator #'expectancies-of-rhythm-integrator)
    analysis))

(defun find-tactus (rhythm ridge-persistency &key (vpo 16))
  (let* ((minimum-interval (.min (rhythm-iois-samples rhythm)))
	 (persistent-intervals (time-support (most-persistent-scales ridge-persistency) vpo))
	 (minimum-period (.min persistent-intervals))
	 (period-ratios (./ persistent-intervals minimum-period))
	 (interval-ratios (./ persistent-intervals minimum-interval)))
    (format t "persistent intervals ~a~%period-ratios ~a~%"
	    persistent-intervals period-ratios)
    (format t "interval-ratios ~a~%" interval-ratios)
    (format t "minimum interval ~a minimum-period ~a~%" minimum-interval minimum-period)))
    
(defun plot-persistency-of-performed-rhythm (rhythm-to-plot)
  (let* ((analysis (analysis-of-rhythm rhythm-to-plot :padding #'causal-pad))
	 (relative-iois (intervals-as-ratios rhythm-to-plot))
	 (ridge-persistency (unweighted-ridge-persistency-of analysis)))
    (write-as-audio rhythm-to-plot
		    #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/test_sound.wav"
		    #P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff")
    ;; (plot-interval-histogram (interval-histogram (list iois-of-rhythm)) (name rhythm-to-plot)) 
    (plot-time-histogram relative-iois 
			 (format nil "Intervals in ~a" (name rhythm-to-plot)) :bin-size 1)
    (window)
    (plot-rhythm rhythm-to-plot :time-in-seconds t)
    (close-window)
    (window)
    (plot-cwt+skeleton-of analysis '() rhythm-to-plot)
    (close-window)
    ;; (window)
    ;; (plot-metrical-profile (list relative-iois) meter)
    ;; (close-window)
    (window)
    (plot-ridge-persistency ridge-persistency
			    (scaleogram analysis)
			    (format nil "Ridge persistency of sample random rhythm in ~a meter"
				    (name rhythm-to-plot)))
    (close-window)
    (performed-rhythm-expectancies rhythm-to-plot
				   (format nil "RP of ~a" (name rhythm-to-plot))
				   :expectation-generator #'expectancies-of-rhythm-ridge-persistency)
;;     (metrical-rhythm-expectancies (list rhythm-to-plot) meter number-of-bars
;; 				  (format nil "integration of ~a" (name rhythm-to-plot))
;; 				  :expectation-generator #'expectancies-of-rhythm-integrator)
    analysis))

(defun one-rhythm (meter number-of-bars)
  (multiple-value-bind (one-rhythm one-iois) 
      (random-rhythm-of-meter meter number-of-bars)
    (plot-profile-and-persistency-of-rhythm one-rhythm one-iois meter number-of-bars)))

(defun plot-profile-and-persistency (sample-size number-of-bars meter-name)
  "Display the combined metrical profile and combined ridge persistency measures"
  (let ((meter (meter-for-name meter-name)))
    (multiple-value-bind (random-rhythms random-iois) (make-metrical-set meter sample-size number-of-bars)
      ;; Need an analysis instance to pass in the scaleogram to label the axes, This is way
      ;; too wasteful!
      ;; TODO should produce the analysis of all the rhythms, then compute the arp &
      ;; expectancies from those.
      (let (;;(analysis (analysis-of-rhythm (first random-rhythms)))
	    ;; (arp (average-ridge-persistency random-rhythms))
	    (description (format nil "~a random rhythms in ~a meter of ~a measures"
				 (length random-rhythms) meter-name number-of-bars)))
	(window)
	(plot-metrical-profile random-iois meter 
			       (format nil "~a random rhythms in ~a meter" (length random-iois) meter-name))
	(close-window)
	(window)
	(plot-interval-histogram (interval-histogram random-iois) description)
	(close-window)
	;; (plot-time-histogram (make-narray random-iois) description :bin-size 1)
	;; (plot-ridge-persistency arp
	;; 				(scaleogram analysis)
	;; 				(format nil "Average ridge persistency of ~a" description))
	(metrical-rhythm-expectancies random-rhythms
				      meter
				      number-of-bars 
				      description ; (format nil "~a (RP)" description)
				      :expectation-generator #'expectancies-of-rhythm-ridge-persistency)
	;; (metrical-rhythm-expectancies random-rhythms
	;;		    meter
	;;		    number-of-bars 
	;;		    (format nil "~a (ending peaks)" description)
	;;		    :expectation-generator #'expectancies-of-rhythm)
	(metrical-rhythm-expectancies random-rhythms
				      meter
				      number-of-bars 
				      (format nil "~a (integration)" description)
				      :expectation-generator #'expectancies-of-rhythm-integrator)))))

(defun essen-named (name)
  (find name dorys::*essen-perf-meters* :test #'equal :key #'first))

(defun time-limited-rhythm-named (name &key (sample-rate 200))
  (mapcar (lambda (x) (limit-rhythm x :maximum-samples (* 15 sample-rate)))
	  (load-essen-rhythms (list (essen-named name)))))

(defun performed-rhythms-of-meter (meter)
  (let ((rhythm-set (time-limited-rhythms-of-meter meter)))
    (performed-rhythm-expectancies rhythm-set 
				   (format nil "~a Performed Rhythms in ~a Meter"
					   (length rhythm-set) meter))))

(defun time-limited-rhythms-of-meter (meter &key (sample-rate 200))
      (mapcar (lambda (x) (limit-rhythm x :maximum-samples (* 15 sample-rate)))
		      (load-essen-rhythms (dorys::essen-of-meter meter))))

;; (time-limited-rhythms-of-meter "3/4")


#|

(defun plot-profile-and-persistency-new (sample-size number-of-bars meter)
  "Display the combined metrical profile and combined ridge persistency measures"
  (multiple-value-bind (random-rhythms random-iois) (make-metrical-set meter sample-size number-of-bars)
    ;; produce the analysis of all the rhythms, then compute the arp &
    ;; expectancies from those.
    (let* ((all-analyses (mapcar (lambda (r) (analysis-of-rhythm r :padding #'causal-pad)) random-rhythms))
	   (ridge-persistencies (mapcar #'unweighted-ridge-persistency-of all-analyses))
	   (arp (average-ridge-persistency-of ridge-persistencies)))
      (window)
      (plot-metrical-profile random-iois meter "")
      (close-window)
      (window)
      (plot-ridge-persistency arp
			      ;; Need to pass in the scaleogram to label the axes.
			      (scaleogram (first all-analyses))
			      (format nil "Average ridge persistency of ~a random rhythms in ~a meter of ~a measures"
				      (length random-rhythms) meter number-of-bars))
      
      (metrical-rhythm-expectancies random-rhythms all-analyses
				    meter 
				    number-of-bars 
				    (format nil "~a random rhythms in ~a meter" (length random-rhythms) meter)))))

(pushnew 'weighted-beat-ridge *plotting*)
(setf e (expectancies-of-rhythm-integrator random-44 :times-to-check (list (1- (duration-in-samples random-44)))))

(setf r '(2 1 1 2 6 1 1 1 1 1 1 1 1 1 1 2))
(setf m (iois-to-rhythm "blah" r :shortest-ioi (/ 200.0 6) :sample-rate 200))

;;; 4 bars of 3 crotchets each = 48 semiquavers
;;; 3 bars of 4 crotchets each = 48 semiquavers

(setf expected-times (last-expectations m44))
(setf expected-times-no-integration (last-expectations-no-integration m44))
(setf expected-times-from-onset (last-onset-expectations m44))
;; (mapcar (lambda (e) (/ (expected-time e) bar-duration-in-samples)) expected-times) 

;; Resolution is too fine?
(scale-from-period (.* bar-duration-in-samples (make-narray '(3.0 3.25 3.5 3.75 4))) 16) ; for 4/4.
;; In samples:
(setf note-durations-44 (.* bar-duration-in-samples (make-narray '(0.0 0.25 0.5 0.75 1.0))))
(scale-from-period note-durations-44 16)
;; In samples:
(setf note-durations-34 (.* bar-duration-in-samples (make-narray '(0.0 0.333 0.666 1.0))))
(scale-from-period note-durations-34 16)
;; In samples:
(setf projection-times (make-narray (mapcar (lambda (e) (- (expected-time e) rhythm-duration)) expected-times)))
(./ projection-times bar-duration-in-samples)

;; Compare integrator vs. last-onset magnitude & phase.
(last-expectations m)
(last-onset-expectations m)

;; (random-metrical-rhythm-expectancies '(3 2 2) :number-of-bars 4 :sample-size 40) ; 100
;; (random-metrical-rhythm-expectancies '(2 2 2 2) :number-of-bars 3 :sample-size 40) ; 100

;;; larger number of bars.
;; (random-metrical-rhythm-expectancies '(3 2 2) :number-of-bars 8 :sample-size 10)
;; (random-metrical-rhythm-expectancies '(2 2 2 2) :number-of-bars 6 :sample-size 10)

(setf mo (append-rhythm m (random-offset 100)))
(pushnew 'ridge-phase *plotting*)
(list (last-expectations mo :last-time (last-onset-time mo)) ; last-onset-expect-mo
      (last-expectations mo :last-time 799) ; last-meter-expect-mo
      (last-expectations mo)) ; last-moment-expect-mo
(plot-expectancies-histogram (last-expectations mo) (name mo))
(mapcar (lambda (e) (time-in-seconds e 200.0)) (last-expectations mo))

(setf iso-rhythm (rhythm-of-grid "Isochronous Rhythm" '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) :shortest-ioi 256))

(setf rhythm-offset (append-rhythm (random-offset 100) iso-rhythm))

(pushnew 'weighted-beat-ridge *plotting*)
(pushnew 'scale-energy-profile *plotting*)
(pushnew 'unweighted-profile *plotting*)
(pushnew 'tempo-preference *plotting*)
;; Do no phase correction when testing metrical expectancies.
(expectancies-of-rhythm-integrator iso-rhythm 
				   :times-to-check (list (1- (duration-in-samples iso-rhythm)))
				   :phase-correct-from nil)


(setf x (tempo-salience-weighting-vector (preferred-tempo-scale 16 200) 128 :octaves-per-stddev 1.0))

(setf m (scaleogram-magnitude (scaleogram (analysis-of-rhythm iso-rhythm))))
(plot (.subseq (.column m 2048) 75 86) (.iseq 75 85) :aspect-ratio 0.66)
|#


;;;;
;;;; Comparison to anthems:
;;;;

#|

(defun strip-anthem-of-anacrusis (anthem)
  "Returns the IOIs with the anacrusis removed."
  (onsets-to-iois (remove-if #'minusp 
			     (iois-to-onsets (second anthem) 
					     (- (dorys::anthem-anacrusis-duration anthem))))))

;;; Only test anthems that are measured to minimum IOI of 16ths.
(setf waltz-anthems-16ths (remove-if-not (lambda (a) (equal (anthem-beat-duration a) 4)) (anthems-of-meter "3/4")))
(setf common-anthems-16ths (remove-if-not (lambda (a) (equal (anthem-beat-duration a) 4)) (anthems-of-meter "4/4")))

;;; The profile is not quite canonical for the anthems, when looked at across all anthems.
;;; 3/4
(plot-metrical-profile (mapcar #'strip-anthem-of-anacrusis waltz-anthems-16ths) "3/4" "")
;;; 4/4
(plot-metrical-profile (mapcar #'strip-anthem-of-anacrusis common-anthems-16ths) "4/4" "")

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

;; (metrical-profile-of-anthem (anthem-named 'dorys::america) '(3 2 2))

;;; Compare frequency of intervals against the generated versions.
(dorys:get-histogram (interval-histogram (list (second (anthem-named 'dorys::america)))))

;;; to check the IOI isn't a problem.
(defun anthem-minimum-duration (anthem &key (crochet-duration 120))
  "Returns the duration of the smallest interval in the anthem, in samples, given a fixed tempo."
  (/ crochet-duration (anthem-beat-duration anthem)))

;;; Most anthems don't demonstrate a canonical metrical profile, often lacking any beats
;;; falling on minor metrical positions.
(plot-ridge-persistency (average-ridge-persistency waltz-anthem-rhythms)
			(scaleogram-of-rhythm (first waltz-anthem-rhythms))
			"Average ridge persistency of waltz anthem rhythms")

|#

#|
;;; We've verified the collection of random rhythms produce correct metrical profiles.
;;; Yet while the average ridge persistency shows ridges for 30 & 60 samples (semiquaver &
;;; quaver) we don't get good values for crotchet etc.
;;;
;;; However we do see some small peaks around the following scales.
;;; Uncertainty (frequency resolution representated as time support).
(time-support (make-narray (list (floor (scale-from-period 360 16))
				 (ceiling (scale-from-period 360 16))
				 (floor (scale-from-period 480 16))
				 (ceiling (scale-from-period 480 16)))) 16)

;;; Need minimum of 1 rhythm of 16 bars to get a good metrical profile
(setf one-34-rhythm (one-rhythm '(3 2 2) 16))
(one-rhythm '(3 2 2) 30)
(one-rhythm '(2 2 2 2) 30)

;;; The average number of bars in an anthem is 18, so we create the same to match the waltz-anthem-16ths
;;; Produces a very nice profile.
(plot-profile-and-persistency 7 18 '(3 2 2))


;;; Test the meter determination code.
(dolist (rhythm random-34-rhythms)
  (format t "proposed meter ~a~%" (meter-of-rhythm rhythm)))

;;; TODO each scale should have it's own summation (leaky integration) constant when
;;; determining the influence and tracking of each peak at each moment. This
;;; should be the differentiation between expectancies-of-rhythm-integrator and
;;; expectancies-of-rhythm-ridge-persistency

;;;; Performed rhythms

(plot-time-histogram (intervals-as-ratios r) "Relative Intervals")
(plot-time-histogram (make-narray one-iois) "IOIs" :bin-size 1)

(plot-interval-histogram (interval-histogram (list (nlisp::array-to-list
						    (intervals-as-ratios r))))
			 "performed-44")


(mapcar #'intervals-as-ratios (load-essen-rhythms performed-44))
(mapcar (lambda (x) (plot-time-histogram (intervals-as-ratios x) (name x))) (load-essen-rhythms performed-44))

|#

;;;; ICMPC plots.
(defun icmpc-plots ()
  (pushnew 'tempo-beat-preference *plotting*)
  ;; About 1 rhythm of 30 bars, or 5 rhythms of 6 bars each is minimally necessary to produce a
  ;; canonical metrical profile.
  (plot-profile-and-persistency 20 8 "3/4")
  (pop *plotting*)
  (plot-profile-and-persistency 20 6 "4/4")
  
  ;; "4/4" "6/8"
  (performed-rhythms-of-meter "4/4")
  (performed-rhythms-of-meter "3/4"))
  
