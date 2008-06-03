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
  (mapcar (lambda (rhythm)
	    (let* ((analysis (analysis-of-rhythm rhythm :padding #'causal-pad)))
	      (plot-cwt-of-rhythm (scaleogram analysis) rhythm) 
	      analysis))
	  rhythms))

(defun write-rhythms (rhythms name)
  (loop
     for rhythm in rhythms
     for index = 1 then (1+ index)
     do (write-as-audio rhythm
			(make-pathname :directory "/Volumes/iDisk/Research/Data/RicardsOnsetTests/UnAccented/"
				       :name (format nil "~a-~d" name index)
				       :type "wav")
			#P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff")))

(defun intervals-of-performed-rhythms (rhythm
				     ;; #'last-onset-expectations last-expectations-no-integrate
				     &key (expectation-generator #'expectancies-of-rhythm-ridge-persistency))
  "Given a performed rhythm, return the confidences of each expectation"
  (let* ((expectancies (last-expectations rhythm :expectancies-generator expectation-generator))
	 (rhythm-duration (duration-in-samples rhythm))
	 (minimum-ioi (.min (rhythm-iois-samples rhythm))) ; i.e. tatum
	 (interval-times (make-narray (mapcar 
				  (lambda (expect) (/ (- (expected-time expect) rhythm-duration) minimum-ioi)) 
				  expectancies)))
	 (confidences (make-narray (mapcar (lambda (expect) (confidence expect)) expectancies))))
    (format t "interval-times ~a~%" interval-times)
    (list interval-times confidences)))

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
	 (metrical-position-histogram (make-histogram '())))
    (dolist (rhythm-iois list-of-rhythm-iois)
      (let* ((metrical-positions (.mod (make-narray (iois-to-onsets rhythm-iois)) measure-length)))
	(add-to-histogram metrical-position-histogram (val metrical-positions))))
    (get-histogram metrical-position-histogram)))

(defun interval-histogram (list-of-rhythm-iois)
  "Create a histogram on the intervals, not the metrical position"
  (let* ((interval-histogram (make-histogram '())))
    (dolist (rhythm-iois list-of-rhythm-iois)
      (add-to-histogram interval-histogram rhythm-iois))
    interval-histogram))

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

;;;;
;;;; Plot routines
;;;;

(defun plot-expectancies (all-expectations title)
  (let ((expect-times (make-narray (mapcar (lambda (expect) (time-in-seconds expect 200.0)) all-expectations)))
	(expect-confidences (make-narray (mapcar (lambda (expect) (confidence expect)) all-expectations))))
    (window)
    (plot expect-confidences expect-times 
 	  :style "points" :xlabel "Time" :ylabel "Confidence" 
 	  :aspect-ratio 0.66
 	  :title title)
    (close-window)))

(defun plot-expectancies-histogram (times time-values meter title 
				    &key (bin-size 0.025)
				    (maximum-confidence 5.0 confidence-supplied))
  (multiple-value-bind (counts time-bins accumulated-time-values)
      (binned-values times time-values :bin-size bin-size)
    ;;(format t "prediction count ~a~%time-bins ~a~%accumulated-time-values ~a~%" 
    ;;	    counts time-bins accumulated-time-values)
    (let ((dividable-counts (.+ (.not counts) counts))
	  (minimum-tics 0.1)
	  (maximum-confidence (if confidence-supplied maximum-confidence (* 1.1 (.max accumulated-time-values))))
	  (start-time (floor (.min time-bins))))
      ;; (format t "averaged time-values ~a~%" (./ accumulated-time-values dividable-counts))
      ;; 
      (window)
      (reset-plot)
      (plot-command "set title font \"Times,22\"")
      (plot-command "set xlabel font \"Times,24\"")
      (plot-command "set ylabel font \"Times,24\"")
      (plot-command "set xtics out ~f,~f" start-time (max bin-size minimum-tics))
      (plot-command "set xrange [~f:*]" start-time)
      (plot-command "set yrange [0:~f]" maximum-confidence)
      ;; Plot the meter behind the histogram
      (plot-martin-tree meter 1.0 maximum-confidence :start-x start-time)
      (plot accumulated-time-values time-bins
	    :style "boxes fill solid 0.75 border -1" ; transparent
	    :xlabel "Divisions of a Measure" 
	    :ylabel "Accumulated Confidence"
	    :label "Relative Accumulated Confidence"
	    :aspect-ratio 0.66
	    :reset nil
	    :title (format nil "Accumulated Expectation Confidences for ~a" title))
      (close-window)
      ;;
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

;;(plot-expectancies-histogram (make-narray '(8.1 8.3 8.5 8.7 8.9 9.0 10.0)) 
;;			     (make-narray '(0.2 0.4 0.5 0.4 0.3 0.01 0.2)) '(3 2 2) "test")
;; (plot-expectancies-histogram (make-narray '(8.0 8.1 8.3 8.5 8.7 8.9 9.0 9.1)) 
;;  			        (make-narray '(0.25 0.2 0.4 0.5 0.4 0.3 0.01 0.2)) 
;;				'(3 2 2) "test" 
;;			        :bin-size 0.01)

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
	 ;; Assume the length of one metrical rhythm is the same for all of them.
	 (rhythm-duration (duration-in-samples (first metrical-rhythms)))
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
    (plot-expectancies-histogram bar-times confidences meter title)))

(defun performed-rhythm-expectancies (performed-rhythms meter title
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
						   meter
						   title :bin-size 0.25)))

(defun plot-interval-expectancies-histogram (times time-values meter title &key (bin-size 0.025))
  (multiple-value-bind (counts time-bins accumulated-time-values)
      (binned-values times time-values :bin-size bin-size)
    (format t "prediction count ~a~%time-bins ~a~%accumulated-time-values ~a~%" 
    	    counts time-bins accumulated-time-values)
    (let ((dividable-counts (.+ (.not counts) counts))
	  (max-confidence (max (.max accumulated-time-values) 3.5))
	  (minimum-tics (round (.max time-bins) 20)))
      ;; (format t "averaged time-values ~a~%" (./ accumulated-time-values dividable-counts))
      (window)
      (reset-plot)
      (plot-command "set title font \"Times,24\"")
      (plot-command "set xlabel font \"Times,24\"")
      (plot-command "set ylabel font \"Times,24\"")
      (plot-command "set xtics out 0,~a" (max bin-size minimum-tics))
      (plot-command "set xrange [0:*]")
      (plot-command "set yrange [0:~d]" max-confidence)
      ;; Plot the meter behind the histogram
      (plot-martin-tree meter (reduce #'* meter) max-confidence :start-x 0.0)
      (plot accumulated-time-values time-bins
	    :style "boxes fill solid 0.75 border -1"
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

;; (plot-expectancies all-expectations (format nil "Expectation Confidences for ~a examples of ~a bars of ~a meter"  sample-size number-of-bars candidate-meter))

(defun plot-ridge-persistency (ridge-persistency scaleogram title &key (sample-rate 200.0 sample-rate-supplied))
  (format t "~a sorted prominent ridges of duration:~%~a~%" 
	  title (time-support (most-persistent-scales ridge-persistency) (voices-per-octave scaleogram)))
  (reset-plot)
  (plot-command "set title font \"Times,24\"")
  (plot-command "set xlabel offset 0,-1 font \"Times,24\"")
  (plot-command "set ylabel font \"Times,24\"")
  (plot-command "set key off")
  (plot-command "set xtics (~{~{\"~d\" ~5d~}~^, ~}) out~%" 
		(if sample-rate-supplied 
		    (label-scale-as-time-support-seconds scaleogram sample-rate)
		    (label-scale-as-time-support scaleogram)))
  (plot (.reverse ridge-persistency) 
	nil 
	:style "boxes fill solid 1.0 border -1"
	:aspect-ratio 0.66 
	:xlabel (if sample-rate-supplied "Time (Seconds)" "Time (Samples)")
	:ylabel "Ridge Presence"
	:reset nil 
	:title title))

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
      (plot-command "set xtics 0,1")
      (plot-command "set yrange [0:1.1]")
      (plot-command "set xrange [0:~d]" (1+ meter-length))
      (plot (normalise all-metrical-positions) (.iseq 1 meter-length)
	    :style "boxes fill solid 1.0 border -1"
	    :xlabel "Semiquavers of a Measure" 
	    :ylabel "Relative Occurrence"
	    :label "Metrical Profile"
	    :aspect-ratio 0.66
	    :reset nil
	    :title (format nil "Metrical Profile for ~a" title)))))

(defun plot-interval-comparison (interval-histogram average-ridge-persistency description
				&key (crochet-duration 120) (vpo 16))
  "Plots a comparison between the histogram of intervals and the multiresolution ridge persistency"
  (format t "histogram of intervals ~a~%" (multiple-value-list (get-histogram interval-histogram)))
  (let* ((interval-counts (make-narray (get-histogram-counts interval-histogram)))
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
  (format t "histogram of intervals ~a~%" (multiple-value-list (get-histogram interval-histogram)))
  (multiple-value-bind (elements counts) (get-histogram interval-histogram)
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
      (plot-command "set xrange [*:~d]" (1+ largest-interval))
      (plot-command "set yrange [*:1.1]")
      (plot (normalise interval-counts) (.iseq 1 largest-interval)
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
    (plot-binned-histogram (make-narray iois-of-rhythm) 
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
			    (format nil "Ridge persistency of ~a" (name rhythm-to-plot))
			    :sample-rate (sample-rate rhythm-to-plot))
    (close-window)
    (metrical-rhythm-expectancies (list rhythm-to-plot) meter number-of-bars 
				  (format nil "~a (RP)" (name rhythm-to-plot))
				  :expectation-generator #'expectancies-of-rhythm-ridge-persistency)
    (metrical-rhythm-expectancies (list rhythm-to-plot) meter number-of-bars
 				  (format nil "~a (ending peaks)" (name rhythm-to-plot))
 				  :expectation-generator #'expectancies-of-rhythm)
    (metrical-rhythm-expectancies (list rhythm-to-plot) meter number-of-bars
				  (format nil "~a (integration)" (name rhythm-to-plot))
				  :expectation-generator #'expectancies-of-rhythm-integrator)
    analysis))

(defun plot-persistency-of-performed-rhythm (rhythm-to-plot)
  (let* ((analysis (analysis-of-rhythm rhythm-to-plot :padding #'causal-pad))
	 (relative-iois (intervals-as-ratios rhythm-to-plot))
	 (ridge-persistency (unweighted-ridge-persistency-of analysis)))
    (write-as-audio rhythm-to-plot
		    #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/test_sound.wav"
		    #P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff")
    ;; (plot-interval-histogram (interval-histogram (list iois-of-rhythm)) (name rhythm-to-plot)) 
    (plot-binned-histogram relative-iois 
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

#|
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

(setf mo (append-rhythm m (random-offset 100)))
(pushnew 'ridge-phase *plotting*)
(list (last-expectations mo :last-time (last-onset-time mo)) ; last-onset-expect-mo
      (last-expectations mo :last-time 799) ; last-meter-expect-mo
      (last-expectations mo)) ; last-moment-expect-mo
;; (plot-expectancies-histogram (last-expectations mo) (name mo))
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

;;;; Performed rhythms

(plot-binned-histogram (intervals-as-ratios r) "Relative Intervals")
(plot-binned-histogram (make-narray one-iois) "IOIs" :bin-size 1)

(plot-interval-histogram (interval-histogram (list (nlisp::array-to-list (intervals-as-ratios r))))
			 "performed-44")

|#
