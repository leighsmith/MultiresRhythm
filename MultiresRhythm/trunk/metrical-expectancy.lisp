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

;; 20mS
(defun make-envelope (amplitude &key (sample-rate 200) (attack 0.020) (decay 0.0) (sustain 0.0) (release 0.020))
  (let* ((attack-samples (round (* attack sample-rate)))
	 (release-samples (round (* release sample-rate)))
	 (duration-samples (round (* sustain sample-rate)))
	 (sustain-samples (- duration-samples attack-samples release-samples)))
    (.concatenate (.rseq 0.0 amplitude attack-samples) 
		  (.rseq amplitude amplitude sustain-samples)
		  (.rseq amplitude 0.0 release-samples))))

(defun rhythm-of-part (name note-list &key (sample-rate 200) (attack 0.020) (release 0.020))
  "Generate a rhythm by modualting the articulation"
  (let* ((last-note (first (last note-list)))
	 (rhythm-length (1+ (round (* (+ (first last-note) (second last-note)) sample-rate))))
	 (time-signal (make-double-array rhythm-length))
	 (attack-samples (round (* attack sample-rate)))
	 (release-samples (round (* release sample-rate))))
    (loop
       for (time duration amplitude) in note-list
       for onset-time-in-samples = (round (* time sample-rate))
       for duration-samples = (round (* duration sample-rate))
       for sustain-samples = (- duration-samples attack-samples release-samples)
       for envelope = (.concatenate (.rseq 0.0 amplitude attack-samples) 
 				    (.rseq amplitude amplitude sustain-samples)
 				    (.rseq amplitude 0.0 release-samples))
       do (setf (.subarray time-signal (list 0 (list onset-time-in-samples 
 						     (+ onset-time-in-samples duration-samples))))
 		envelope))
    (make-instance 'rhythm
		   :name name
		   :description name
		   :time-signal time-signal
		   :sample-rate sample-rate)))

(defun create-probe-rhythms (name weighted-onsets &key (probe-base-time 3.0d0) 
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
(defun random-metrical-rhythm-of-meter (meter &key (number-of-bars 2))
  (let* ((bar-length (reduce #'* meter))
	 (rhythm-name (format nil "~d bar random rhythm in ~a meter" number-of-bars meter))
	 (random-metrical-rhythm (shoe::pattern-from-grammar-long-segment 0 (* number-of-bars bar-length)
									  bar-length meter)))
    ;; An entire bar is 1 second = 200 samples. 180BPM
    (values (iois-to-rhythm rhythm-name 
			    random-metrical-rhythm 
			    :shortest-ioi (/ 200.0 bar-length) 
			    :sample-rate 200)
	    random-metrical-rhythm)))

;; (setf random-44 (random-metrical-rhythm-of-meter '(2 2 2 2)))
;; (random-metrical-rhythm-of-meter '(3 2 2))
;; (random-metrical-rhythm-of-meter '(3 2))

(defun probed-random-rhythm-of-meter (meter)
  (let* ((metrical-rhythm (random-metrical-rhythm-of-meter meter))
	 (empty+probe-rhythms (create-probe-rhythms "empty+probe" '((0.0 1.0) 
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

(defun create-metrical-set (candidate-meter size-of-rhythm-set &key (number-of-bars 6))
  "Return a set of unique metrical rhythms"
  (loop ; repeat size-of-rhythm-set ;; needs to be 
     while (< (length metrical-interval-set) size-of-rhythm-set)
     for (rhythm intervals) = (multiple-value-list (random-metrical-rhythm-of-meter candidate-meter 
						    :number-of-bars number-of-bars))
     when (not (member intervals metrical-interval-set :test #'equal))
     collect intervals into metrical-interval-set
     ;; and collect rhythm into metrical-rhythm-set
     and collect (append-rhythm rhythm (random-offset 100)) into metrical-rhythm-set
     ;; (append rhythm (rhythm-of-weighted-onsets "empty duration" '((0.0 1.0) (1.0 0.0))))
     finally (return metrical-rhythm-set)))

(defun write-rhythms (rhythms name)
  (loop
     for rhythm in rhythms
     for index = 1 then (1+ index)
     do (write-as-audio rhythm
			(make-pathname :directory "/Volumes/iDisk/Research/Data/RicardsOnsetTests/UnAccented/"
				       :name (format nil "~a-~d" name index)
				       :type "wav")
			#P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff")))

(defun plot-expectancies (all-expectations title)
  (let ((expect-times (make-narray (mapcar (lambda (expect) (time-in-seconds expect 200.0)) all-expectations)))
	(expect-confidences (make-narray (mapcar (lambda (expect) (confidence expect)) all-expectations))))
     (plot expect-confidences expect-times 
 	  :style "points" :xlabel "Time" :ylabel "Confidence" 
 	  :aspect-ratio 0.66
 	  :title title)))

;;; Sum all values falling within the given bins ranges. Effectively integrate.
;;; Could sort by time value. Return the sum and the average of the confidences in the bins.
(defun time-bins (all-expectations &key (bin-size 0.025))
  (let* ((expect-times (make-narray (mapcar (lambda (expect) (time-in-seconds expect 200.0)) all-expectations)))
	 (expect-confidences (make-narray (mapcar (lambda (expect) (confidence expect)) all-expectations)))
	 (number-of-bins (ceiling (/ (range expect-times) bin-size)))
	 (bin-boundaries (.rseq (.min expect-times) (.max expect-times) number-of-bins))
	 (bins (make-double-array number-of-bins)))
    (loop
       for bin-index from 1 below number-of-bins
       ;; determine which elements in expect-times (& therefore expect-confidences) are within the bin.
       for in-bin = (.and (.>= expect-times (.aref bin-boundaries (1- bin-index)))
			  (.< expect-times (.aref bin-boundaries bin-index)))
       for in-bin-count = (.sum in-bin)
       do (progn
	    ;; (format t "from ~a to ~a ~a values~%" (.aref bin-boundaries (1- bin-index)) (.aref bin-boundaries bin-index) in-bin-count)
	    (setf (.aref bins bin-index) (.sum (.* in-bin expect-confidences))))
	    ;; (setf (.aref bins bin-index) (if (zerop in-bin-count)
       ;; 				     in-bin-count
       ;; 			     (/ (.sum (.* in-bin expect-confidences)) in-bin-count))))
       finally (return (values bins bin-boundaries)))))

(defun plot-expectancies-histogram (all-expectations title)
  (multiple-value-bind (prediction-counts time-bins)
      (time-bins all-expectations)
    (plot prediction-counts time-bins
 	  :style "boxes fill solid 1.0 border -1"
	  :xlabel "Time (Seconds)" 
	  :ylabel "Accumulated Confidence" 
 	  :aspect-ratio 0.66
 	  :title title)))

(defun metrical-rhythm-expectancies (candidate-meter &key (sample-size 40) (number-of-bars 2))
  "Given a meter, create a set of random metrical rhythms plot the confidences of each expectation"
  (let* ((random-metrical-rhythms (create-metrical-set candidate-meter sample-size :number-of-bars number-of-bars))
	 (expectancy-set (mapcar #'last-expectations random-metrical-rhythms))
	 ;; (expectancy-set (mapcar (lambda (r) (last-expectations r :last-time (last-onset-time r))) random-metrical-rhythms))
	 ;; (expectancy-set (mapcar (lambda (r) (last-expectations r :last-time 799)) random-metrical-rhythms))
	 ;; Make all expectations into a single list for easy traversal.
	 (all-expectations (reduce #'append expectancy-set)))
    (window)
    (plot-expectancies all-expectations 
		       (format nil "Expectation Confidences for ~a examples of ~a bars of ~a meter" 
			       sample-size number-of-bars candidate-meter))
    (window)
    (plot-expectancies-histogram all-expectations 
				 (format nil "Accumulated Expectation Confidences for ~a examples of ~a bars of ~a meter" 
					 sample-size number-of-bars candidate-meter))))

#|
;; zero valued last point to stretch the rhythm
(setf jongsma-ternary-meter-rhythms 
      (create-probe-rhythms "ternary-meter" '((0.0   1.0) 
					      (0.333 0.43) 
					      (0.666 0.43) 
					      (1.0   1.0)
					      (1.333 0.43) 
					      (1.666 0.43) 
					      (2.0 1.0) 
					      (3.0 1.0) 
					      (4.0 0.0))))

(setf jongsma-binary-meter-rhythms 
      (create-probe-rhythms "binary-meter" '((0.0 1.0) 
					     (0.5 0.43) 
					     (1.0 1.0) 
					     (1.5 0.43)
					     (2.0 1.0) 
					     (3.0 1.0) 
					     (4.0 0.0))))


(setf binary-meter (rhythm-of-part "binary-meter" '(( 0.0 0.350 1.0) 
						    ( 2.0 0.255 1.0) 
						    ( 4.0 0.350 1.0) 
						    ( 6.0 0.255 1.0)
						    ( 8.0 0.350 1.0) 
						    (10.0 0.255 1.0) 
						    (12.0 0.350 1.0) 
						    (14.0 0.255 0.5) 
						    (16.0 0.350 1.0) 
						    (18.0 0.255 1.0)
						    (20.0 0.350 1.0) 
						    (22.0 0.255 1.0)
						    (24.0 0.350 1.0) 
						    (26.0 0.255 1.0)
						    (28.0 0.350 1.0) 
						    (30.0 0.255 1.0))))

(plot-rhythm binary-meter)

(setf binary-analysis (analysis-of-rhythm binary-meter))
(plot-cwt-of-rhythm (scaleogram binary-analysis) binary-meter)
(setf binary-ridges (ridges-at-time (skeleton binary-analysis) 3026))
(setf binary-duration (time-support (make-narray (mapcar (lambda (ridge) (scale-at-time ridge 3026)) binary-ridges)) 16))


(setf ternary-meter (rhythm-of-part "ternary-meter" '(( 0.0 0.350 1.0) 
						      ( 2.0 0.255 1.0) 
						      ( 4.0 0.255 1.0) 
						      ( 6.0 0.350 1.0)
						      ( 8.0 0.255 1.0) 
						      (10.0 0.255 1.0) 
						      (12.0 0.350 1.0) 
						      (14.0 0.255 0.5) 
						      (16.0 0.255 1.0) 
						      (18.0 0.350 1.0)
						      (20.0 0.255 1.0) 
						      (22.0 0.255 1.0)
						      (24.0 0.350 1.0) 
						      (26.0 0.255 1.0)
						      (28.0 0.255 1.0) 
						      (30.0 0.350 1.0))))


(plot-rhythm ternary-meter)

(setf ternary-analysis (analysis-of-rhythm ternary-meter))
(plot-cwt (scaleogram ternary-analysis))
;; Would seem to be the same period corresponding to the IOI rate (400 samples) & 
;; no meter is induced

(setf binary-meter-amplitude (rhythm-of-weighted-onsets "binary-meter" '(( 0.0 1.0) 
									 ( 2.0 0.43) 
									 ( 4.0 1.0) 
									 ( 6.0 0.43)
									 ( 8.0 1.0) 
									 (10.0 0.43) 
									 (12.0 1.0) 
									 (14.0 0.43) 
									 (16.0 1.0) 
									 (18.0 0.43)
									 (20.0 1.0) 
									 (22.0 0.43)
									 (24.0 1.0) 
									 (26.0 0.43)
									 (28.0 1.0) 
									 (30.0 0.43))))

(setf ternary-meter-amplitude (rhythm-of-weighted-onsets "ternary-meter" '(( 0.0 1.0) 
									   ( 2.0 0.43) 
									   ( 4.0 0.43) 
									   ( 6.0 1.0)
									   ( 8.0 0.43) 
									   (10.0 0.43) 
									   (12.0 1.0) 
									   (14.0 0.0) 
									   (16.0 0.43) 
									   (18.0 1.0)
									   (20.0 0.43) 
									   (22.0 0.43)
									   (24.0 1.0) 
									   (26.0 0.43)
									   (28.0 0.43) 
									   (30.0 1.0))))

(plot-rhythm jongsma-binary-meter)
;;; Test with causal padding.
(setf jongsma-binary-analysis (analysis-of-rhythm jongsma-binary-meter))
(plot-cwt-of-rhythm (scaleogram jongsma-binary-analysis) jongsma-binary-meter)

(setf jongsma-ternary-meter (rhythm-of-weighted-onsets "ternary-meter" '((0.0   1.0) 
									 (0.333 0.43) 
									 (0.666 0.43) 
									 (1.0   1.0)
									 (1.333 0.43) 
									 (1.666 0.43) 
									 (2.0 1.0) 
									 (3.0 1.0) 
									 (4.0 0.0))))

(setf jongsma-ternary-analysis (analysis-of-rhythm jongsma-ternary-meter))
(plot-cwt-of-rhythm (scaleogram jongsma-ternary-analysis) jongsma-ternary-meter)

(write-as-audio (random-metrical-rhythm-of-meter '(2 2 2 2))
		#P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/test_sound.wav"
		#P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff")

(write-as-audio jongsma-ternary-meter
		#P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/ternary_sound.wav"
		#P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff")

(write-as-audio (random-metrical-rhythm-of-meter '(3 2))
		#P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/test_sound.wav"
		#P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff")

(pushnew 'weighted-beat-ridge *plotting*)
;; (setf random-44 (random-metrical-rhythm-of-meter '(2 2 2 2)))
(setf e (expectancies-of-rhythm-integrator random-44 :times-to-check (list (1- (duration-in-samples random-44)))))


;; (dolist (candidate-meter '((3 2) (2 2)))

(setf short-ternary-meter (rhythm-of-weighted-onsets "ternary-meter" '((0.0   1.0) 
								       (0.333 0.43) 
								       (0.666 0.43) 
								       (1.0   1.0)
								       (1.333 0.43) 
								       (1.666 0.43) 
								       (2.0   1.0) 
								       (3.0   0.0))))

(setf short-binary-meter (rhythm-of-weighted-onsets "binary-meter" '((0.0 1.0) 
								     (0.5 0.43) 
								     (1.0 1.0) 
								     (1.5 0.43)
								     (2.0 1.0) 
								     (3.0 0.0))))

(multiple-value-setq (m r) (random-metrical-rhythm-of-meter '(3 2) :number-of-bars 4))
(setf r '(2 1 1 2 6 1 1 1 1 1 1 1 1 1 1 2))
(setf m (iois-to-rhythm "blah" r :shortest-ioi (/ 200.0 6) :sample-rate 200))
(write-as-audio m
		#P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/test_sound.wav"
		#P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff")
;; Compare integrator vs. last-onset magnitude & phase.
(last-expectations m)
(last-onset-expectations m)

;; (metrical-rhythm-expectancies '(3 2) :number-of-bars 4 :sample-size 60)
;; (metrical-rhythm-expectancies '(2 2) :number-of-bars 4 :sample-size 60)

(setf mo (append-rhythm m (random-offset 100)))
(pushnew 'ridge-phase *plotting*)
(list (last-expectations mo :last-time (last-onset-time mo)) ; last-onset-expect-mo
      (last-expectations mo :last-time 799) ; last-meter-expect-mo
      (last-expectations mo)) ; last-moment-expect-mo
|#
