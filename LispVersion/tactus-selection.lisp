;;;; -*- Lisp -*-
;;;;
;;;; Functions for selecting the tactus given the rhythm and multiresolution analysis.
;;;;
;;;; In nlisp (Matlab-like Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <leigh@leighsmith.com> 
;;;;
;;;; Copyright (c) 2007-2014, All Rights Reserved.
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

;;; Declarations

(defgeneric tactus-for-rhythm (rhythm-to-analyse &key voices-per-octave tactus-selector)
  (:documentation "Returns the selected tactus for the rhythm."))

(defgeneric choose-tactus (rhythm-to-analyse analysis &key tactus-selector)
  (:documentation "Returns the selected tactus for the rhythm."))

;;; Tactus selectors
(defgeneric select-longest-lowest-tactus (rhythm-to-analyse analysis)
  (:documentation "Returns the longest duration and lowest scale ridge."))

(defgeneric select-longest-highest-tactus (rhythm-to-analyse analysis)
  (:documentation "Returns the longest duration and highest scale ridge."))

(defgeneric create-weighted-beat-ridge (rhythm-to-analyse analysis)
  (:documentation "Returns the ridge by cumulative evidence with absolute tempo weighting"))

(defgeneric select-probable-tactus (rhythm-to-analyse analysis)
  (:documentation "Returns the ridge by probabilistic modelling"))

;;; Implementation

(defmethod select-longest-tactus ((performed-rhythm rhythm) (rhythm-analysis multires-analysis) &key (scale-order #'>))
  "Returns the longest duration and either highest or lowest scale ridge."
  (let* ((skeleton-to-analyse (skeleton rhythm-analysis))
	 (max-ridge (make-instance 'ridge)))
    (dolist (ridge (ridges skeleton-to-analyse))
      (if (or (> (duration-in-samples ridge) (duration-in-samples max-ridge))
	      ;; average-scale returns scale numbers indexed from the highest scales, so 
	      (and (eql (duration-in-samples ridge) (duration-in-samples max-ridge))
		   (funcall scale-order (average-scale ridge) (average-scale max-ridge))))
	  (setf max-ridge ridge)))
    (list max-ridge)))

(defmethod select-longest-lowest-tactus ((performed-rhythm rhythm) (rhythm-analysis multires-analysis))
  "Returns the longest duration and lowest scale ridge."
  (select-longest-tactus performed-rhythm rhythm-analysis :scale-order #'>))

(defmethod select-longest-highest-tactus ((performed-rhythm rhythm) (rhythm-analysis multires-analysis))
  "Returns the longest duration and highest scale ridge"
  (select-longest-tactus performed-rhythm rhythm-analysis :scale-order #'<))

(defun max-scale-of-profile (scale-profile &key (neighbourhood-size 3))
  "Given a profile of scales at a given time point, estimate the fractional scale maximum"
  (let* ((index-of-max (argmax scale-profile))
	 (neighbourhood-start (- index-of-max (floor neighbourhood-size 2)))
	 (indexes-of-neighbourhood (.iseq neighbourhood-start (+ neighbourhood-start (1- neighbourhood-size))))
	 (maximal-neighbourhood (.arefs scale-profile indexes-of-neighbourhood)))
    (nlisp::peak-x (.* indexes-of-neighbourhood 1d0) maximal-neighbourhood 15)))

;; TODO expand to return a list of ridges when there is a discontinuity.
(defun ridges-of-max-scales (magnitude-profile)
  "Returns a single ridge along the magnitude-scale plane maximum"
  (loop
     for time from 0 below (.array-dimension magnitude-profile 1)
     for scale-profile = (.column magnitude-profile time)
     ;; Maximum peak of energy is assumed to be the beat scale.
     for beat-scale = (argmax scale-profile)
     ;; for beat-scale = (max-scale-of-profile scale-profile)  ; For fractional scales.
     collect beat-scale into beat-scales  ; For integer scales.
     finally (return (make-instance 'ridge
				    :start-sample 0 ; TODO could be when beat period is confirmed.
				    :scales beat-scales))))

(defun steady-rhythm-transition-probabilities (number-of-scales &key 
						 (tempo-waver-low 0.8d0)
						 (tempo-waver-hi 0.15d0)
						 (tempo-change-prob 0.1d0))
  "Generate a scale transition probability matrix that favours remaining on the same
  scale, progressively penalising larger transitions. tempo-waver-lo and hi specify
  the ratio of path inertia away from the path that is acceptable for 1 and 2 scales respectively."
    ;; Now assign the inertia entries.
    (loop
       ;; Layout the path steady rhythm transitions in relative terms w.r.t staying on the same
       ;; path (the diagonal of the transition matrix), then normalise to become a probablity
       ;; distribution. This is easier as it takes into account edges of the transition matrix.
       with scale-change-prob = (/ (* (- 1.0d0 tempo-change-prob) tempo-change-prob)
				   (- number-of-scales 5))
       ;; Calculate the probability the tempo will jump to any other scale other than a 1 or 2
       ;; scale deviation either side of the same scale. This means all other scales other than
       ;; those 5.
       with transition-probs = (make-double-array (list number-of-scales number-of-scales)
						  :initial-element scale-change-prob)
       for row-index from 0 below number-of-scales 
       for transition-row = (.row transition-probs row-index)
       do 
	 (setf (.aref transition-row row-index) 1.0d0)
	 (if (< row-index (- number-of-scales 2))
	     (setf (.aref transition-row (+ row-index 1)) tempo-waver-low))
	 (if (>= row-index 1)
	     (setf (.aref transition-row (- row-index 1)) tempo-waver-low))
	 (if (< row-index (- number-of-scales 3))
	     (setf (.aref transition-row (+ row-index 2)) tempo-waver-hi))
	 (if (>= row-index 2)
	     (setf (.aref transition-row (- row-index 2)) tempo-waver-hi))
       ;; Now scale each row to sum to 1.0, making it a probability distribution.
       ;; TODO do this per row.
	 (setf (.row transition-probs row-index) (./ transition-row (.sum transition-row)))
       finally (return transition-probs)))

;; Models ridge extraction as a hidden Markov model, with the ridge extracted as the
;; hidden state. The scaleogram-probabilities are the observation probability distribution 
;; for each scale. tempo-beat-preference is the initial state probability distribution.
(defun select-probable-ridge (scaleogram-magnitudes tempo-beat-preference &key (path-inertia 0.9d0))
  "Returns the ridge which is most likely, based on observations reflected in the
   scaleogram magnitude observation matrix, initialised by the tempo beat preference. 
   Path-inertia is the probability of staying on the same tempo."
  (let* ((number-of-scales (.row-count scaleogram-magnitudes))
	 ;; normalise the magnitudes at each time window into observation probabilities.
	 (scaleogram-probabilities (probability-distribution scaleogram-magnitudes))
	 (transition-probs (steady-rhythm-transition-probabilities number-of-scales
								   :tempo-change-prob (- 1.0d0 path-inertia)))
	 (state-path (viterbi tempo-beat-preference scaleogram-probabilities transition-probs)))
    (diag-plot 'scaleogram-probabilities
      ;; (plot-magnitude scaleogram-probabilities))
      (plot-image #'highlighted-ridges-image
		  (list (list (make-instance 'ridge :start-sample 0 :scales (nlisp::array-to-list state-path))) scaleogram-probabilities)
		  '((1.0 0.5) (0.0 0.0))
		  ""))
    (diag-plot 'scaleogram-probability-profile
      (plot (.reverse (.column scaleogram-probabilities 480)) nil :title (format nil "scale at 480 ~a~%" (.aref state-path 480))))
    (diag-plot 'transition-probabilities
      (image transition-probs nil nil))
    (make-instance 'ridge :start-sample 0 :scales (nlisp::array-to-list state-path))))

(defmethod create-weighted-beat-ridge ((rhythm-to-analyse rhythm) (analysis multires-analysis))
  "Creates a ridge using the maximum value of the cumulative sum of the scaleogram energy"
  (let* ((scaleogram (scaleogram analysis))
	 (cumulative-scale-persistency (cumsum (scaleogram-magnitude scaleogram)))
	 (vpo (voices-per-octave scaleogram))
	 (sample-rate (sample-rate rhythm-to-analyse))
	 (salient-scale (preferred-tempo-scale vpo sample-rate))
	 (tempo-beat-preference (tempo-salience-weighting salient-scale (.array-dimensions cumulative-scale-persistency)
							  :octaves-per-stddev 0.8))
	 (tempo-preference-profile (./ (.column tempo-beat-preference 0) (.sum (.column tempo-beat-preference 0))))
 	 (weighted-persistency-profile (.* cumulative-scale-persistency tempo-beat-preference)))
    (diag-plot 'weighted-beat-ridge
      (plot-image #'coloured-magnitude-image (list weighted-persistency-profile) '((1.0 0.5) (0.0 0.3))
		  (axes-labelled-in-seconds scaleogram sample-rate 4)
		  :title (format nil "weighted persistency profile of ~a" (name rhythm-to-analyse))))
    ;; (ridges-of-max-scales weighted-persistency-profile)
    (select-probable-ridge weighted-persistency-profile tempo-preference-profile :path-inertia 0.97)))

(defmethod weighted-persistency-beat-ridge ((rhythm-to-analyse rhythm) (analysis multires-analysis))
  "Creates a ridge using the maximum value of the cumulative sum of the normalised ridges"
  (let* ((scaleogram (scaleogram analysis))
	 (cumulative-ridge-persistency (cumsum (make-ridge-plane (skeleton analysis))))
	 (vpo (voices-per-octave scaleogram))
	 (sample-rate (sample-rate rhythm-to-analyse))
	 (salient-tempo-scale (preferred-tempo-scale vpo sample-rate))
	 ;; Tuned filter that gives the future projections more space.
	 (tempo-beat-preference (tempo-salience-weighting salient-tempo-scale 
							  (.array-dimensions cumulative-ridge-persistency)
							  :envelope #'skewed-gaussian-envelope
							  :octaves-per-stddev 2.0))
 	 (weighted-persistency-profile (.* cumulative-ridge-persistency tempo-beat-preference)))
    (diag-plot 'weighted-beat-ridge
      (plot-image #'magnitude-image (list weighted-persistency-profile) '((1.0 0.5) (0.0 0.3))
		  (axes-labelled-in-seconds scaleogram sample-rate 4)
		  :title (format nil "weighted persistency profile of ~a" (name rhythm-to-analyse))))
    (ridges-of-max-scales weighted-persistency-profile)))

(defmethod select-weighted-beat-ridge ((rhythm-to-analyse rhythm) (analysis multires-analysis))
  "Selects a ridge using the maximum value of the tempo weighted scaleogram energy"
  (let* ((scaleogram (scaleogram analysis))
	 (magnitude (scaleogram-magnitude scaleogram))
	 (vpo (voices-per-octave scaleogram))
	 (sample-rate (sample-rate rhythm-to-analyse))
	 (salient-scale (preferred-tempo-scale vpo sample-rate))
	 (tempo-beat-preference (tempo-salience-weighting salient-scale (.array-dimensions magnitude)
							  :octaves-per-stddev 1.0))
 	 (weighted-magnitude (.* magnitude tempo-beat-preference)))
    (diag-plot 'weighted-beat-ridge
      (plot-image #'magnitude-image (list weighted-magnitude) '((1.0 0.5) (0.0 0.3))
		  (axes-labelled-in-seconds scaleogram sample-rate 4)
		  :title (format nil "weighted magnitude profile of ~a" (name rhythm-to-analyse))))
    (ridges-of-max-scales weighted-magnitude)))

(defun create-beat-ridge (rhythm-to-analyse analysis)
  "Creates a ridge using the maximum value of the cumulative sum of the scaleogram energy"
  (let* ((scaleogram (scaleogram analysis))
	 (cumulative-scale-persistency (cumsum (scaleogram-magnitude scaleogram))))
	 ;; (vpo (voices-per-octave scaleogram)))
	 ;; (sample-rate (sample-rate rhythm-to-analyse))
    (ridges-of-max-scales cumulative-scale-persistency)))

(defmethod select-tactus-by-beat-multiple ((performed-rhythm rhythm) (rhythm-analysis multires-analysis))
  "Returns the ridges that are the most commonly occurring lowest integer multiples of the beat"
  (let* ((skeleton (skeleton rhythm-analysis))
	 (vpo (voices-per-octave skeleton))
	 ;; TODO Try Gaussian weighting the scale-persistency?
 	 (beat-period (unweighted-beat-period-of-rhythm performed-rhythm (scaleogram rhythm-analysis)))
 	 ;; (beat-scale (scale-from-period beat-period vpo))
 	 (beat-multiples (.iseq 1 7))  ; determines the bar periods
 	 (candidate-bar-periods (.* beat-period beat-multiples))
 	 (candidate-bar-scales (scale-from-period candidate-bar-periods vpo)))
    (format t "Checking multiples for beat period ~a, as ~a~%" beat-period (.round candidate-bar-scales))
    (loop
       for candidate-bar-scale across (val (.round candidate-bar-scales))
       ;; Retrieve ridges possessing a candidate-bar-scale
       for candidate-bar-scale-ridges = (ridges-containing-scale skeleton candidate-bar-scale)
       ;; Measure total duration covered by all candidate bar scale ridges, 
       for candidate-ridge-duration = (reduce #'+ (mapcar #'duration-in-samples candidate-bar-scale-ridges))
       ;; TODO weight by distances of scales in ridge from the nominated scale.
       ;; do (format t "For candidate-bar-scale ~a, period ~a~%" candidate-bar-scale (time-support candidate-bar-scale vpo))
       ;; Longest total duration is the selected ridge.
       maximizing candidate-ridge-duration into max-candidate-ridge-duration
       collecting (list candidate-ridge-duration candidate-bar-scale-ridges) into ridge-totals
       ;; finally (format t "candidate ridges and durations ~a~%" ridge-totals)
       finally (return (second (find max-candidate-ridge-duration ridge-totals :key #'first))))))

(defmethod create-weighted-windowed-beat-ridge ((rhythm-to-analyse rhythm) (analysis multires-analysis))
  "Creates a ridge using the maximum value of the windowed sum of the scaleogram energy"
  (let* ((window-in-seconds 5d0)
	 (window-in-samples (floor (* (sample-rate rhythm-to-analyse) window-in-seconds)))
	 (scaleogram (scaleogram analysis))
	 (windowed-scale-persistency (window-integration (scaleogram-magnitude scaleogram) window-in-samples))
	 (vpo (voices-per-octave scaleogram))
	 (sample-rate (sample-rate rhythm-to-analyse))
	 (salient-scale (preferred-tempo-scale vpo sample-rate))
	 (tempo-beat-preference (tempo-salience-weighting salient-scale (.array-dimensions windowed-scale-persistency)))
 	 (weighted-persistency-profile (.* windowed-scale-persistency tempo-beat-preference)))
    (image (.flip weighted-persistency-profile) nil nil :aspect-ratio 0.2 :title "weighted persistency profile")
    (ridges-of-max-scales weighted-persistency-profile)))

;;; Can return bar-scale to emulate create-beat-ridge
(defun determine-beat-scale-at-time (cumulative-scale-persistency time vpo &key diagnose)
  (let* ((scale-persistency-profile (.column cumulative-scale-persistency time))
	 (max-period (time-support (1- (.length scale-persistency-profile)) vpo))
	 (average-persistency (mean scale-persistency-profile))
	 ;; Maximum peak of energy is assumed to be the beat scale.
	 (beat-scale (argmax scale-persistency-profile))
	 (beat-period (time-support beat-scale vpo))
	 (max-multiple (floor max-period beat-period))
	 ;; Ensure there are no candidate bar scales that exceed the total scales, but
	 ;; check all multiples up to 7/8 periods.
	 (beat-multiples (.iseq (min 2 max-multiple) (min 7 max-multiple)))
	 (candidate-bar-periods (.* beat-period beat-multiples))
	 (candidate-bar-scales (.round (scale-from-period candidate-bar-periods vpo)))
	 ;; look at the scale persistency profiles at candidate-bar-scales locations
	 (bar-scale-profiles (.arefs scale-persistency-profile candidate-bar-scales))
	 (highest-bar-scale-profile (.max bar-scale-profiles))
	 (bar-scale-index (position highest-bar-scale-profile (val bar-scale-profiles))) ; (argmax bar-scale-profiles)
	 (bar-scale (.aref candidate-bar-scales bar-scale-index)))
    (if diagnose
	(let* ((selected-scales (make-double-array (.array-dimensions scale-persistency-profile))))
	  (format t "average-persistency ~a beat-scale ~a beat-period ~a bar-scale ~a~%candidate-bar-periods ~a~%candidate-bar-scales ~a~%bar-scale-profiles ~a~%"
		  average-persistency beat-scale beat-period bar-scale candidate-bar-periods candidate-bar-scales bar-scale-profiles)
	  (setf (.arefs selected-scales candidate-bar-scales) bar-scale-profiles)
	  (plot (list scale-persistency-profile selected-scales) nil
		 :styles '("lines" "impulses") :aspect-ratio 0.66)))
    ;; Compare the height of the candidate peak against the average height
    ;; (or area?) to get a degree of assurance.
    (if (> highest-bar-scale-profile average-persistency) bar-scale beat-scale)))

(defmethod create-bar-ridge ((rhythm-to-analyse rhythm) (analysis multires-analysis))
  "Creates a ridge using the maximum value of the cumulative sum of the scaleogram energy"
  (let* ((window-in-seconds 2d0)
	 (window-in-samples (floor (* (sample-rate rhythm-to-analyse) window-in-seconds)))
	 (scaleogram (scaleogram analysis))
	 ;; Cumulative persistency is a moving window.
	 (windowed-scale-persistency (window-integration (scaleogram-magnitude scaleogram) window-in-samples))
	 (vpo (voices-per-octave scaleogram)))
    (image windowed-scale-persistency nil nil :aspect-ratio 0.2)
    (loop
       for time from 0 below (duration-in-samples scaleogram)
       collect (determine-beat-scale-at-time windowed-scale-persistency time vpo) into beat-scales
       finally (return (make-instance 'ridge
				      :start-sample 0 ; could be when beat period is confirmed.
				      :scales beat-scales)))))

;; (defmethod create-bar-ridge ((rhythm-to-analyse rhythm) (analysis multires-analysis))
;;   "Creates a ridge using the maximum value of the cumulative sum of the scaleogram energy"
;;   (let* ((window-in-seconds 2d0)
;; 	 (scaleogram (scaleogram analysis))
;; 	 ;; Cumulative persistency should be a moving window.
;; 	 (cumulative-scale-persistency (cumsum (scaleogram-magnitude scaleogram)))
;; 	 (vpo (voices-per-octave scaleogram)))
;;     (loop
;;        for time from 0 below (duration-in-samples scaleogram)
;;        collect (determine-beat-scale-at-time cumulative-scale-persistency time vpo) into beat-scales
;;        finally (return (make-instance 'ridge
;; 				      :start-sample 0 ; could be when beat period is confirmed.
;; 				      :scales beat-scales)))))

(defmethod select-probable-tactus ((performed-rhythm rhythm) (rhythm-analysis multires-analysis))
  "Returns the ridge with most evidence using Viterbi decoding to trace the ridge"
  (let* ((path-inertia 0.95d0) ;; Probability of staying on the same path.
	 (scaleogram (scaleogram rhythm-analysis))
	 (vpo (voices-per-octave scaleogram))
	 (scaleogram-magnitude (scaleogram-magnitude scaleogram))
	 (salient-scale (preferred-tempo-scale vpo (sample-rate rhythm-analysis)))
	 ;; Create a Gaussian envelope spanning the number of scales. 
	 (tempo-beat-preference (tempo-salience-weighting salient-scale (.array-dimensions scaleogram-magnitude)
							  :octaves-per-stddev 0.8))
	 ;; Normalise to a probability distribution
	 (tempo-preference-profile (./ (.column tempo-beat-preference 0) (.sum (.column tempo-beat-preference 0)))))
    (select-probable-ridge (.* scaleogram-magnitude tempo-beat-preference) tempo-preference-profile :path-inertia path-inertia)))

(defmethod choose-tactus ((analysis-rhythm rhythm) (analysis multires-analysis)
			  &key (tactus-selector #'select-longest-lowest-tactus))
  "Returns the selected tactus given the rhythm."
  ;; select out the tactus from all ridge candidates.
  (let* ((chosen-tactus (funcall tactus-selector analysis-rhythm analysis))
	 (chosen-tactus-list (if (listp chosen-tactus) chosen-tactus (list chosen-tactus))))
    (format t "Chosen tactus ~a using ~a~%" chosen-tactus-list tactus-selector)
    (diag-plot 'tactus-ridge 
      (plot-ridge (first chosen-tactus-list)))
    (diag-plot 'cwt+skeleton ;; overlay tactus on skeleton.
      (plot-cwt+skeleton-of analysis chosen-tactus-list analysis-rhythm))
    (diag-plot 'cwt ;; overlay tactus on phase image.
      (plot-cwt+ridges (scaleogram analysis) chosen-tactus-list analysis-rhythm
		       ;; :phase-palette :greyscale
		       ;; :magnitude-palette :jet
		       :title (name analysis-rhythm)))
    chosen-tactus-list))

(defmethod tactus-for-rhythm ((analysis-rhythm rhythm) &key (voices-per-octave 16)
			      (tactus-selector #'select-longest-lowest-tactus))
  "Returns the selected tactus given the rhythm."
  (let* ((analysis (analysis-of-rhythm analysis-rhythm :voices-per-octave voices-per-octave)))
    (values (choose-tactus analysis-rhythm analysis :tactus-selector tactus-selector) analysis)))
