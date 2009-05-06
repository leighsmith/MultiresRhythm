;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Probabilistic model for finding the initial downbeat of the rhythm.
;;;;
;;;; In nlisp (Matlab-like Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2009
;;;;

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :multires-rhythm)
(use-package :nlisp)

;;; Free parameters.

;;; The number of amplitude profile alignments considered per bar. Defines the contrast
;;; and threshold between highest and lowest probability for all possible downbeat
;;; locations. Must be greater than the number of beats-per-measure. Shouldn't need
;;; adjustment. Could be infinity, it just saves memory.
(defparameter *max-amp-matches* 8)

;; Number of bars used in the matching of the amplitude profile.
(defparameter *bars-of-amplitude-profile* 2)

;; Number of bars used in the gap accent evaluation of the onset detection function.
(defparameter *bars-of-gap-profile* 2)

;; Resolution of the beat position estimation states in subdivisions of each beat (4 = semiquavers).
(defparameter *subdivisions-of-beat* 4)

(defparameter *plots-per-rhythm* 10)

(defparameter *measures-to-plot* nil)

;; TODO should be a closure, not a parameter.
(defparameter *measure-count* 0)

;;; Since the likelihoods are ordered, simply comparing the first and second likelihoods is
;;; sufficient to generalise a measure of likelihood into binary classes (low/high).
;;; Incorporating the average of the likelihoods indicates how much the first rises above the remainder.
(defun probability-of-estimate (match-scores)
  "Return a normalised single probability based on the contrast of the match-score (unnormalised likelihood) of the chosen value against the remaining values."
  (/ (- (.aref match-scores 0) (mean (.subseq match-scores 1))) (.aref match-scores 0)))

;; Make a histogram of the labels for a maximum of total-lables, adding the probabilities
;; where there are duplicates. 
(defun combine-probabilities (discrete-labels probabilities total-labels)
  "Combines any duplicated labels into a single value, adding the probabilities"
  (loop
     with label-tally = (make-double-array total-labels)
     for label across (val discrete-labels)
     for probability across (val probabilities)
     do (incf (.aref label-tally label) probability)
     finally (return label-tally)))

(defun maximum-state-values (discrete-labels descending-values total-labels)
  "Selects the highest values for each unique discrete-label for total-labels. Assumes the
  descending-values are sorted in descending order"
  (loop 
     with max-state-values = (make-double-array total-labels)
     for label across (val discrete-labels)
     for value across (val descending-values)
     do (if (zerop (.aref max-state-values label))
	    (setf (.aref max-state-values label) value))
     finally (return max-state-values)))

(defun count-occurrence-on-beats (rhythm condition description beats-per-measure beat-duration)
  "Returns a count of those beats that notes span that match the condition"
  (let*	((beat-count (make-double-array beats-per-measure))
	 (notes-matching-condition (.find condition)))
    (format t "notes matching ~a ~a~%" description notes-matching-condition)
    (if (plusp (.length notes-matching-condition))
	(let ((beats-matching-condition (.mod (.round (./ (onset-time-of-note rhythm notes-matching-condition) beat-duration)) 
					      beats-per-measure)))
	  (format t "onsets in fractional beats ~a~%"
		  (./ (onset-time-of-note rhythm notes-matching-condition) (coerce beat-duration 'double-float)))
	  (format t "downbeats ~d~%" beats-matching-condition)
	  ;; If the same downbeat occurs more than once, the probability increment is only
	  ;; done once. TODO must check if this right.
	  (setf (.arefs beat-count beats-matching-condition) 1d0)))
    beat-count))

#|
(defun silence-score (silence-evaluation-region comparison-region)
  "Values > 1 indicate the variation and mean of the silence evaluation region is less
   than the comparison region, i.e more likely to be silence."
  ;; Calculate the stddev & mean over the region of interest, 2 measures.
  (let* ((stddev-measures (stddev comparison-region))
	 (mean-measures (mean comparison-region))
	 ;; (coeff-of-variation (/ stddev-measures mean-measures))
	 ;; Stddev's are just RMS measures of the amplitude envelope. 
	 (stddev-silence-region (stddev silence-evaluation-region))
	 (mean-silence-region (mean silence-evaluation-region))
	 ;; (cov-silence-region (/ stddev-silence-region mean-silence-region))
	 (epsilon 0.01d0)
	 (silence-score (* (/ stddev-measures (if (zerop stddev-silence-region) epsilon stddev-silence-region))
			   (/ mean-measures (if (zerop mean-silence-region) epsilon mean-silence-region)))))

    ;; (setf (.aref downbeat-score downbeat-index) (if (zerop cov-silence-region)
    ;; 						 4.0d0 ; if we are in true silence
    ;; 						 (/ coeff-of-variation cov-silence-region)))
    
    ;;(format t "cov whole ~,3f cov silence ~,3f ratio ~,3f~%"
    ;;	    coeff-of-variation cov-silence-region (/ coeff-of-variation cov-silence-region))
    (format t "comparison stddev ~,3f mean ~,3f~%" stddev-measures mean-measures)
    (format t "silence stddev ~,3f mean ~,3f~%" stddev-silence-region mean-silence-region)
    (format t "max whole ~,3f max silence ~,3f~%"
	    (.max comparison-region) (.max silence-evaluation-region))
    (format t "silence score from intersection of stddev & mean ratios ~,3f~%" silence-score)
    silence-score))
|#

(defun silence-score (silence-evaluation-region comparison-region)
  "Values > 1 indicate the variation and mean of the silence evaluation region is less
   than the comparison region, i.e more likely to be silence."
  ;; Calculate the stddev & mean over the region of interest, 2 measures.
  (let* ((stddev-measures (stddev comparison-region))
	 (mean-measures (mean comparison-region))
	 (max-silence-region (.max silence-evaluation-region))
	 (epsilon 0.001d0) ; Avoid div by 0 errors.
	 (silence-score (/ (.max comparison-region) (if (zerop max-silence-region) epsilon max-silence-region))))
    (format t "comparison stddev ~,3f mean ~,3f~%" stddev-measures mean-measures)
    (format t "max whole ~,3f max silence ~,3f~%"
	    (.max comparison-region) (.max silence-evaluation-region))
    (format t "silence score from ratio of local and global maxima ~,3f~%" silence-score)
    silence-score))

(defmethod silence-evidence ((rhythm-to-analyse rhythm)
			     measure-start-sample ; beginning of our region of interest
			     measure-index
			     beat-durations-in-measure
			     subdivisions-of-beat)
  "From the rhythm, for one measure, starting at measure-start-sample, determines if each 
beat and subdivision thereof is a silent region"
  (let* ((bar-duration (.sum beat-durations-in-measure)) ; in samples
	 (beats-per-measure (.length beat-durations-in-measure))
	 (tatums-per-measure (* beats-per-measure subdivisions-of-beat))
	 (downbeat-score (make-double-array tatums-per-measure)) ; initialised to 0.0
	 (rhythm-length (duration-in-samples rhythm-to-analyse)))
  (dotimes (tatum-index tatums-per-measure
	    ;; Normalise the downbeat location likelihood, since there is only one location per measure.
	    ;; TODO however, that assumes a direct relationship between silence
	    ;; following and preceding a note and it being a downbeat.
	    ;; When returning P(downbeat) = 0 for all downbeat locations we indicate the lack of
	    ;; decision as all are equally likely and none contribute to the final decision.
	    (if (zerop (.sum downbeat-score)) downbeat-score (./ downbeat-score (.sum downbeat-score))))
    (let* ((beat-duration (.aref beat-durations-in-measure (floor tatum-index subdivisions-of-beat)))
	   (downbeat-location (round (* tatum-index (/ bar-duration tatums-per-measure))))
	   (gap-start (round (+ measure-start-sample downbeat-location (* beat-duration -0.5))))
	   (gap-end (min (+ gap-start beat-duration) rhythm-length))
	   (silence-score (silence-score (.subseq (time-signal rhythm-to-analyse) (max 0 gap-start) gap-end)
					 (time-signal rhythm-to-analyse))))
      (setf (.aref downbeat-score tatum-index) silence-score)
      (format t "Measure ~a beat location ~a silence region (~a ~a) score = ~,3f~%"
	      measure-index downbeat-location gap-start gap-end
	      (.aref downbeat-score tatum-index))))))

#|
(defmethod silence-evidence ((rhythm-to-analyse rhythm)
			     measure-start-sample ; begining of our region of interest
			     measure-index
			     beats-per-measure bar-duration beat-durations-in-measure)
  "Determines if each beat is a silent region"
  (loop
     with subbeats-per-measure = (* beats-per-measure *subdivisions-of-beat*)
     with downbeat-score = (make-double-array subbeats-per-measure) ; initialised to 0.0
     with rhythm-length = (duration-in-samples rhythm-to-analyse)
     with search-region = (.subseq (time-signal rhythm-to-analyse) measure-start-sample 
				   (min rhythm-length (+ measure-start-sample (1- (* bar-duration *bars-of-gap-profile*)))))
     for downbeat-index from 0 below subbeats-per-measure
     for beat-duration = (.aref beat-durations-in-measure (floor downbeat-index *subdivisions-of-beat*))
     for downbeat-location = (round (* downbeat-index (/ bar-duration subbeats-per-measure)))
     for gap-start = (round (+ measure-start-sample downbeat-location (* beat-duration -0.5)))
     for gap-end = (min (+ gap-start beat-duration) rhythm-length)
     for silence-score = (silence-score (.subseq (time-signal rhythm-to-analyse) (max 0 gap-start) gap-end)
					(time-signal rhythm-to-analyse))
     do
       (setf (.aref downbeat-score downbeat-index) silence-score)
       (format t "Measure ~a beat location ~a silence region (~a ~a) score = ~,3f~%"
	       measure-index downbeat-location gap-start gap-end
	       (.aref downbeat-score downbeat-index))
     finally
       (return 
	 ;; Normalise the downbeat location likelihood, since there is only one location per measure.
	 ;; TODO however, that assumes a direct relationship between silence
	 ;; following and preceding a note and it being a downbeat.
	 (let ((downbeat-probabilities 
		;; When returning P(downbeat) = 0 for all downbeat locations we indicate the lack of
		;; decision as all are equally likely and none contribute to the final decision.
		;; downbeat-score))
		(if (zerop (.sum downbeat-score)) downbeat-score (./ downbeat-score (.sum downbeat-score)))))
	   
	   (format t "Downbeat probabilities ~a~%" downbeat-probabilities)
	   (diag-plot 'gap-evaluation
	     (if (find measure-index *measures-to-plot*)
		 (progn
		   (plot (list (.normalise search-region) downbeat-probabilities downbeat-probabilities)
			 (list (.iseq 0 (1- (.length search-region)))
			       (.* (.iseq 0 (1- subbeats-per-measure)) (round (/ bar-duration subbeats-per-measure)))
			       (.* (.iseq 0 (1- subbeats-per-measure)) (round (/ bar-duration subbeats-per-measure))))
			 :aspect-ratio 0.2
			 :title (format nil "plot of measure ~a" measure-index)
			 :legends '("ODF" "beat gap likelihood")
			 :styles '("lines" "linespoints" "impulses")))))
	   downbeat-probabilities))))
|#

;;; We have a singularly perceptually longer interval (i.e. markedly longer) than other
;;; events in the sequence, in which case the onset starting the first relatively long
;;; (perceptually significant) interval longer than the beat period is likely to be the downbeat event.
;;; TODO perhaps all we need to have passed in is the rhythm, beat-durations-in-measure & calculate the rest.
(defmethod gap-accent-downbeat-evidence ((rhythm-to-analyse rhythm) 
					 measure-start-sample ; begining of our region of interest
					 measure-index
					 beats-per-measure bar-duration beat-durations-in-measure)
  "Returns the probabilities of the downbeat at each beat location. Estimates formed by
when the gap exceeds the beat period. bar-duration and beat-duration in samples"
    (loop
       with look-ahead = 1.25d0		; look ahead a number of beats for a gap.
       with attack-skip = 0.20d0	; skip over the relative attack portion on the beat.
       with subbeats-per-measure = (* beats-per-measure *subdivisions-of-beat*)
       with downbeat-score = (make-double-array subbeats-per-measure) ; initialised to 0.0
       with rhythm-length = (duration-in-samples rhythm-to-analyse)
       with search-region = (.subseq (time-signal rhythm-to-analyse) measure-start-sample 
				     (min rhythm-length (+ measure-start-sample (1- (* bar-duration *bars-of-gap-profile*)))))
       for downbeat-index from 0 below subbeats-per-measure
       for beat-duration = (.aref beat-durations-in-measure (floor downbeat-index *subdivisions-of-beat*))
       ;; look ahead a certain number of beats for a gap.
       for region-length = (round (* beat-duration look-ahead))
       for downbeat-location = (round (* downbeat-index (/ bar-duration subbeats-per-measure)))
       ;; we don't look exactly at the start of the beat, since we assume that will be loud.
       for gap-start = (round (+ measure-start-sample downbeat-location (* beat-duration attack-skip)))
       for gap-end = (min (+ gap-start region-length) rhythm-length)
       for preceding-gap-end = (+ measure-start-sample downbeat-location)
       for preceding-gap-start = (max (- preceding-gap-end region-length) 0)
       for following-silence-score = (silence-score (.subseq (time-signal rhythm-to-analyse) gap-start gap-end) search-region)
       for preceding-silence-score = (if (zerop preceding-gap-start)
					 0.0d0
					 (silence-score (.subseq (time-signal rhythm-to-analyse) preceding-gap-start preceding-gap-end)
							search-region))
       do
	 (setf (.aref downbeat-score downbeat-index) (+ following-silence-score preceding-silence-score))
	 (format t "Measure ~a beat location ~a preceding silence region (~a ~a) following silence region (~a ~a) score = ~,3f~%"
		 measure-index downbeat-location preceding-gap-start preceding-gap-end gap-start gap-end
		 (.aref downbeat-score downbeat-index))
       finally (return 
		 ;; Normalise the downbeat location likelihood, since there is only one location per measure.
		 ;; TODO however, that assumes a direct relationship between silence
		 ;; following and preceding a note and it being a downbeat.
		 (let ((downbeat-probabilities 
			;; When returning P(downbeat) = 0 for all downbeat locations we indicate the lack of
			;; decision as all are equally likely and none contribute to the final decision.
			;; downbeat-score))
			(if (zerop (.sum downbeat-score)) downbeat-score (./ downbeat-score (.sum downbeat-score)))))
		   (format t "Downbeat probabilities ~a~%" downbeat-probabilities)
		   (diag-plot 'gap-evaluation
		     (if (find measure-index *measures-to-plot*)
			 (progn
			   (plot (list (.normalise search-region) downbeat-probabilities downbeat-probabilities)
				 (list (.iseq 0 (1- (.length search-region)))
				       (.* (.iseq 0 (1- subbeats-per-measure)) (round (/ bar-duration subbeats-per-measure)))
				       (.* (.iseq 0 (1- subbeats-per-measure)) (round (/ bar-duration subbeats-per-measure))))
				 :aspect-ratio 0.2
				 :title (format nil "plot of measure ~a" measure-index)
				 :legends '("ODF" "beat gap likelihood")
				 :styles '("lines" "linespoints" "impulses")))))
		   downbeat-probabilities))))


;;; Check when we have intervals which are non-unique, i.e other intervals of similar length reoccur
;;; in the initial sequence.  
(defmethod duration-maxima-downbeat-evidence ((rhythm-to-analyse rhythm) meter
						beats-per-measure tempo-in-bpm bar-duration beat-duration)
  "Returns the probabilities of the downbeat at each beat location. bar-duration and beat-duration in samples"
  (let ((time-limited-iois (rhythm-iois-samples rhythm-to-analyse))
	(downbeat-probabilities (make-double-array beats-per-measure))) ; initialised to 0.0
    (format t "time limited iois ~a, beat-duration ~a~%" time-limited-iois beat-duration)
    (if (plusp (.length time-limited-iois))
	(let* ((categorised-iois (rhythm-categories time-limited-iois (sample-rate rhythm-to-analyse)))
	       (maximum-perceived-interval (.max categorised-iois))
	       (duration-maxima-count (count-occurrence-on-beats rhythm-to-analyse 
								 (.> (./ categorised-iois maximum-perceived-interval) 0.8)
								 "maximum duration"
								 beats-per-measure beat-duration)))
	  (format t "categorised-iois ~a~%" categorised-iois)
	  ;; (diag-plot 'categorised-iois
	  ;;   (if (find measure-index *measures-to-plot*)
	  ;; 	(plot-rhythm rhythm-to-analyse :title (format nil " ~d" measure-index))))
	  ;; TODO Find the earliest maximum IOI
	  (if (plusp (.sum duration-maxima-count))
	      (setf downbeat-probabilities (./ duration-maxima-count (.sum duration-maxima-count))))))
    (format t "downbeat probabilities ~a~%" downbeat-probabilities)
    ;; By returning P(downbeat) = 0 for all downbeat locations we indicate the lack of
    ;; decision as all are equally likely and none contribute to the final decision.
    downbeat-probabilities))


;;; TODO: Examine full phase congruency and phase congruency over a focused range, for
;;; example, weighted by energy and over multiple frequency points of metrical harmonics.
;;; Try sampling the phase where the known downbeats are, & plotting the cross-scale profiles.
(defmethod mrr-phase-downbeat-evidence ((ODF-fragment rhythm)
					meter beats-per-measure tempo-in-bpm
					bar-duration beat-duration)
  "Examine the CWT phase of the fragment"
  (let ((downbeat-probabilities (make-double-array beats-per-measure)))
    (if (plusp (decf *plots-per-rhythm*))
	(let ((analysis (analysis-of-rhythm ODF-fragment :padding #'causal-pad)))
	  (plot-analysis analysis)
	  (window)
	  (plot (phase-congruency analysis) nil 
		:title (format nil "normalised phase congruency of ~a" (name ODF-fragment)) 
		:aspect-ratio 0.2)
	  (close-window)
	  (plot-rhythm ODF-fragment)))
    downbeat-probabilities))

(defun ratio-of-times (durations times)
  "Returns the closest relative location within each beat (ordered list) of each time (an unordered list)"
  ;; (format t "durations ~a times ~a~%" durations times)
  (loop
     ;; Convert the durations to onsets, dropping the last onset, matching the number of durations.
     with onsets = (make-narray (butlast (iois-to-onsets (nlisp::array-to-list durations))))
     for time across (val times)
     for beat-ratio = (loop
			 for onset across (val onsets)
			 for onset-index from 0
			 for duration across (val durations)
			 for difference = (- time onset)
			 ;; since the onsets are ordered, we can exit the loop on first +ve difference.
			 when (and (plusp difference) (<= difference duration))
			 ;; do (format t "time ~a onset ~a duration ~a~%" time onset duration)
			 return (coerce (+ onset-index (/ difference duration)) 'double-float))
     collect beat-ratio into beat-ratios
     finally (return (make-narray beat-ratios))))

(defmethod amplitude-profile-downbeat-evidence ((ODF-fragment rhythm)
						meter beats-per-measure 
						bar-duration beat-durations-in-measure)
  "Estimate the downbeat of the fragment of the onset detection function. Returns a vector
   of length beats-in-measure with the probabilty of the downbeat at each beat indicated."
  ;; metrical profile length in bars
  (let ((tempo-in-bpm (mean (./ 60.0d0 (./ beat-durations-in-measure (sample-rate ODF-fragment)))))
	(metrical-profile-length (1- (floor (duration-in-samples ODF-fragment) bar-duration))))
    (multiple-value-bind (shift-positions shift-scores) 
	(match-ODF-meter meter tempo-in-bpm ODF-fragment metrical-profile-length :maximum-matches *max-amp-matches*) 
      ;; TODO I'm not sure removing these is the best option, perhaps we should fold them.
      (let* ((prune-beyond-1-measure (.find (.>= shift-positions bar-duration)))
	     (shift-positions-in-measure (remove-arefs shift-positions prune-beyond-1-measure))
	     (shift-scores-in-measure (remove-arefs shift-scores prune-beyond-1-measure))
	     (scores (make-double-array (* beats-per-measure *subdivisions-of-beat*)))
      	     ;; Convert shift positions into downbeats.
	     (downbeats (ratio-of-times beat-durations-in-measure shift-positions-in-measure))
	     (subbeat-grid (.floor (.* downbeats *subdivisions-of-beat*)))
	     ;; balance by the relative score values into probabilities.
	     (scores-as-probabilities (if (zerop (.sum shift-scores-in-measure))
					  shift-scores-in-measure
					  (./ shift-scores-in-measure (.sum shift-scores-in-measure)))))
	(setf (.arefs scores subbeat-grid) scores-as-probabilities)
	;; (format t "===============================================~%")
	(format t "shift positions ~a~%downbeats ~a~%downbeat probabilities ~,5f~%"
	 	shift-positions-in-measure downbeats scores-as-probabilities)
	;; (format t "Downbeat expected value ~a vs. combined rounded argmax ~a~%"
	;; 	(.sum (.* (.iseq 0 (1- beats-per-measure)) scores-as-probabilities)) (argmax scores-as-probabilities))
	(diag-plot 'selected-downbeat
	    (if (plusp (decf *plots-per-rhythm*))
		(visualise-downbeat-index meter tempo-in-bpm ODF-fragment (argmax scores-as-probabilities))))
	scores))))

;; TODO Perhaps add these: meter beats-per-measure tempo-in-bpm bar-duration beat-duration
;; as accessors to rhythm? or as a metrical-structure or just meter
;; instance. We are saving the meter and it's relation to tempo. Of course, bar duration
;; is computed from beat-duration and beats-per-measure.

;; (defmethod diagnose-amplitude-profile ((ODF-fragment rhythm) meter beats-per-measure tempo-in-bpm bar-duration beat-duration)
;;   ;; metrical profile length in bars
;;   (let ((metrical-profile-length (1- (/ (duration-in-samples ODF-fragment) bar-duration))))
;;     (format t "Downbeat for measure ~d, sample ~d, shift by ~d samples~%" measure-index 
;; 	    (.aref downbeat-samples measure-index)
;; 	    (- (.aref downbeat-samples measure-index) start-sample))
;;     (window) 
;;     (mrr::visualise-downbeat '(2 2 2) tempo-in-bpm ODF-fragment 
;; 			     (- (.aref downbeat-samples measure-index) start-sample)
;; 			     metrical-profile-length)
;;     (close-window)))

(defun onsets-at-subdivisions (beat-durations-in-measure subdivisions-of-beat)
  "Converts from IOI's for main beats to onset times in samples for tatums"
  (loop
     with beat-onset-sample = 0
     with onsets = (make-integer-array (* (.length beat-durations-in-measure) subdivisions-of-beat))
     with tatum-durations = (./ beat-durations-in-measure (coerce subdivisions-of-beat 'double-float))
     for beat-index from 0 below (.length beat-durations-in-measure)
     do
       (loop
	  for subdiv-index from 0 below subdivisions-of-beat
	  do
	    (setf (.aref onsets (+ (* beat-index subdivisions-of-beat) subdiv-index)) beat-onset-sample)
	    (setf beat-onset-sample (+ beat-onset-sample (.aref tatum-durations beat-index))))
     finally (return onsets))))

(defun observe-downbeat-of (rhythm beat-times meter beats-per-measure downbeat-estimator)
  "Returns an estimate of downbeat location across the entire ODF rhythm using an estimator"
  (loop
     ;; Convert beat-times in seconds to durations of each beat in samples.
     with beat-durations = (.round (.* (.diff beat-times) (sample-rate rhythm)))
     ;; Reduce by one measure to ensure the last search region is a full two bars,
     ;; avoiding a bias in the downbeat-estimator towards starting beats.
     with number-of-measures = (1- (floor (.length beat-durations) beats-per-measure)) ; floor avoids partial bars
     with downbeat-estimates = (make-double-array (list (* beats-per-measure *subdivisions-of-beat*) number-of-measures))
     initially (format t "~%~a~%rhythm duration ~a samples, meter ~a number of measures ~a~%"
		       downbeat-estimator (duration-in-samples rhythm) meter number-of-measures) 
       (format t "First beat-durations ~a~%" (.subseq beat-durations 0 16))
     for measure-index from 0 below number-of-measures
     ;; in samples
     for beat-durations-in-measure = (.subseq beat-durations
					      (* beats-per-measure measure-index) 
					      (* beats-per-measure (1+ measure-index)))
     for bar-duration = (.sum beat-durations-in-measure) ; in samples
     for start-sample = (round (* (- (.aref beat-times (* measure-index beats-per-measure))
				     (.aref beat-times 0))
				  (sample-rate rhythm)))
     ;; collect probabilities of the downbeat occuring at each measure location.
     for downbeat-probabilities =  
       (progn 
	 (format t "Measure ~3d start sample ~a~%" measure-index start-sample)
	 (format t "beat duration in samples ~a = ~a~%" beat-durations-in-measure bar-duration)
	 (funcall downbeat-estimator rhythm start-sample measure-index
		  beat-durations-in-measure *subdivisions-of-beat*))
     do					; collect likelihood in downbeat-estimates
       (format t "Downbeat probabilities ~a~%" downbeat-probabilities)
       ;; (format t "observed at sample ~d~%~%"
       ;;	       (+ start-sample (* (argmax downbeat-probabilities) beat-duration)))
       (diag-plot 'gap-evaluation
       	 (if (find measure-index *measures-to-plot*)
       	     (let ((search-region (.subseq (time-signal rhythm) start-sample 
       					   (min (duration-in-samples rhythm) (+ start-sample (1- (* bar-duration 2)))))))
       	       (plot (list (.normalise search-region) downbeat-probabilities 
			   (.arefs downbeat-probabilities (.* (.iseq 0 (1- beats-per-measure)) *subdivisions-of-beat*)))
       		     (list (.iseq 0 (1- (.length search-region)))
       			   (onsets-at-subdivisions beat-durations-in-measure *subdivisions-of-beat*)
       			   (onsets-at-subdivisions beat-durations-in-measure 1))
		     :aspect-ratio 0.2
		     :title (format nil "plot of measure ~a" measure-index)
		     :legends '("ODF" "beat gap likelihood" "beat location")
		     :styles '("lines" "linespoints" "impulses")))))
       (setf (.column downbeat-estimates measure-index) downbeat-probabilities) 
     finally (return downbeat-estimates)))

(defun amass-evidence (estimates)
  "Returns the row number (downbeat estimate) with most evidence"
  (let ((location-evidence (.partial-sum (.transpose estimates))))
    (format t "amassed downbeat location evidence ~a~%" location-evidence)
    ;; TODO calc how much more likely: (probability-of-estimate location-evidence)
    (argmax location-evidence)))

;;; This uses a Viterbi dynamic path decoder.
(defun decode-evidence (estimates name &key (downbeat-inertia 0.9d0))
  "Returns the row number (downbeat estimate) with most evidence"
  ;; Assume all beats are equally likely. This can be later modified by analysis of a corpus.
  ;; Perhaps there should be a slight bias to the downbeat, depending on the style.
  (let* ((beats-per-measure (.row-count estimates))
	 (initial-probabilities (make-double-array beats-per-measure :initial-element (/ 1.0d0 beats-per-measure)))
	 (transition-probs (make-double-array (list beats-per-measure beats-per-measure)
					      :initial-element (/ (- 1.0d0 downbeat-inertia) (1- beats-per-measure))))
	 state-path)
    ;; Create a downbeat transition matrix matching the number of beats-per-measure.
    (dotimes (state beats-per-measure)
      (setf (.aref transition-probs state state) downbeat-inertia))
    ;;(format t "initial probs ~a~%transition probs ~a~%estimates ~a~%" initial-probabilities transition-probs estimates)
    (setf state-path (viterbi initial-probabilities estimates transition-probs))
    ;; (format t "Viterbi decoded downbeat location evidence ~a~%" state-path)
    (diag-plot 'viterbi-path
      (plot state-path nil :aspect-ratio 0.66 :styles '("linespoints") :title name))
    ;; TODO calc how much more likely: (probability-of-estimate location-evidence)
    ;; Since the Viterbi decoder amasses evidence, the last state is assumed the most likely...
    ;; TODO or should it be the mode of the path values?
    (.last state-path)))

(defun assess-evidence (evidence evidence-name rhythm)
  (format t "~a change in observation over time ~a~%" 
	  evidence-name
	  ;; find argmax for each.
	  (reduce-dimension evidence (lambda (y) (coerce (position (.max y) (val y)) 'double-float))))
  (diag-plot 'downbeat-probabilities
    ;; For plotting with base 1 labelling.
    ;; (image evidence (.iseq 1 (.column-count evidence)) (.iseq 1 (.row-count evidence))
    (image evidence nil nil
	   :title (format nil "~a observations of ~a" evidence-name (name rhythm)) 
	   :xlabel "Time (measures)"
	   :ylabel "Downbeat location (beat)"
	   :aspect-ratio 0.666))
  (let ((viterbi-estimation (decode-evidence evidence (name rhythm))) ; Use a Viterbi decoder to find the best path estimate of the downbeat.
	(argmax-estimation (amass-evidence evidence))) ; Just find the beat with maximum accumulated evidence
    (format t "Decoding (~d) and maximum accumulated evidence (~d) ~s~%" 
	    viterbi-estimation argmax-estimation
	    (if (= viterbi-estimation argmax-estimation) "match" "don't match"))
    (round argmax-estimation *subdivisions-of-beat*)))

(defun downbeat-estimation (rhythm beat-times meter beats-per-measure prior-evidence)
  "Use competing features to form an estimation of downbeat. Returns a single number mod beats-per-measure"
  (let* (;; (amplitude-observations (observe-downbeat-of rhythm 
	 ;;  					      beat-times
	 ;;  					      meter
	 ;;  					      beats-per-measure
	 ;;  					      *bars-of-amplitude-profile*
	 ;;  					      #'amplitude-profile-downbeat-evidence)) 
	 ;; (duration-maxima-observations (observe-downbeat-of rhythm
	 ;; 						    beat-times
	 ;; 						    meter
	 ;; 						    beats-per-measure
	 ;; 						    1
	 ;; 						    #'duration-maxima-downbeat-evidence))
 	 (gap-accent-observations (observe-downbeat-of rhythm
						       beat-times
						       meter
						       beats-per-measure
						       *bars-of-gap-profile*
						       #'gap-accent-downbeat-evidence))
         (combined-observations gap-accent-observations))
	 ;; (combined-observations duration-maxima-observations))
	 ;; (combined-observations amplitude-observations))
	 ;; Combine the observations into a single state vs. time matrix.
	 ;; Use the union of observations, normalised by dividing by the number of observations.
	 ;; (combined-observations (./ (.+ gap-accent-observations duration-maxima-observations) 2.0d0)))
	 ;; (combined-observations (./ (.+ gap-accent-observations duration-maxima-observations amplitude-observations) 3.0d0)))
	 ;(combined-observations (./ (.+ gap-accent-observations amplitude-observations) 2.0d0)))
	 ;; (combined-observations (.transpose (.concatenate (.transpose amplitude-observations)
	 ;;						  (.transpose gap-accent-observations)))))
    ;; (assess-evidence duration-maxima-observations "Duration Maxima observations" rhythm)
    ;; (assess-evidence gap-accent-observations "Gap Accent" rhythm)))
    ;; (assess-evidence amplitude-observations "Amplitude observations" rhythm)
    ;; Since the rounding from assess-evidence can round to the end of the last beat, we
    ;; fold that back to the first using modulo.
    (mod (assess-evidence combined-observations "Combined" rhythm prior-evidence) beats-per-measure)))

(defun downbeat-estimation-amplitude (rhythm beat-times meter beats-per-measure)
  "Use amplitude only to form an estimation of downbeat. Returns a single number mod beats-per-measure"
  (let* ((amplitude-observations (observe-downbeat-of rhythm 
						      beat-times
						      meter
						      beats-per-measure
						      *bars-of-amplitude-profile*
						      #'amplitude-profile-downbeat-evidence)))
    (assess-evidence amplitude-observations "Amplitude profile" rhythm)))

(defun downbeat-estimation-duration (rhythm beat-times meter beats-per-measure)
  "Use duration only to form an estimation of downbeat. Returns a single number mod beats-per-measure"
  (let* ((duration-observations (observe-downbeat-of rhythm
						     beat-times
						     meter
						     beats-per-measure
						     1
						     #'duration-maxima-downbeat-evidence)))
    (assess-evidence duration-observations "Duration maxima" rhythm)))

(defun downbeat-estimation-fixed (rhythm beat-times meter beats-per-measure)
  "The null hypothesis that all downbeats are on the first beat location"
  (declare (ignore rhythm beat-times meter beats-per-measure))
  0)

(defun downbeat-estimation-random (rhythm beat-times meter beats-per-measure)
  "The null hypothesis that all downbeats are randomly one of the beat locations"
  (declare (ignore rhythm beat-times meter))
  (random beats-per-measure))

(defun downbeat-estimation-phase (rhythm beat-times meter beats-per-measure)
  "Use amplitude only to form an estimation of downbeat. Returns a single number mod beats-per-measure"
  (let* ((phase-observations (observe-downbeat-of rhythm 
						      beat-times
						      meter
						      beats-per-measure
						      4
						      #'mrr-phase-downbeat-evidence)))
    (assess-evidence phase-observations "Amplitude profile" rhythm)))


(defun observation-probabilities (rhythm beat-times meter beats-per-measure)
  "Use duration only to return probabilities of downbeat location for each position of gap duration."
  (let* ((gap-observations (observe-downbeat-of rhythm
						beat-times
						meter
						beats-per-measure
						#'silence-evidence))
;;						#'gap-accent-downbeat-evidence))
	 (argmaxes (reduce-dimension gap-observations (lambda (y) (position (.max y) (val y))))) ; argmax fn
	 (percentage-correct (./ (.sum (.= argmaxes 1)) (.length argmaxes))))
    (image gap-observations nil nil
	   :title (format nil "~a observations of ~a" "Gap accent" (name rhythm)) 
	   :xlabel "Time (measures)"
	   :ylabel "Downbeat location (beat)"
	   :aspect-ratio 0.666)
    ;; Normalise the observations to make them probabilities.
    (./ (.partial-sum (.transpose gap-observations)) (.column-count gap-observations))))
;;  (reduce-dimension (.transpose gap-observations) #'nlisp::.mult)))


