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

(defparameter *epsilon* 0.0001d0) ; Used to avoid divide by zero errors.

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
(defparameter *subdivisions-of-beat* 1)

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
     finally (return onsets)))

(defun silence-score-stddev (silence-evaluation-region comparison-region)
  "Values > 1 indicate the variation and mean of the silence evaluation region is less
   than the comparison region, i.e more likely to be silence."
  ;; Calculate the stddev & mean over the region of interest, 2 measures.
  (let* ((stddev-comparison (stddev comparison-region))
	 (mean-comparison (mean comparison-region))
	 ;; Stddev's are just RMS measures of the amplitude envelope. 
	 (stddev-silence-region (stddev silence-evaluation-region))
	 (mean-silence-region (mean silence-evaluation-region))
	 (silence-score (/ (* (/ stddev-comparison (+ stddev-silence-region *epsilon*))
			      (/ mean-comparison (+ mean-silence-region *epsilon*)))
			   (+ mean-silence-region *epsilon*))))
    ;; (format t "comparison stddev ~,3f mean ~,3f~%" stddev-comparison mean-comparison)
    ;; (format t "silence stddev ~,3f mean ~,3f~%" stddev-silence-region mean-silence-region)
    ;; (format t "reciprical mean ~,3f~%" (/ 1.0d0 (+ mean-silence-region *epsilon*)))
    ;; (format t "silence score from intersection of stddev & mean ratios ~,3f~%" silence-score)
    silence-score))

;; (coeff-of-variation (/ stddev-comparison mean-comparison))
;; (cov-silence-region (/ stddev-silence-region mean-silence-region))
;; (setf (.aref downbeat-score downbeat-index) (if (zerop cov-silence-region)
;; 						 4.0d0 ; if we are in true silence
;; 						 (/ coeff-of-variation cov-silence-region)))
;;(format t "cov whole ~,3f cov silence ~,3f ratio ~,3f~%"
;;	    coeff-of-variation cov-silence-region (/ coeff-of-variation cov-silence-region))

(defun silence-score-max (silence-evaluation-region comparison-region)
  "Values > 1 indicate the variation and mean of the silence evaluation region is less
   than the comparison region, i.e more likely to be silence."
  ;; Calculate the stddev & mean over the region of interest, 2 measures.
  (let* ((max-silence-region (.max silence-evaluation-region))
	 (silence-score (/ (.max comparison-region) (+ max-silence-region *epsilon*))))
    ;; (format t "max whole ~,3f max silence ~,3f~%" (.max comparison-region) max-silence-region)
    ;; (format t "silence score from ratio of local and global maxima ~,3f~%" silence-score)
    silence-score))

(defmethod silence-evidence ((rhythm-to-analyse salience-trace-rhythm)
			     measure-start-sample ; beginning of our region of interest
			     measure-index
			     beat-durations-in-measure
			     subdivisions-of-beat)
  "From the rhythm, for one measure, starting at measure-start-sample, determines if each 
beat and subdivision thereof is a silent region"
  (let* ((tatum-locations (onsets-at-subdivisions beat-durations-in-measure subdivisions-of-beat))
	 (tatums-per-measure (.length tatum-locations))
	 (tatum-score (make-double-array tatums-per-measure)) ; initialised to 0.0
	 (tatum-durations (./ beat-durations-in-measure (coerce subdivisions-of-beat 'double-float)))
	 (rhythm-length (duration-in-samples rhythm-to-analyse)))
  (dotimes (tatum-index tatums-per-measure
	    ;; Normalise the downbeat location likelihood, since there is only one location per measure.
	    ;; TODO however, that assumes a direct relationship between silence
	    ;; following and preceding a note and it being a downbeat.
	    ;; When returning P(downbeat) = 0 for all downbeat locations we indicate the lack of
	    ;; decision as all are equally likely and none contribute to the final decision.
	    (./ tatum-score (+ (.sum tatum-score) *epsilon*)))
    (let* ((tatum-location (.aref tatum-locations tatum-index))
	   (tatum-duration (.aref tatum-durations (floor tatum-index subdivisions-of-beat)))
	   (gap-start (round (+ measure-start-sample tatum-location (* tatum-duration -0.5))))
	   (gap-end (min (round (+ gap-start tatum-duration)) rhythm-length))
	   (silence-score (silence-score-stddev (.subseq (odf rhythm-to-analyse) (max 0 gap-start) gap-end)
					 (odf rhythm-to-analyse))))
      (setf (.aref tatum-score tatum-index) silence-score)
      (format t "Measure ~a tatum ~a location ~a samples, silence region (~a ~a) score = ~,3f~%"
	      measure-index tatum-index tatum-location gap-start gap-end silence-score)))))

;;; We have a singularly perceptually longer interval (i.e. markedly longer) than other
;;; events in the sequence, in which case the onset starting the first relatively long
;;; (perceptually significant) interval longer than the beat period is likely to be the downbeat event.
(defmethod gap-accent-downbeat-evidence ((rhythm-to-analyse salience-trace-rhythm) 
					 measure-start-sample ; begining of our region of interest
					 measure-index
					 beat-durations-in-measure
					 subdivisions-of-beat)
  "Returns the probabilities of the downbeat at each beat location. Estimates formed by
when the gap exceeds the beat period. bar-duration and beat-duration in samples"
    (loop
       with gap-position = :both
       with look-ahead = 1.0d0		; look ahead a number of beats for a gap.
       ;; with look-ahead = 1.25d0		; look ahead a number of beats for a gap.
       ;; with attack-skip = 0.20d0	; skip over the relative attack portion on the beat.
       with attack-skip = 0.0d0	; don't skip over the relative attack portion on the beat, avoids accepting snares.
       with tatums-per-measure = (* (.length beat-durations-in-measure) subdivisions-of-beat)
       with tatum-locations = (onsets-at-subdivisions beat-durations-in-measure subdivisions-of-beat)
       with downbeat-score = (make-double-array tatums-per-measure) ; initialised to 0.0
       with rhythm-length = (duration-in-samples rhythm-to-analyse)
       with bar-duration = (.sum beat-durations-in-measure) ; in samples
       with search-region = (.subseq (odf rhythm-to-analyse) measure-start-sample 
				     (min rhythm-length (+ measure-start-sample (1- (* bar-duration *bars-of-gap-profile*)))))
       for tatum-index from 0 below tatums-per-measure
       for tatum-location across (val tatum-locations)
       for beat-duration = (.aref beat-durations-in-measure (floor tatum-index subdivisions-of-beat))
       ;; look ahead a certain number of beats for a gap.
       for lookahead-length = (round (* beat-duration look-ahead))
       ;; we don't look exactly at the start of the beat, since we assume that will be loud.
       for gap-start = (round (+ measure-start-sample tatum-location (* beat-duration attack-skip)))
       for gap-end = (min (+ gap-start lookahead-length) rhythm-length)
       for preceding-gap-end = (round (+ measure-start-sample tatum-location))
       for preceding-gap-start = (max (- preceding-gap-end lookahead-length) 0)
       ;; for following-silence-score = (silence-score-stddev (.subseq (odf rhythm-to-analyse) gap-start gap-end)
	;; 						   (odf rhythm-to-analyse))
       for following-silence-score = (silence-score-stddev (.subseq (odf rhythm-to-analyse) gap-start gap-end) search-region)
       for preceding-silence-score = (if (zerop preceding-gap-start)
					 0.0d0
					 (silence-score-stddev (.subseq (odf rhythm-to-analyse) preceding-gap-start preceding-gap-end)
							search-region))
       do
	 (setf (.aref downbeat-score tatum-index) 	 ;; Gap precedes OR follows the downbeat.
	       (ccase gap-position
		 (:following following-silence-score)
		 (:preceding preceding-silence-score)
		 (:both (+ following-silence-score preceding-silence-score))))
	 (format t "Measure ~a tatum ~a location ~a preceding region (~a ~a) following region (~a ~a) score = ~,3f~%"
		 measure-index tatum-index tatum-location preceding-gap-start preceding-gap-end gap-start gap-end
		 (.aref downbeat-score tatum-index))
       finally (return 
		 ;; Normalise the downbeat location to a likelihood, since there is only one location per measure.
		 ;; TODO however, that assumes a direct relationship between silence
		 ;; following and preceding a note and it being a downbeat.
		 ;; When returning P(downbeat) = 0 for all downbeat locations we indicate the lack of
		 ;; decision as all are equally likely and none contribute to the final decision.
		 ;; downbeat-score)))
		 (./ downbeat-score (+ (.sum downbeat-score) *epsilon*)))))

;;; Check when we have intervals which are non-unique, i.e other intervals of similar length reoccur
;;; in the initial sequence.  
(defmethod duration-maxima-downbeat-evidence ((rhythm-to-analyse salience-trace-rhythm)
					      meter
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
(defmethod mrr-phase-downbeat-evidence ((ODF-fragment salience-trace-rhythm)
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

(defmethod amplitude-profile-downbeat-evidence ((whole-rhythm salience-trace-rhythm)
						measure-start-sample ; begining of our region of interest
						measure-index
						beat-durations-in-measure
						subdivisions-of-beat)
  "Estimate the downbeat of the fragment of the onset detection function. Returns a vector
   of length beats-in-measure with the probabilty of the downbeat at each beat indicated."
  ;; metrical profile length in bars
  (let* ((meter '(2 2 2 2)) ;; TODO
	 (beats-per-measure (.length beat-durations-in-measure))
	 (bar-duration (.sum beat-durations-in-measure))
	 (tempo-in-bpm (mean (./ 60.0d0 (./ beat-durations-in-measure (sample-rate whole-rhythm)))))
	 (odf-fragment (subset-of-rhythm whole-rhythm 
					 (list measure-start-sample (+ measure-start-sample (* bar-duration 2))))))
    (multiple-value-bind (shift-positions shift-scores) 
	(match-ODF-meter meter tempo-in-bpm odf-fragment 2 :maximum-matches *max-amp-matches*)
      ;; TODO I'm not sure removing these shift positions is the best option, perhaps we should fold them.
      (let* ((prune-beyond-1-measure (.find (.>= shift-positions bar-duration)))
	     (shift-positions-in-measure (remove-arefs shift-positions prune-beyond-1-measure))
	     (shift-scores-in-measure (remove-arefs shift-scores prune-beyond-1-measure))
	     (scores (make-double-array (* beats-per-measure subdivisions-of-beat)))
      	     ;; Convert shift positions into downbeats estimates.
	     (downbeats (ratio-of-times beat-durations-in-measure shift-positions-in-measure))
	     (tatum-grid (.floor (.* downbeats subdivisions-of-beat)))
	     ;; balance by the relative score values into probabilities.
	     (scores-as-probabilities (./ shift-scores-in-measure (+ (.sum shift-scores-in-measure) *epsilon*))))
	(setf (.arefs scores tatum-grid) scores-as-probabilities)
	;; (format t "===============================================~%")
	(format t "shift positions ~a~%downbeats ~a~%downbeat probabilities ~,5f~%"
	 	shift-positions-in-measure downbeats scores-as-probabilities)
	;; (format t "Downbeat expected value ~a vs. combined rounded argmax ~a~%"
	;; 	(.sum (.* (.iseq 0 (1- beats-per-measure)) scores-as-probabilities)) (argmax scores-as-probabilities))
	(diag-plot 'selected-downbeat
	    (if (plusp (decf *plots-per-rhythm*))
		(visualise-downbeat-index meter tempo-in-bpm odf-fragment (argmax scores-as-probabilities))))
	scores))))

;; TODO Perhaps add these: meter beats-per-measure tempo-in-bpm bar-duration beat-duration
;; as accessors to rhythm? or as a metrical-structure or just meter
;; instance. We are saving the meter and it's relation to tempo. Of course, bar duration
;; is computed from beat-duration and beats-per-measure.

;; (defmethod diagnose-amplitude-profile ((ODF-fragment salience-trace-rhythm) meter beats-per-measure tempo-in-bpm bar-duration beat-duration)
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

(defun observe-downbeat-of (rhythm beat-times beats-per-measure subdivisions-of-beat downbeat-estimator)
  "Returns an estimate of downbeat location across the entire ODF rhythm using an estimator"
  (loop
     ;; Convert beat-times in seconds to durations of each beat in samples.
     with beat-durations = (.round (.* (.diff beat-times) (sample-rate rhythm)))
     ;; Reduce by one measure to ensure the last search region is a full two bars,
     ;; avoiding a bias in the downbeat-estimator towards starting beats.
     with number-of-measures = (1- (floor (.length beat-durations) beats-per-measure)) ; floor avoids partial bars
     with downbeat-estimates = (make-double-array (list (* beats-per-measure subdivisions-of-beat) number-of-measures))
     initially (format t "~%~a~%rhythm duration ~a samples, number of measures ~a~%"
		       downbeat-estimator (duration-in-samples rhythm) number-of-measures) 
       (format t "First beat-durations ~a~%" (.subseq beat-durations 0 16))
     for measure-index from 0 below number-of-measures
     ;; in samples
     for beat-durations-in-measure = (.subseq beat-durations
					      (* beats-per-measure measure-index) 
					      (* beats-per-measure (1+ measure-index)))
     for bar-duration = (.sum beat-durations-in-measure) ; in samples
     for start-time = (.aref beat-times (* measure-index beats-per-measure))
     for start-sample = (round (* (- start-time (.aref beat-times 0)) (sample-rate rhythm)))
     ;; collect probabilities of the downbeat occuring at each measure location.
     for downbeat-probabilities =  
       (progn 
	 (format t "Measure ~3d start sample ~a seconds ~,3f~%" measure-index start-sample start-time)
	 (format t "beat duration in samples ~a = ~a~%" beat-durations-in-measure bar-duration)
	 (funcall downbeat-estimator rhythm start-sample measure-index
		  beat-durations-in-measure subdivisions-of-beat))
     do					; collect likelihood in downbeat-estimates
       (format t "Downbeat probabilities ~a~%" downbeat-probabilities)
       ;; (format t "observed at sample ~d~%~%"
       ;;	       (+ start-sample (* (argmax downbeat-probabilities) beat-duration)))
       (diag-plot 'downbeat-observations
       	 (if (find measure-index *measures-to-plot*)
       	     (let ((search-region (.subseq (odf rhythm) start-sample 
       					   (min (duration-in-samples rhythm) (+ start-sample (1- (* bar-duration 2)))))))
       	       (plot (list (.normalise search-region) downbeat-probabilities 
			   (.arefs downbeat-probabilities (.* (.iseq 0 (1- beats-per-measure)) subdivisions-of-beat)))
       		     (list (.iseq 0 (1- (.length search-region)))
       			   (onsets-at-subdivisions beat-durations-in-measure subdivisions-of-beat)
       			   (onsets-at-subdivisions beat-durations-in-measure 1))
		     :aspect-ratio 0.2
		     :title (format nil "plot of measure ~a of ~a" measure-index (name rhythm))
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

(defun assess-evidence (evidence evidence-name rhythm prior-evidence)
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

(defun sensor-value (i sensors)
  "Returns the sensor indices for the given index. Effectively a row-major-aref calculation"
  (if sensors
      (let ((span (reduce #'* sensors)))
	(cons (floor i span) (sensor-value (mod i span) (rest sensors))))
      (list i)))

;;; All sensors are assumed to be of the same time dimension.
(defun joint-evidence (&rest sensors)
  "Combines the evidence from the supplied list of sensors into a single megavariable
  whole values are all possible tuples of values of the individual state variables."
  (let* ((sensor-states (mapcar #'.row-count sensors))
	 (state-indices)
	 (number-of-tuples (reduce #'* sensor-states))
	 (joint-evidence (make-double-array (list number-of-tuples (.column-count (first sensors)))
					    :initial-element 1d0)))
    (dotimes (joint-index number-of-tuples joint-evidence)
      (setf state-indices (sensor-value joint-index (rest sensor-states)))
      (dotimes (sensor-index (length sensors))
	;; (format t "joint index ~a pulling from row ~a of sensor ~a~%" 
	;;	joint-index (nth sensor-index state-indices) sensor-index)
	(setf (.row joint-evidence joint-index)
	      (.* (.row joint-evidence joint-index)
		  (.row (nth sensor-index sensors) (nth sensor-index state-indices))))))))

(defmethod downbeat-estimation ((analysed-rhythm rhythm-description) prior-evidence)
  "Use competing features to form an estimation of downbeat. Returns a single number mod beats-per-measure"
  (let* ((rhythm (rhythm analysed-rhythm))
	 (beat-times (beat-times (meter analysed-rhythm)))
	 (beats-per-measure (beats-per-measure (meter analysed-rhythm)))
	 (amplitude-observations (observe-downbeat-of rhythm 
	   					      beat-times
	   					      beats-per-measure
	   					      *subdivisions-of-beat*
	   					      #'amplitude-profile-downbeat-evidence)) 
	 ;; (duration-maxima-observations (observe-downbeat-of rhythm
	 ;; 						    beat-times
	 ;; 						    beats-per-measure
	 ;; 						    *subdivisions-of-beat*
	 ;; 						    #'duration-maxima-downbeat-evidence))
 	 (gap-accent-observations (observe-downbeat-of rhythm
						       beat-times
						       beats-per-measure
						       *subdivisions-of-beat*
						       #'gap-accent-downbeat-evidence))
	 ;; Combine the observations into a single state vs. time matrix.
	 (combined-observations (joint-evidence gap-accent-observations amplitude-observations)))
         ;; (combined-observations gap-accent-observations))
	 ;; (combined-observations duration-maxima-observations))
	 ;; (combined-observations amplitude-observations))
	 ;; Use the union of observations, normalised by dividing by the number of observations.
	 ;; (combined-observations (./ (.+ gap-accent-observations duration-maxima-observations) 2.0d0)))
	 ;; (combined-observations (./ (.+ gap-accent-observations duration-maxima-observations amplitude-observations) 3.0d0)))
	 ;(combined-observations (./ (.+ gap-accent-observations amplitude-observations) 2.0d0)))
	 ;; (combined-observations (.transpose (.concatenate (.transpose amplitude-observations)
	 ;;						  (.transpose gap-accent-observations)))))
    (diag-plot 'downbeat-observations
      (image amplitude-observations nil nil
	     :title (format nil "~a observations of ~a" "Amplitude" (name (rhythm analysed-rhythm)))
	     :xlabel "Time (measures)"
	     :ylabel "Downbeat location (beat)"
	     :aspect-ratio 0.666))
    ;; (assess-evidence duration-maxima-observations "Duration Maxima observations" rhythm)
    ;; (assess-evidence gap-accent-observations "Gap Accent" rhythm)))
    ;; (assess-evidence amplitude-observations "Amplitude observations" rhythm)
    ;; Since the rounding from assess-evidence can round to the end of the last beat, we
    ;; fold that back to the first using modulo.
    (assess-evidence combined-observations "Combined" rhythm prior-evidence)))

(defmethod downbeat-estimation-amplitude ((analysed-rhythm rhythm-description))
  "Use amplitude only to form an estimation of downbeat. Returns a single number mod beats-per-measure"
  (let* ((amplitude-observations (observe-downbeat-of (rhythm analysed-rhythm)
						      (beat-times (meter analysed-rhythm))
						      (beats-per-measure (meter analysed-rhythm))
						      *subdivisions-of-beat*
						      #'amplitude-profile-downbeat-evidence)))
    (assess-evidence amplitude-observations "Amplitude profile" (rhythm analysed-rhythm))))

(defmethod downbeat-estimation-fixed ((analysed-rhythm rhythm-description))
  "The null hypothesis that all downbeats are on the first beat location"
  (declare (ignore analysed-rhythm))
  0)

(defmethod downbeat-estimation-random ((analysed-rhythm rhythm-description))
  "The null hypothesis that all downbeats are randomly one of the beat locations"
  (random (beats-per-measure (meter analysed-rhythm))))

(defmethod downbeat-estimation-phase ((analysed-rhythm rhythm-description))
  "Use amplitude only to form an estimation of downbeat. Returns a single number mod beats-per-measure"
  (let* ((phase-observations (observe-downbeat-of (rhythm analysed-rhythm)
						  (beat-times (meter analysed-rhythm))
						  (beats-per-measure (meter analysed-rhythm))
						  *subdivisions-of-beat*
						  #'mrr-phase-downbeat-evidence)))
    (assess-evidence phase-observations "Amplitude profile" (rhythm analysed-rhythm))))

(defmethod observation-probabilities ((analysed-rhythm rhythm-description))
  "Use duration only to return probabilities of downbeat location for each position of gap duration."
  (let* ((gap-observations (observe-downbeat-of (rhythm analysed-rhythm)
						(beat-times (meter analysed-rhythm))
						(beats-per-measure (meter analysed-rhythm))
						*subdivisions-of-beat*
						#'gap-accent-downbeat-evidence))
	 (argmaxes (reduce-dimension gap-observations (lambda (y) (position (.max y) (val y))))) ; argmax fn
	 (percentage-correct (./ (.sum (.= argmaxes 1)) (.length argmaxes))))
    (image gap-observations nil nil
	   :title (format nil "~a observations of ~a" "Gap accent" (name (rhythm analysed-rhythm)))
	   :xlabel "Time (measures)"
	   :ylabel "Downbeat location (beat)"
	   :aspect-ratio 0.666)
    ;; Normalise the observations to make them probabilities.
    (./ (.partial-sum (.transpose gap-observations)) (.column-count gap-observations))))
;;  (reduce-dimension (.transpose gap-observations) #'nlisp::.mult)))

#|
(setf preceding-gaps (mrr::observe-downbeat-of (rhythm u2) 
					       (mrr::beat-times (mrr::meter u2))
					       (mrr::beats-per-measure (mrr::meter u2))
					       #'mrr::gap-accent-downbeat-evidence))
|#
