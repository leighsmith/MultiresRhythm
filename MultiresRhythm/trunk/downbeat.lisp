;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for finding the initial downbeat of the rhythm.
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

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :multires-rhythm)
(use-package :nlisp)

(defun plot-anacrusis-profiles (rhythm first-notes)
  "Plots the profiles of the first first-notes number of notes in the rhythm"
  ;; retrieve the first set of onset times of the anacrusis.
  (let ((times-in-samples (nlisp::array-to-list (.subarray (onsets-in-samples rhythm)
							   (list 0 (list 0 (1- first-notes))))))
	(scaleogram (scaleogram-of-rhythm rhythm)))
    (format t "times-in-samples ~a~%" times-in-samples)
    (plot-scale-energy-at-times scaleogram times-in-samples :description (name rhythm))))

(defun ridge-ratios-at-time (ridges datum-ridge note-time &key (voices-per-octave 16))
  "Return the ratios of ridges to some datum ridge at each given time point"
  (let* ((datum-time-support (time-support (scale-at-time datum-ridge note-time) voices-per-octave))
	 (ratios-to-datum (mapcar (lambda (ridge-of-note) 
				    (/ (time-support (scale-at-time ridge-of-note note-time) voices-per-octave) datum-time-support))
				  ridges)))
    (format t "datum-time-support ~f~%" datum-time-support)
    (sort ratios-to-datum #'<)))

(defgeneric accent-rhythm (rhythm-to-accent start-onset period &key accent))

(defmethod accent-rhythm ((rhythm-to-accent rhythm) start-onset period &key (accent 1.25d0))
  "Scales certain beats in a rhythm, accenting every period-th onset, starting from
start-onset, these measures are in samples"
  ;; scale down all the other beats
  (let* ((rhythm-sample-length (duration-in-samples rhythm-to-accent))
	 (duration-of-metric-region (- rhythm-sample-length start-onset))
	 (time-signal (time-signal rhythm-to-accent))
	 (onset-times (.floor (.rseq start-onset 
				     (- rhythm-sample-length period)
				     (/ duration-of-metric-region period))))
	 (scaled-notes (.* (.arefs time-signal onset-times) accent))
	 (scaled-time-signal (.* time-signal (/ 1 accent))))
    (setf (.arefs scaled-time-signal onset-times) scaled-notes)
    (setf (time-signal rhythm-to-accent) scaled-time-signal)
    rhythm-to-accent))

(defun long-silence-dyadic-pad (signal)
  "Pad at double the dyadic length"
  (centered-silence-pad signal (dyadic-length (1+ (dyadic-length (.length signal))))))

;; Retrieve the first region of rhythm (or do we have to do the whole thing to get
;; sufficient lower frequency regions?) this would seem problematic for inducing an
;; anacrusis. We should be able to set a maximum time region covering beat or bar period.
(defmethod scaleogram-of-rhythm-silence ((analysis-rhythm rhythm) &key (voices-per-octave 16))
  (format t "Length of Rhythm ~f seconds~%" (duration analysis-rhythm))
  (let ((silence-padded-rhythm (long-silence-dyadic-pad (time-signal analysis-rhythm))))
    (cwt silence-padded-rhythm voices-per-octave)))

;; (setf scaleogram (scaleogram-of-rhythm-silence rhythm))
;; (plot-cwt scaleogram)
;; (multiple-value-setq (skeleton ridge-peaks) (skeleton-of-scaleogram scaleogram (sample-rate rhythm)))
;; (plot-cwt+skeleton-of analysis nil rhythm)
;; (plot-cwt+skeleton-of analysis (subseq (longest-tactus-candidates skeleton) 0 9) rhythm)

;; (setf scaleogram (scaleogram-of-rhythm rhythm))
;; (multiple-value-setq (skeleton ridge-peaks) (skeleton-of-scaleogram scaleogram (sample-rate rhythm)))
;; (plot-cwt+skeleton-of analysis
;; 			 (list (ridge-containing-scale-and-time skeleton 120 0)
;; 			       (ridge-containing-scale-and-time skeleton 122 6104))
;; 			 rhythm)


;; (clap-to-rhythm rhythm :tactus-selector (lambda (skeleton) 
;; 					  (list (ridge-containing-scale-and-time skeleton 120 0)
;; 						(ridge-containing-scale-and-time skeleton 122 6104))))

;; (ridges-containing-scale skeleton 120)
;; (ridges-containing-scale skeleton 122)

;; Little difference between a complete ridge list and the first major ridge alone.
;; (clap-to-rhythm rhythm :tactus-selector (lambda (skeleton) (ridge-containing-scale-and-time skeleton 120 0)))


;;; TODO could weight by the energy density value when creating the persistency profile.
;;; We would do this by ridge-persistency-of using the ridge-peaks rather than
;;; computing a new binary valued set of peaks. This would be more efficient even if we
;;; reverted to using the binary values as we would just use (.> ridge-peaks 0).
(defmethod beat-period-of-rhythm ((rhythm-to-analyse rhythm) (skeleton-to-analyse skeleton))
  "Find the beat level as the highest peak in the ridge persistency profile weighted by absolute tempo."
  (let* ((ridge-persistency-profile (ridge-persistency-of skeleton-to-analyse))
	 (vpo (voices-per-octave skeleton-to-analyse))
	 (sample-rate (sample-rate rhythm-to-analyse))
	 (salient-scale (preferred-tempo-scale vpo sample-rate))
	 (tempo-beat-preference (tempo-salience-weighting-vector salient-scale (.length ridge-persistency-profile)))
 	 (weighted-persistency-profile (.* ridge-persistency-profile tempo-beat-preference))
	 (beat-scale (position (.max weighted-persistency-profile) (val weighted-persistency-profile)))
	 (beat-period (time-support beat-scale vpo)))
    (format t "Computed beat (with tempo weighting) as scale ~a, period ~,3f samples, ~,3f seconds~%" 
	    beat-scale beat-period (/ beat-period sample-rate))
    (diag-plot 'beat-period 
      ;; (plot-command "set xtics (~{~{\"~d\" ~5d~}~^, ~})~%" (label-scale-as-time-support skeleton-to-analyse))
      (plot-command "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
		    (label-scale-as-time-support-seconds skeleton-to-analyse sample-rate))
      (plot (mapcar #'.reverse (list ridge-persistency-profile tempo-beat-preference weighted-persistency-profile))
	    nil
	    :title (format nil "Ridge persistency profile for ~a" (name rhythm-to-analyse))
	    :legends '("Original ridge persistency" "absolute tempo preference profile" "weighted persistency profile") 
	    :aspect-ratio 0.66
	    :reset nil))
    beat-period))

#|
(defmethod beat-period-of-rhythm ((rhythm-to-analyse rhythm) (rhythm-analysis multires-analysis))
  "Find the beat level as the highest peak in the ridge persistency profile weighted by absolute tempo."
  (let* ((ridge-persistency-profile (ridge-persistency-of (skeleton rhythm-analysis)))
	 (vpo (voices-per-octave skeleton-to-analyse))
	 (sample-rate (sample-rate rhythm-to-analyse))
	 (salient-scale (preferred-tempo-scale vpo sample-rate))
	 (tempo-beat-preference (tempo-salience-weighting-vector salient-scale (.length ridge-persistency-profile)))
	 ;; Perhaps use a scale-persistency as a weighting?
	 (scale-persistency-profile (scale-persistency (scaleogram-magnitude (scaleogram rhythm-analysis))))

 	 (weighted-persistency-profile (.* ridge-persistency-profile tempo-beat-preference))
	 (beat-scale (position (.max weighted-persistency-profile) (val weighted-persistency-profile)))
	 (beat-period (time-support beat-scale vpo)))
    (format t "Computed beat (with tempo weighting) as scale ~a, period ~,3f samples, ~,3f seconds~%" 
	    beat-scale beat-period (/ beat-period sample-rate))
    (window)
    ;; (plot-command "set xtics (~{~{\"~d\" ~5d~}~^, ~})~%" (label-scale-as-time-support skeleton-to-analyse))
    (plot-command "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
		  (label-scale-as-time-support-seconds skeleton-to-analyse sample-rate))
    (plot (mapcar #'.reverse (list ridge-persistency-profile tempo-beat-preference weighted-persistency-profile))
	  nil
	  :title (format nil "Ridge persistency profile for ~a" (name rhythm-to-analyse))
	  :legends '("Original ridge persistency" "absolute tempo preference profile" "weighted persistency profile") 
	  :aspect-ratio 0.66
	  :reset nil)
    (close-window)
    beat-period))
|#

#|
(defmethod unweighted-beat-period-of-rhythm ((rhythm-to-analyse rhythm) (skeleton-to-analyse skeleton))
  "Find the beat level as the highest peak in the ridge persistency profile"
  (let* ((ridge-persistency-profile (ridge-persistency-of skeleton-to-analyse))
	 (vpo (voices-per-octave skeleton-to-analyse))
	 (sample-rate (sample-rate rhythm-to-analyse))
	 (beat-scale (position (.max ridge-persistency-profile) (val ridge-persistency-profile)))
	 (beat-period (time-support beat-scale vpo)))
    (format t "Computed beat (without tempo weighting) as scale ~a, period ~,3f samples, ~,3f seconds~%" 
	    beat-scale beat-period (/ beat-period sample-rate))
    (window)
    ;; (plot-command "set xtics (~{~{\"~d\" ~5d~}~^, ~})~%" (label-scale-as-time-support skeleton-to-analyse))
    (plot-command "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
		  (label-scale-as-time-support-seconds skeleton-to-analyse sample-rate))
    (plot (mapcar #'.reverse (list ridge-persistency-profile))
	  nil
	  :title (format nil "Ridge persistency profile for ~a" (name rhythm-to-analyse))
	  :legends '("Original ridge persistency" "absolute tempo preference profile" "weighted persistency profile") 
	  :aspect-ratio 0.66
	  :reset nil)
    (close-window)
    beat-period))
|#

;;; Use the scale persistency to identify the beat.
(defmethod unweighted-beat-period-of-rhythm ((rhythm-to-analyse rhythm) (scaleogram-to-analyse scaleogram))
  "Find the beat level as the highest peak in the ridge persistency profile"
  (let* ((scale-persistency-profile (scale-persistency (scaleogram-magnitude scaleogram-to-analyse)))
	 (vpo (voices-per-octave scaleogram-to-analyse))
	 (sample-rate (sample-rate rhythm-to-analyse))
	 (beat-scale (position (.max scale-persistency-profile) (val scale-persistency-profile)))
	 (beat-period (time-support beat-scale vpo)))
    (format t "Computed beat (without tempo weighting) as scale ~a, period ~,3f samples, ~,3f seconds~%" 
	    beat-scale beat-period (/ beat-period sample-rate))
    (diag-plot 'beat-period
      ;; (plot-command "set xtics (~{~{\"~d\" ~5d~}~^, ~})~%" (label-scale-as-time-support skeleton-to-analyse))
      (plot-command "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
		    (label-scale-as-time-support-seconds scaleogram-to-analyse sample-rate))
      (plot (mapcar #'.reverse (list scale-persistency-profile))
	    nil
	    :title (format nil "Scale persistency profile for ~a" (name rhythm-to-analyse))
	    :legends '("Original scale persistency" "absolute tempo preference profile" "weighted persistency profile") 
	    :aspect-ratio 0.66
	    :reset nil))
    beat-period))

;;(setf prob-distribution (./ weighted-persistency-profile (.sum weighted-persistency-profile)))
;;(setf expected-beat-scale (.sum (.* (.iseq 0 (1- (.length weighted-persistency-profile))) prob-distribution)))

(defun is-greater-rhythmic-period (original comparison &key (number-of-voices-different 2.0)
				   (voices-per-octave 16) ;; TODO this is problematic, needs setting for each run.
				   (minimum-differing-ratio (expt 2 (/ number-of-voices-different voices-per-octave))))
  "Checks if the comparison is greater than the difference in period of number-of-voices-different voices"
  ;; (format t "comparison-ratio ~f minimum-differing-ratio ~f~%" (/ comparison original) minimum-differing-ratio)
  (if (zerop original)
      t
      (> (- (/ comparison original) minimum-differing-ratio) 0.00001)))

;;; We skip until we find truly the first longer interval (not equal to beat period) and
;;; not start clapping until after that interval. So this is a conservative downbeat
;;; finder. The opportune finder uses #'<= and accepts the first occurrence of the current
;;; beat period or longer as the downbeat.  Alternatively if the longer interval is not
;;; within the starting beats, accept the beat period as the next best bet. Starting beats
;;; need to be defined as a multiple of the beat period.  It is possible for
;;; beat-period-of-rhythm to erroneously produce a beat-period which is longer than any
;;; interval in the rhythm. This causes find-downbeat to return nil.
(defmethod find-downbeat ((rhythm-to-analyse rhythm) beat-period &key (strategy #'<))
  "Given the beat period, returns the number of the downbeat, skipping any anacrusis"
  (let* ((iois (rhythm-iois-samples rhythm-to-analyse)) ; Create the time intervals from the rhythm time signal.
	 ;; Starting from the beginning of the piece, look for the first duration longer than the beat duration.
	 (first-downbeat-interval (position (round beat-period) (val iois) :test strategy)))
    ;; (format t "~a~%" strategy)
    (cond (first-downbeat-interval ; if able to find an interval longer than the beat-period
	   first-downbeat-interval) ; The onset starting this longer duration is the downbeat event.
	  (t (format t "Unable to find an interval longer than beat period in IOIs ~a~%" iois)
	     (find-downbeat rhythm-to-analyse (/ beat-period 2d0) :strategy strategy))))) ; use half the beat period.

;; (position (round beat-period) (val iois) :test #'=)) ; use the first beat period.

;;; Probably need to determine downbeat by assessing syncopation, not necessarily first
;;; beat in bar? Downbeats are maximally non-syncopating positions. Syncopation occurs
;;; from a phase change in a periodic rhythm signal. Alternatively, rather than accepting
;;; the first longer period than the beat period, perhaps we need to rate longer periods
;;; as more likely to be the downbeat, i.e if there's a long period, but an even longer
;;; period out of phase, accept the longest period as the more likely.


;;; Alternative of a purely rule based approach over a maximum number of
;;; events: 5 or 6. i.e use a simplified version of LH82 for disambiguating (parsing) the
;;; first short number of beats. Purely assess it's ability to determine the downbeat, not
;;; necessarily the rest of the rhythm. So use intervals together with the maximum length
;;; of the repetition.
(defmethod find-downbeat-simple ((rhythm-to-analyse rhythm) beat-period &key (maximum-window 6))
  "Given the beat period, returns the number of the downbeat, skipping any anacrusis"
  (let* ((iois (rhythm-iois-samples rhythm-to-analyse)) ; Create the time intervals from the rhythm time signal.
	 (initial-iois (.subseq iois 0 maximum-window))
	 (maximum-interval (.max initial-iois))
	 (locations-of-maximum (.find (.= maximum-interval initial-iois)))
	 (first-downbeat-interval (.aref locations-of-maximum 0)))
    first-downbeat-interval)) ; The onset starting this longest duration is the downbeat event.

(defmethod find-downbeat-scores ((rhythm-to-analyse rhythm) beat-period &key (maximum-window 6))
  "Given the beat period, returns the number of the downbeat, skipping any anacrusis"
  (let* ((iois (rhythm-iois-samples rhythm-to-analyse)) ; Create the time intervals from the rhythm time signal.
	 (initial-iois (.subseq iois 0 maximum-window)))
    (position (round beat-period) (val initial-iois) :test #'<=))) ; use the first beat period.

;;; Use 100mSec as minimum tatum.
(defun rhythm-categories (iois sample-rate &key (minimum-tatum-seconds 0.100d0))
  "Maps from intervals into perceptually distinct interval categories"
  (let* ((float-iois (.* iois 1d0))
	 ;; (average-ioi (mean float-iois))
	 ;; Define an absolute tempo threshold using the sample-rate.
	 (tatum-estimate (max (.min float-iois) (* minimum-tatum-seconds sample-rate))))
    ;; (format t "tatum estimate ~a~%" tatum-estimate)
    ;; (format t "average ioi ~a stddev ~a~%" (mean float-iois) (stddev float-iois))
    (.round (./ iois tatum-estimate))))

;; TODO
;; Incorporate an absolute tempo component from London, Cross & Himberg (2006)
;; "Relatively long and/or final elements in a series of tones are perceived as accented (Povel and Ok- 
;; kerman 1981; Jones 1987; Johnson-Laird 1991, Drake and Palmer 1993)." London, Cross &
;; Himberg (2006)

;;; Alternative of a purely rule based approach over a maximum number of
;;; events: 5 or 6. i.e use a simplified version of LH82 for disambiguating (parsing) the
;;; first short number of beats. Purely assess it's ability to determine the downbeat, not
;;; necessarily the rest of the rhythm. So we use intervals together with the maximum length
;;; of the repetition.
;;;
(defmethod find-downbeat-short ((rhythm-to-analyse rhythm) &key 
				(bar-period 400.0 bar-period-supplied) 
				(maximum-window 2.5d0)
				(maximum-events 5))
  "Returns the number of the downbeat, skipping any anacrusis. bar-period in samples"
  (let* ((maximum-window-samples (if bar-period-supplied 
				     bar-period
				     (round (* maximum-window (sample-rate rhythm-to-analyse)))))
	 ;; We use a temporal limit... 
	 (time-limited-iois (rhythm-iois-samples (limit-rhythm rhythm-to-analyse
							       :maximum-samples maximum-window-samples)))
	 ;; ...and an event density (maximum event memory) limit.
	 (initial-iois (.* (.subseq time-limited-iois 0 (1- (min (1+ (.length time-limited-iois)) maximum-events))) 1d0))
	 (perceptual-categories (rhythm-categories initial-iois (sample-rate rhythm-to-analyse)))
	 (maximum-perceived-interval (.max perceptual-categories))
	 (locations-of-maximum (.find (.= maximum-perceived-interval perceptual-categories))))
    (format t "initial iois ~a~%perceptual-categories ~a~%" initial-iois perceptual-categories)
    ;; There are two situations: 1) we have a singularly perceptually longer interval
    ;; (i.e. markedly longer) than other events in the initial sequence, in which case the
    ;; onset starting the first relatively long (perceptually significant) interval is
    ;; highly likely to be the downbeat event, or 2) we have intervals which are non-unique,
    ;; i.e other intervals of similar length reoccur in the initial sequence.
    ;; For now, we only assume the first case, since it's not clear that situation is
    ;; contradicted by the second case.
    ;; TODO We could check if there are several iois that are the same as earlier initial intervals.
    (.aref locations-of-maximum 0)))

;;;;
;;;; Probabilistic model
;;;;

;;; Free parameters.

;;; The number of amplitude profile alignments considered per bar. Defines the contrast
;;; and threshold between highest and lowest probability for all possible downbeat
;;; locations. Must be greater than the number of beats-per-measure. Shouldn't need
;;; adjustment. Could be infinity, it just saves memory.
(defparameter *max-amp-matches* 8)

;; Number of bars used in the matching of the amplitude profile.
(defparameter *bars-of-amplitude-profile* 2)

(defparameter *plots-per-rhythm* 2)

;; Resolution of the beat position estimation states in subdivisions of each beat (4 = semiquavers).
(defparameter *subdivisions-of-beat* 16)

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

;;; We have a singularly perceptually longer interval (i.e. markedly longer) than other
;;; events in the sequence, in which case the onset starting the first relatively long
;;; (perceptually significant) interval longer than the beat period is likely to be the downbeat event.
;;; TODO We could check if there are several IOIs that are the same as earlier initial intervals.
;;; TODO perhaps all we need to have passed in is the rhythm, beat-duration and bar-duration in samples?
(defmethod gap-accent-downbeat-evidence ((rhythm-to-analyse rhythm) meter
					 beats-per-measure bar-duration beat-durations-in-measure)
  "Returns the probabilities of the downbeat at each beat location. Estimates formed by
when the gap exceeds the beat period. bar-duration and beat-duration in samples"
    (loop
       with semiquavers-per-measure = (* beats-per-measure *subdivisions-of-beat*)
       with downbeat-probabilities = (make-double-array semiquavers-per-measure) ; initialised to 0.0
       with rhythm-length = (duration-in-samples rhythm-to-analyse)
       for downbeat-index from 0 below semiquavers-per-measure
       ;; look ahead 1.5 beats for a gap.
       for region-length = (round (* (.aref beat-durations-in-measure (floor downbeat-index *subdivisions-of-beat*)) 1.5))
       for distance-weighting = (.rseq 0 1.0d0 region-length)
       for downbeat-location = (round (* downbeat-index (/ bar-duration semiquavers-per-measure)))
       for gap-end = (min (+ downbeat-location region-length) rhythm-length)
       for silence-evaluation-region = (.subseq (time-signal rhythm-to-analyse) downbeat-location gap-end) 
       for distance-weighted-region = (.* silence-evaluation-region distance-weighting)
       do
	 (setf (.aref downbeat-probabilities downbeat-index)
	       (- 1.0d0 (/ (- (.max distance-weighted-region) (mean distance-weighted-region))
			   (.max distance-weighted-region))))
	 ;;(format t "Silence probability (~a ~a) = ~,3f~%" downbeat-location gap-end 
	 ;;	 (.aref downbeat-probabilities downbeat-index))
	 ;; (if (plusp (decf *plots-per-rhythm*))
	 ;;     (diag-plot 'gap-evaluation 
	 ;;       (plot (.* silence-evaluation-region distance-weighting) nil :aspect-ratio 0.2)))
       ;; By returning P(downbeat) = 0 for all downbeat locations we indicate the lack of
       ;; decision as all are equally likely and none contribute to the final decision.
       finally (diag-plot 'gap-evaluation
		 (if (plusp (decf *plots-per-rhythm*))
		     (plot (list (time-signal rhythm-to-analyse) downbeat-probabilities)
			   (list (.iseq 0 (1- rhythm-length))
				 (.* (.iseq 0 (1- semiquavers-per-measure)) (round (/ rhythm-length semiquavers-per-measure))))
			   :aspect-ratio 0.2
			   :styles '("lines" "linespoints"))))
       finally (return downbeat-probabilities)))

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
	  (diag-plot 'categorised-iois
	    (if (plusp (decf *plots-per-rhythm*))
		(plot-rhythm rhythm-to-analyse :title (format nil " ~d" *plots-per-rhythm*))))
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

#|
(defmethod amplitude-profile-downbeat-evidence ((ODF-fragment rhythm)
						meter beats-per-measure tempo-in-bpm
						bar-duration beat-duration)
  "Estimate the downbeat of the fragment of the onset detection function. Returns a vector
   of length beats-in-measure with the probabilty of the downbeat at each beat indicated."
  ;; metrical profile length in bars
  (let ((metrical-profile-length (1- (floor (duration-in-samples ODF-fragment) bar-duration))))
    (multiple-value-bind (shift-options shift-scores) 
	(match-ODF-meter meter tempo-in-bpm ODF-fragment metrical-profile-length :maximum-matches *max-amp-matches*) 
      (let* ((downbeats (.mod (./ shift-options (coerce beat-duration 'double-float)) beats-per-measure))
	     ;; Convert shift positions into downbeats. Rounds values unevenly -0.25 < x < 0.75
	     ;; since the shift measure is too fine to round up a shift of 0.5 to the next beat.
	     (maximum-scores (maximum-state-values (.mod (.floor (.+ downbeats 0.25d0)) beats-per-measure) 
						   shift-scores beats-per-measure))
	     ;; balance by the relative score values into probabilities.
	     (scores-as-probabilities (if (zerop (.sum maximum-scores))
					  maximum-scores
					  (./ maximum-scores (.sum maximum-scores)))))
	;; (format t "===============================================~%")
	;; (format t "shift options ~a~%downbeats ~a~%downbeat probabilities ~,5f~%"
	;; 	shift-options downbeats scores-as-probabilities)
	;; (format t "Downbeat expected value ~a vs. combined rounded argmax ~a~%"
	;; 	(.sum (.* (.iseq 0 (1- beats-per-measure)) scores-as-probabilities)) (argmax scores-as-probabilities))
	(diag-plot 'selected-downbeat
	    (if (plusp (decf *plots-per-rhythm*))
		(visualise-downbeat-index meter tempo-in-bpm ODF-fragment (argmax scores-as-probabilities))))
	scores-as-probabilities))))
|#

(defmethod amplitude-profile-downbeat-evidence ((ODF-fragment rhythm)
						meter beats-per-measure 
						bar-duration beat-durations-in-measure)
  "Estimate the downbeat of the fragment of the onset detection function. Returns a vector
   of length beats-in-measure with the probabilty of the downbeat at each beat indicated."
  ;; metrical profile length in bars
  (let ((tempo-in-bpm (.mean (./ 60.0d0 beat-durations-in-measure (sample-rate ODF-fragment))))
	(metrical-profile-length (1- (floor (duration-in-samples ODF-fragment) bar-duration))))
    (multiple-value-bind (shift-positions shift-scores) 
	(match-ODF-meter meter tempo-in-bpm ODF-fragment metrical-profile-length :maximum-matches *max-amp-matches*) 
      	     ;; Convert shift positions into downbeats.
      (let* ((scores (make-double-array (* beats-per-measure *subdivisions-of-beat*)))
	     (downbeats (.mod (./ shift-positions (coerce beat-duration 'double-float)) beats-per-measure))
	     (semiquaver-grid (.floor (.* downbeats *subdivisions-of-beat*)))
	     ;; balance by the relative score values into probabilities.
	     (scores-as-probabilities (if (zerop (.sum shift-scores))
					  shift-scores
					  (./ shift-scores (.sum shift-scores)))))
	(setf (.arefs scores semiquaver-grid) scores-as-probabilities)
	;; (format t "===============================================~%")
	(format t "shift positions ~a~%downbeats ~a~%downbeat probabilities ~,5f~%"
	 	shift-positions downbeats scores-as-probabilities)
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

(defun observe-downbeat-of (rhythm beat-times meter beats-per-measure bar-search downbeat-estimator)
  "Returns an estimate of downbeat location across the entire ODF rhythm using an estimator"
  ;; bar-search = Number of bars to search over.
  (setf *plots-per-rhythm* 10) 		; reset the number of plots
  (loop
     ;; Convert beat-times in seconds to durations of each beat in samples.
     with beat-durations = (.round (.* (.diff beat-times) (sample-rate rhythm)))
     ;; Reduce by one measure to ensure the last search region is a full two bars,
     ;; avoiding a bias in the amplitude profile towards starting beats.
     with number-of-measures = (1- (floor (.length beat-durations) beats-per-measure)) ; floor avoids partial bars
     with downbeat-estimates = (make-double-array (list (* beats-per-measure *subdivisions-of-beat*) number-of-measures))
     initially (format t "~%~a~%rhythm duration ~a samples, meter ~a number of measures ~a~%"
		       downbeat-estimator (duration-in-samples rhythm) meter number-of-measures)
     for measure-index from 0 below number-of-measures
     ;; in samples
     for beat-durations-in-measure = (.subseq beat-durations
					      (* beats-per-measure measure-index) 
					      (* beats-per-measure (1+ measure-index)))
     for bar-duration = (.sum beat-durations-in-measure) ; in samples
     and start-sample = 0 then (+ start-sample bar-duration)
     for downbeat-probabilities =  
       (let* ((search-duration (* bar-search bar-duration)) ; in samples TODO not really accurate.
	      (search-region (list start-sample (1- (min (duration-in-samples rhythm) (+ start-sample search-duration)))))
	      (fragment-of-ODF (subset-of-rhythm rhythm search-region)))
	 ;; collect probabilities of the downbeat occuring at each measure location.
	 (format t "beat duration in samples ~a = ~a~%" beat-durations-in-measure bar-duration)
	 (format t "search region ~a search duration ~a~%" search-region search-duration)
	 ;; (format t "observed at sample ~d~%~%"
	 ;;	       (+ start-sample (* (argmax downbeat-probabilities) beat-duration)))
	 (funcall downbeat-estimator fragment-of-ODF meter beats-per-measure
		  bar-duration beat-durations-in-measure))
     do
       (setf (.column downbeat-estimates measure-index) downbeat-probabilities)  ; collect likelihood in downbeat-estimates
     finally (return downbeat-estimates)))

(defun amass-evidence (estimates)
  "Returns the row number (downbeat estimate) with most evidence"
  (let ((location-evidence (.partial-sum (.transpose estimates))))
    (format t "amassed downbeat location evidence ~a~%" location-evidence)
    ;; TODO calc how much more likely: (probability-of-estimate location-evidence)
    (argmax location-evidence)))

;;; This uses a Viterbi dynamic path decoder.
(defun decode-evidence (estimates &key (downbeat-inertia 0.9d0))
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
      (plot state-path nil :aspect-ratio 0.66 :styles '("linespoints")))
    ;; TODO calc how much more likely: (probability-of-estimate location-evidence)
    ;; Since the Viterbi decoder amasses evidence, the last state is assumed the most likely...
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
  (let ((viterbi-estimation (decode-evidence evidence)) ; Use a Viterbi decoder to find the best path estimate of the downbeat.
	(argmax-estimation (amass-evidence evidence))) ; Just find the beat with maximum accumulated evidence
    (format t "Decoding (~d) and maximum accumulated evidence (~d) ~s~%" 
	    viterbi-estimation argmax-estimation
	    (if (= viterbi-estimation argmax-estimation) "match" "don't match"))
    argmax-estimation))

(defun downbeat-estimation (rhythm beat-times meter beats-per-measure)
  "Use competing features to form an estimation of downbeat. Returns a single number mod beats-per-measure"
  (let* (;; (amplitude-observations (observe-downbeat-of rhythm 
	 ;; 					      beat-times
	 ;; 					      meter
	 ;; 					      beats-per-measure
	 ;; 					      *bars-of-amplitude-profile*
	 ;; 					      #'amplitude-profile-downbeat-evidence)) 
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
						       1
						       #'gap-accent-downbeat-evidence))
         (combined-observations gap-accent-observations))
	 ;; (combined-observations duration-maxima-observations))
	 ;; (combined-observations amplitude-observations))
	 ;; Use the union of observations, normalised by dividing by the number of observations.
	 ;; (combined-observations (./ (.+ gap-accent-observations duration-maxima-observations) 2.0d0)))
	 ;; (combined-observations (./ (.+ gap-accent-observations duration-maxima-observations amplitude-observations) 3.0d0)))
	 ;; combine the observations into a single state vs. time matrix.
	 ;; (combined-observations (.transpose (.concatenate (.transpose amplitude-observations)
	 ;;						  (.transpose gap-accent-observations)))))
    ;; (assess-evidence duration-maxima-observations "Duration Maxima observations" rhythm)
    ;; (assess-evidence gap-accent-observations "Gap Accent" rhythm)))
    ;; (assess-evidence amplitude-observations "Amplitude observations" rhythm)
    (assess-evidence combined-observations "Combined" rhythm)))

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
