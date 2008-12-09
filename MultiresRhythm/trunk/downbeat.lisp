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
      (nplot (mapcar #'.reverse (list ridge-persistency-profile tempo-beat-preference weighted-persistency-profile))
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
    (nplot (mapcar #'.reverse (list ridge-persistency-profile tempo-beat-preference weighted-persistency-profile))
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
    (nplot (mapcar #'.reverse (list ridge-persistency-profile))
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
      (nplot (mapcar #'.reverse (list scale-persistency-profile))
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
;;; finder. The opportune finder uses #'<= and accepts the first occurance of the current
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

(defun rhythm-categories (iois sample-rate)
  "Maps from intervals into perceptually distinct interval categories"
  (declare (ignore sample-rate)) ; TODO should add absolute tempo preference using the sample-rate.
  (let* ((float-iois (.* iois 1d0))
	 ;; (average-ioi (mean float-iois))
	 (tatum-estimate (.min float-iois)))
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

;;; Since the likelihoods are ordered, simply comparing the first and second likelihoods is
;;; sufficient to generalise a measure of likelihood into binary classes (low/high).
;;; Incorporating the average of the likelihoods indicates how much the first rises above the remainder.
(defun probability-of-estimate (match-scores)
  "Return a normalised single probability based on the contrast of the match-score (unnormalised likelihood) of the chosen value against the remaining values."
  (/ (- (.aref match-scores 0) (mean (.subseq match-scores 1))) (.aref match-scores 0)))

;;(defun downbeat-from-position (shift-options beat-duration)
;;  (round (.aref shift-options 0) beat-duration))

;;; There are two situations: 1) we have a singularly perceptually longer interval
;;; (i.e. markedly longer) than other events in the initial sequence, in which case the
;;; onset starting the first relatively long (perceptually significant) interval is
;;; highly likely to be the downbeat event, or 2) we have intervals which are non-unique,
;;; i.e other intervals of similar length reoccur in the initial sequence.
;;; For now, we only assume the first case, since it's not clear that situation is
;;; contradicted by the second case.
;;; TODO We could check if there are several iois that are the same as earlier initial intervals.
;;; TODO perhaps all we need to have passed in is the rhythm, beat-duration and bar-duration in samples?
(defmethod duration-downbeat-estimation ((rhythm-to-analyse rhythm) meter
					 beats-per-measure tempo-in-bpm bar-duration beat-duration)
  "Returns the number of the downbeat, skipping any anacrusis. bar-duration and beat-duration in samples"
  (let (;; We use a temporal limit... Perhaps we can just pass this amount in?
	(time-limited-iois (rhythm-iois-samples (limit-rhythm rhythm-to-analyse
							      :maximum-samples bar-duration))))
    (if (plusp (.length time-limited-iois))
	(let* ((perceptual-categories (rhythm-categories (.concatenate (make-narray (list beat-duration)) time-limited-iois) (sample-rate rhythm-to-analyse)))
	       (categorised-beat (.aref perceptual-categories 0))
	       (categorised-iois (.subseq perceptual-categories 1))
	       (maximum-perceived-interval (.max categorised-iois))
	       (locations-of-maximum (.find (.and (.= maximum-perceived-interval categorised-iois)
						  (.> categorised-iois categorised-beat)))))
	     (format t "time limited iois ~a, beat-duration ~a~%" time-limited-iois beat-duration)
	     (format t "categorised-iois ~a, categorised beat ~a~%" categorised-iois categorised-beat)
	     (if (plusp (.length locations-of-maximum))
		 ;; Find the earliest maximum IOI
		 (let ((downbeat (round (onset-time-of-note rhythm-to-analyse (.aref locations-of-maximum 0)) beat-duration)))
		   (format t "locations of max duration ~a downbeat ~d probability ~,5f~%"
			   locations-of-maximum downbeat 0.0)
		   downbeat)
		 0))
	;; We should mark this as being unknown, rather than just
	;; defaulting. Prob. return -1? or probability 0 which doesn't really mean we don't know...
	0)))

#|
Overblown, & doesn't always return 4 values.
;; Make a histogram of the labels, adding the probabilities
(defun combine-probabilities (discrete-labels probabilities)
  "Combines any duplicated labels into a single value, adding the probabilities"
  (let ((label-hash (make-hash-table :size (.length discrete-labels))))
    (map nil (lambda (element probability) (incf (gethash element label-hash 0) probability)) 
	 (val discrete-labels) (val probabilities))
    (make-narray (get-histogram-counts label-hash))))
|#

;; Make a histogram of the labels, adding the probabilities
(defun combine-probabilities (discrete-labels probabilities total-labels)
  "Combines any duplicated labels into a single value, adding the probabilities"
  (loop
     with label-tally = (make-double-array total-labels)
     for label across (val discrete-labels)
     for probability across (val probabilities)
     do (incf (.aref label-tally label) probability)
     finally (return label-tally)))

(defmethod amplitude-profile-downbeat-estimation ((ODF-fragment rhythm)
						  meter beats-per-measure tempo-in-bpm bar-duration beat-duration)
  "Estimate the downbeat of the fragment of the onset detection function. Returns a vector
   of length beats-in-measure with the probabilty of the downbeat at each beat indicated."
  (declare (ignore bar-duration))
  (multiple-value-bind (shift-options shift-scores) (match-ODF-meter meter tempo-in-bpm ODF-fragment)  
    (let* ((downbeats (.mod (.round (./ shift-options beat-duration)) beats-per-measure)) ; Super dumb for now.
	   ;; balance by the relative score values.
	   (scores-as-probabilities (./ shift-scores (.sum shift-scores)))) 
      (format t "shift options ~a downbeat probabilities ~,5f~%"
	      shift-options (combine-probabilities downbeats scores-as-probabilities beats-per-measure))
      (combine-probabilities downbeats scores-as-probabilities beats-per-measure))))

;; TODO could pass in the search duration in number of bars.
(defun downbeat-estimate-rhythm (rhythm tempo-in-bpm meter beats-per-measure downbeat-estimator)
  "Returns an estimate of downbeat location across the entire ODF rhythm using an estimator"
  (loop
     with beat-duration = (round (* (/ 60.0 tempo-in-bpm) (sample-rate rhythm))) ; in samples
     with bar-duration = (* beats-per-measure beat-duration) ; in samples
     with number-of-measures = (floor (duration-in-samples rhythm) bar-duration) ; floor avoids partial bars
     with downbeat-estimates = (make-double-array (list beats-per-measure number-of-measures))
     with search-duration = (* 2 bar-duration) ; in samples
     initially (format t "beat duration in samples ~a, meter ~a~%" beat-duration meter)
     for measure-index from 0 below number-of-measures
     for start-sample from 0 below (duration-in-samples rhythm) by bar-duration
     for search-region = (list start-sample (1- (min (duration-in-samples rhythm) (+ start-sample search-duration))))
     for fragment-of-ODF = (subset-of-rhythm rhythm search-region)
     for downbeat-probabilities = (funcall downbeat-estimator fragment-of-ODF meter beats-per-measure
					   tempo-in-bpm bar-duration beat-duration)
     do (format t "tempo ~,2f bpm, search duration ~a search region ~a~%"
		tempo-in-bpm search-duration search-region)	
       (setf (.column downbeat-estimates measure-index) downbeat-probabilities) ; collect likelihood in downbeat-probability
     finally (return downbeat-estimates)))

(defun downbeat-estimation (rhythm tempo-in-bpm meter beats-per-measure)
  "Use competing features to form an estimation of downbeat. Returns a single number mod beats-per-measure"
  (let ((amplitude-estimates (downbeat-estimate-rhythm rhythm 
						       tempo-in-bpm
						       meter
						       beats-per-measure
						       #'amplitude-profile-downbeat-estimation))
;;; 	(duration-estimates (downbeat-estimate-rhythm rhythm
;;; 						      tempo-in-bpm
;;; 						      meter
;;; 						      beats-per-measure
;;; 						      #'duration-downbeat-estimation)))
)
    (format t "amplitude estimates ~a~%" amplitude-estimates)
 ;;   (format t "duration estimates ~a~%" duration-estimates)
    (format t "partial-sum ~a~%" (.partial-sum (.transpose amplitude-estimates)))
    (argmax (.partial-sum (.transpose amplitude-estimates)))))
