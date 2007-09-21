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

;; To get anthems with an anacrusis:
;; (remove-if #'zerop *national-anthems* :key #'anthem-start-at)
;; How many rhythms have an anacrusis?
;; (/ (length (remove-if #'zerop *national-anthems* :key #'anthem-start-at)) (length *national-anthems*)))

(defun plot-anacrusis-profiles (rhythm first-notes)
  "Plots the profiles of the first first-notes number of notes in the rhythm"
  ;; retrieve the first set of onset times of the anacrusis.
  (let ((times-in-samples (nlisp::array-to-list (.subarray (onsets-in-samples rhythm)
							   (list 0 (list 0 (1- first-notes))))))
	(scaleogram (scaleogram-of-rhythm rhythm)))
    (format t "times-in-samples ~a~%" times-in-samples)
    (plot-scale-energy-at-times scaleogram times-in-samples :description (name rhythm))))

;; (plot-anacrusis-profiles (anthem-rhythm (anthem-named 'netherlands)) (downbeat-number (anthem-named 'netherlands))

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

(defun long-silence-dyadic-pad (signal &key (silence-pad t))
  "Pad at double the dyadic length"
  (pad-signal signal (dyadic-length (1+ (dyadic-length (.length signal)))) :silence-pad silence-pad))

;; Retrieve the first region of rhythm (or do we have to do the whole thing to get
;; sufficient lower frequency regions?) this would seem problematic for inducing an
;; anacrusis. We should be able to set a maximum time region covering beat or bar period.
(defmethod scaleogram-of-rhythm-silence ((analysis-rhythm rhythm) &key (voices-per-octave 16))
  (format t "Length of Rhythm ~f seconds~%" (duration analysis-rhythm))
  (let ((silence-padded-rhythm (long-silence-dyadic-pad (time-signal analysis-rhythm) :silence-pad t)))
    (cwt silence-padded-rhythm voices-per-octave)))

;; (setf rhythm (anthem-rhythm (anthem-named 'america)))
;; (setf rhythm (anthem-rhythm (anthem-named 'australia)))
;; (setf scaleogram (scaleogram-of-rhythm-silence rhythm))
;; (plot-cwt scaleogram)
;; (multiple-value-setq (skeleton ridge-peaks) (skeleton-of-scaleogram scaleogram (sample-rate rhythm)))
;; (plot-highlighted-ridges scaleogram nil ridge-peaks :sample-rate (sample-rate rhythm))
;; (plot-highlighted-ridges scaleogram (subseq (longest-tactus-candidates skeleton) 0 9) ridge-peaks :sample-rate (sample-rate rhythm))

;; (setf scaleogram (scaleogram-of-rhythm rhythm))
;; (multiple-value-setq (skeleton ridge-peaks) (skeleton-of-scaleogram scaleogram (sample-rate rhythm)))
;; (plot-highlighted-ridges scaleogram 
;; 			 (list (ridge-containing-scale-and-time skeleton 120 0)
;; 			       (ridge-containing-scale-and-time skeleton 122 6104))
;; 			 ridge-peaks 
;; 			 :sample-rate (sample-rate rhythm))


;; (clap-to-rhythm rhythm :tactus-selector (lambda (skeleton) 
;; 					  (list (ridge-containing-scale-and-time skeleton 120 0)
;; 						(ridge-containing-scale-and-time skeleton 122 6104))))

;; (ridges-containing-scale skeleton 120)
;; (ridges-containing-scale skeleton 122)

;; Little difference between a complete ridge list and the first major ridge alone.
;; (clap-to-rhythm rhythm :tactus-selector (lambda (skeleton) (ridge-containing-scale-and-time skeleton 120 0)))


;;; TODO could weight by the energy density value when creating the persistency profile.
;;; We would do this by ridge-persistency-of-skeleton using the ridge-peaks rather than
;;; computing a new binary valued set of peaks. This would be more efficient even if we
;;; reverted to using the binary values as we would just use (.> ridge-peaks 0).
(defmethod beat-period-of-rhythm ((rhythm-to-analyse rhythm) (skeleton-to-analyse skeleton))
  "Find the beat level as the highest peak in the ridge persistency profile weighted by absolute tempo."
  (let* ((ridge-persistency-profile (ridge-persistency-of-skeleton skeleton-to-analyse))
	 (vpo (voices-per-octave skeleton-to-analyse))
	 (sample-rate (sample-rate rhythm-to-analyse))
	 (salient-scale (preferred-tempo-scale vpo sample-rate))
	 (tempo-beat-preference (.column (tempo-salience-weighting salient-scale 
								   (list (.length ridge-persistency-profile) 1)) 0))
 	 (weighted-persistency-profile (.* ridge-persistency-profile tempo-beat-preference))
	 (beat-scale (position (.max weighted-persistency-profile) (val weighted-persistency-profile)))
	 (beat-period (time-support beat-scale vpo)))
    (format t "Computed beat (with tempo weighting) as scale ~a, period ~a samples, ~a seconds~%" 
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

#|
(defmethod beat-period-of-rhythm ((rhythm-to-analyse rhythm) (rhythm-analysis multires-analysis))
  "Find the beat level as the highest peak in the ridge persistency profile weighted by absolute tempo."
  (let* ((ridge-persistency-profile (ridge-persistency-of-skeleton (skeleton rhythm-analysis)))
	 (vpo (voices-per-octave skeleton-to-analyse))
	 (sample-rate (sample-rate rhythm-to-analyse))
	 (salient-scale (preferred-tempo-scale vpo sample-rate))
	 (tempo-beat-preference (.column (tempo-salience-weighting salient-scale 
								   (list (.length ridge-persistency-profile) 1)) 0))
	 ;; Perhaps use a scale-persistency as a weighting?
	 (scale-persistency-profile (scale-persistency (scaleogram-magnitude (scaleogram rhythm-analysis))))

 	 (weighted-persistency-profile (.* ridge-persistency-profile tempo-beat-preference))
	 (beat-scale (position (.max weighted-persistency-profile) (val weighted-persistency-profile)))
	 (beat-period (time-support beat-scale vpo)))
    (format t "Computed beat (with tempo weighting) as scale ~a, period ~a samples, ~a seconds~%" 
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
  (let* ((ridge-persistency-profile (ridge-persistency-of-skeleton skeleton-to-analyse))
	 (vpo (voices-per-octave skeleton-to-analyse))
	 (sample-rate (sample-rate rhythm-to-analyse))
	 (beat-scale (position (.max ridge-persistency-profile) (val ridge-persistency-profile)))
	 (beat-period (time-support beat-scale vpo)))
    (format t "Computed beat (without tempo weighting) as scale ~a, period ~a samples, ~a seconds~%" 
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
    (format t "Computed beat (without tempo weighting) as scale ~a, period ~a samples, ~a seconds~%" 
	    beat-scale beat-period (/ beat-period sample-rate))
    (window)
    ;; (plot-command "set xtics (~{~{\"~d\" ~5d~}~^, ~})~%" (label-scale-as-time-support skeleton-to-analyse))
    (plot-command "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
		  (label-scale-as-time-support-seconds scaleogram-to-analyse sample-rate))
    (nplot (mapcar #'.reverse (list scale-persistency-profile))
	   nil
	   :title (format nil "Scale persistency profile for ~a" (name rhythm-to-analyse))
	   :legends '("Original scale persistency" "absolute tempo preference profile" "weighted persistency profile") 
	   :aspect-ratio 0.66
	   :reset nil)
    (close-window)
    beat-period))

;;(setf prob-distribution (./ weighted-persistency-profile (.sum weighted-persistency-profile)))
;;(setf expected-beat-scale (.sum (.* (.iseq 0 (1- (.length weighted-persistency-profile))) prob-distribution)))

(defun is-greater-rhythmic-period (original comparison &key (number-of-voices-different 2.0)
				   (voices-per-octave 16) ;; TODO this is problematic, needs setting for each run.
				   (minimum-differing-ratio (expt 2 (/ number-of-voices-different voices-per-octave))))
  "Checks if the comparison is greater than the difference in period of number-of-voices-different voices"
  ;; (format t "comparison-ratio ~f minimum-differing-ratio ~f~%" (/ comparison original) minimum-differing-ratio)
  (> (- (/ comparison original) minimum-differing-ratio) 0.00001))

;;; We skip until we find truly the first longer interval (not equal to beat period) and
;;; then count back from there, or more logically, not start clapping until after that
;;; beat. So this is a conservative downbeat finder. The opportune finder uses #'<= and
;;; accepts the first occurance of the current beat period or longer as the downbeat.
;;; Alternatively if the longer interval is not within the starting beats, accept the beat
;;; period as the next best bet. Starting beats need to be defined as a multiple of the
;;; beat period.
;;; It is possible for  beat-period-of-rhythm to erroneously produce a beat-period which
;;; is longer than any interval in the rhythm. This causes find-downbeat to return nil.
(defmethod find-downbeat ((rhythm-to-analyse rhythm) beat-period &key (strategy #'<))
  "Given the beat period, returns the number of the downbeat, skipping any anacrusis"
  (let* ((iois (rhythm-iois-samples rhythm-to-analyse)) ; Create the time intervals from the rhythm time signal.
	 ;; Starting from the beginning of the piece, look for the first duration longer than the beat duration.
	 (first-downbeat-interval (position (round beat-period) (val iois) :test strategy)))
    ;; (format t "~a~%" strategy)
    (if (not first-downbeat-interval)	; if unable to find an interval longer than the beat-period
	(progn (format t "Unable to find an interval longer than beat period in IOIs ~a~%" iois)
	       (find-downbeat rhythm-to-analyse (/ beat-period 2d0) :strategy strategy)) ; use half the beat period.
;;	       (position (round beat-period) (val iois) :test #'=)) ; use the first beat period.
	;; The beat starting this longer duration is the downbeat.
	first-downbeat-interval)))

;;; Probably need to determine downbeat by assessing syncopation, not necessarily first
;;; beat in bar? Downbeats are maximally non-syncopating positions. Syncopation occurs
;;; from a phase change in a periodic rhythm signal. Alternatively, rather than accepting
;;; the first longer period than the beat period, perhaps we need to rate longer periods
;;; as more likely to be the downbeat, i.e if there's a long period, but an even longer
;;; period out of phase, accept the longest period as the more likely.


