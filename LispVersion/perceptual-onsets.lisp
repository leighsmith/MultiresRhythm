;;;; -*- Lisp -*-
;;;;
;;;; Functions for reading and processing the perceptual salience signals produced by the
;;;; Plymouth auditory model and equivalently other onset detection functions read as a file.
;;;;
;;;; In nlisp (Matlab-like Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <leigh@leighsmith.com> 
;;;;
;;;; Copyright (c) 2007-2014, All Rights Reserved
;;;;

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :multires-rhythm)
(use-package :nlisp)

(defclass odf-rhythm (rhythm)
  ((odf :initarg :odf :accessor odf :initform (make-double-array '(1)))) ; Onset detection function
  (:documentation "A subclass of rhythm that holds the full salience trace in addition to the onset data."))

;; (setf ps (odf-rhythm "Research/Sources/OtherResearchers/UoP/AuditorySaliencyModel/ines1.saliency" "Research/Sources/OtherResearchers/UoP/AuditorySaliencyModel/ines1.onsets" :weighted t))
;; (setf res1 (perceptual-onsets-to-rhythm "res1/res1_1_resp_text" "res1/res1_1_pOnsets" :weighted t))

(defmethod plot-rhythm ((rhythm-to-plot odf-rhythm) &key 
			(reset t) (time-in-seconds nil) (title ""))
  (if reset (reset-plot))
  (if time-in-seconds
      (plot-command "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
		    (label-samples-as-seconds (duration-in-samples rhythm-to-plot)
					      (sample-rate rhythm-to-plot))))
  (plot (list (.arefs (odf rhythm-to-plot) (onsets-in-samples rhythm-to-plot)) 
	      (odf rhythm-to-plot))
	;; different x axes for each y parameters in the list.
	(list (onsets-in-samples rhythm-to-plot) (.iseq 0 (1- (duration-in-samples rhythm-to-plot))))
	:aspect-ratio 0.2 
        ;; :styles '("impulses linetype 1" "lines linetype 3")
	:styles '("points pointtype 3" "lines linetype 3")
	:legends '("Onsets" "Saliency")
	:reset nil
	:title (format nil "Salience trace and onsets of ~a~a" (name rhythm-to-plot) title)))

;; (plot-rhythm res1)
;; (plot (odf res1) nil :aspect-ratio 0.66)
;; (plot-cwt salience-scaleogram :title "res(1,1) continuous salience")

;; (intervals-in-samples (nlisp::array-to-list (.column perceptual-onsets 0)) :sample-rate 100)
;; (onsets-to-grid (nlisp::array-to-list (.floor (.column perceptual-onsets 0))))

;;; TODO make mixing and clapping outputs conditional on the parameters being supplied.
(defun clap-to-odf-file (odf-path clap-file-path original-sound-path accompaniment-sound-path
			 &key 
			   (sample-rate 200)
			   (start-from-beat 0) 
			   (beat-multiple 1 multiple-supplied-p))
  "Evaluation routine using a supplied ODF file, sample rate and audio file, generates a clap file and a mix file"
  (let* ((odf-rhythm (rhythm-from-odf odf-path :sample-rate sample-rate :description "log-novelty"))
	 ;; Tactus selector options:
	 ;; #'create-weighted-beat-ridge - uses a absolute tempo weighted filter.
	 ;; #'select-probable-beat-ridge - uses Viterbi decoding of likely ridges.
	 ;; #'select-longest-lowest-tactus
	 ;; #'select-longest-highest-tactus
	 ;; Not used:
	 ;; #'create-weighted-windowed-beat-ridge
	 ;; #'create-beat-ridge
	 ;; #'select-tactus-by-beat-multiple
	 ;; #'create-bar-ridge
	 ;; #'create-beat-multiple-ridge
	 ;; (tactus-selector #'select-longest-lowest-tactus)
	 (tactus-selector #'create-weighted-beat-ridge)
	 (clap-times (if multiple-supplied-p
			(clap-to-rhythm odf-rhythm 
					:start-from-beat start-from-beat
					:beat-multiple beat-multiple
					:tactus-selector tactus-selector)
			(clap-to-rhythm odf-rhythm 
					:start-from-beat start-from-beat
					:tactus-selector tactus-selector)))
	 ;; TODO gotta be a better way for division than making it a double-float.
	 (clap-times-in-seconds (./ clap-times (* 1d0 (sample-rate odf-rhythm)))))
    (diag-plot 'odf
      (plot-rhythm odf-rhythm))
    (diag-plot 'onsets ;; Plot the salience trace, the produced onsets, & zero phase.
      (plot-claps odf-rhythm 
		  (.find (onset-time-signal odf-rhythm))
		  ;; empty phase
		  (make-double-array (duration-in-samples odf-rhythm))
		  :signal-name (format nil "perceptual onsets plot of ~a" (name odf-rhythm))))

    ;; TODO Should have a save-rhythm-and-claps method specialised on
    ;; odf-rhythm that uses the weighted onsets rhythm so that we know the
    ;; onsets are as the detector computes them.
    ;; (save-rhythm-and-claps odf-rhythm clap-times)
    ;; (format t "Wrote rhythm as scorefile ~a~%" ) ; TODO
    ;; just write out the claps as a scorefile alone:
    ;; (save-rhythm (make-instance 'rhythm clap-times))
    ;; TODO write out clap times as text file. Bodged up for now.
    (.save clap-times-in-seconds clap-file-path :format :text)

    (format t "Beat times of ~a in seconds:~%~a~%" (name odf-rhythm) clap-times-in-seconds)
    (format t "Tempo estimate: ~,2f BPM~%" (tempo-from-claps clap-times (sample-rate odf-rhythm)))
    ;; Make the perceptual onset detector results audible
    (save-rhythm-mix accompaniment-sound-path original-sound-path clap-times-in-seconds
		     :clap-sample-file #P"/Users/leigh/Research/Data/Handclap Examples/cowbell.aiff")
    (format t "Wrote mix as soundfile ~a~%" accompaniment-sound-path)))

;;; TODO This should be replaced when we integrate Plymouth's onsets thresholding function
;;; into our code.
;; (defmethod onset-time-of-note ((rhythm odf-rhythm) note-numbers)
;;   "Returns the sample number of the beat-number'th beat in the given rhythm"
;;   (let* ((onsets-rhythm (make-instance 'rhythm 
;; 				       :time-signal (onset-time-signal rhythm)
;; 				       :sample-rate (sample-rate rhythm))))
;;     (onset-time-of-note onsets-rhythm note-numbers)))

(defmethod onset-time-of-note ((rhythm-to-analyse odf-rhythm) (note-numbers n-array))
  "Returns the sample number of the beat-number'th beat in the given rhythm"
  (.arefs (.find (onset-time-signal rhythm-to-analyse)) note-numbers))

(defmethod onset-time-of-note ((rhythm-to-analyse odf-rhythm) note-number)
  "Returns the sample number of the beat-number'th beat in the given rhythm"
  (.aref (.find (onset-time-signal rhythm-to-analyse)) note-number))

(defmethod onsets-in-samples ((rhythm-to-analyse odf-rhythm))
  (.find (onset-time-signal rhythm-to-analyse)))

(defun remove-double-onsets (rhythm onset-times &key (filter-window-in-seconds 0.05))
  "Remove all but the highest valued onsets that fall within the filter-window (specified in samples)"
  (let* ((filter-window-in-samples (* filter-window-in-seconds (sample-rate rhythm)))
	 ;; Find IOIs below the threshold.
	 (within-window (.find (.< (.diff onset-times) filter-window-in-samples))))
    (if (zerop (.length within-window))	; Exit out when within-window is empty.
	onset-times
	;; Determine the reliability (amplitude) of the within window onset candidates and the
	;; following candidates.
	(let* ((reliability-within-window (.arefs (odf rhythm) (.arefs onset-times within-window)))
	       (next-reliability-within-window (.arefs (odf rhythm) (.arefs onset-times (.+ within-window 1))))
	       ;; Remove whichever is less. The inequality operator will return a binary increment value.
	       (first-are-greater (.> reliability-within-window next-reliability-within-window))
	       ;; Form the array of indices of onset-times to remove.
	       (to-remove (.+ within-window (.floor first-are-greater))))
	  (remove-arefs onset-times (.remove-duplicates to-remove))))))

(defmethod subset-of-rhythm ((rhythm-to-subset odf-rhythm) time-region)
  "Subset the ODF as well as the onset-time-signal"
  (let ((subset-rhythm (call-next-method rhythm-to-subset time-region)))
    ;; TODO we put the time region inside a list to appease .subarray when working on a vector.
    (setf (odf subset-rhythm) (.subarray (odf rhythm-to-subset) (list time-region)))
    subset-rhythm))

;; Should factor this into a windowing operation using a given function.
(defun windowed-kurtosis (odf-signal &key onset-window-duration (hop 0.5d0))
  "Compute a running (overlapping windows) kurtosis on the signal"
  (loop 
     with kurtosis-values = (make-double-array (list (.length odf-signal)))
     for window-start from 0 below (- (.length odf-signal) onset-window-duration) by (floor (* hop onset-window-duration))
     for window-end = (min (1- (.length odf-signal)) (+ window-start onset-window-duration))
     for windowed-signal = (.subseq odf-signal window-start window-end)
     for kurtosis = (gsl:kurtosis (nlisp::nlisp->gsl windowed-signal))
     do
       ;; (format t "~a ~a~%" window-start window-end))
       (setf (.subarray kurtosis-values (list 0 (list window-start window-end))) 
	     (make-double-array (list (- window-end window-start)) :initial-element kurtosis))
     finally (return kurtosis-values)))

(defun plot-windowed-kurtosis (rhythm)
  (let* ((window-size 25)
	 (wk (windowed-kurtosis (odf rhythm) :onset-window-duration window-size)))
    (plot-command "set yscale -2 2")
    (plot (list (odf rhythm) (.signum wk))
	  nil
	  :reset nil
	  :aspect-ratio 0.2 :styles '("lines" "impulses"))))

;;; Onset detection attempts
#|
(setf onsets (.* (.subseq (mrr::autocorrelation (mrr::odf rhythm))
			  (duration-in-samples rhythm))
		 (mrr::extrema-points-vector (mrr::odf
					      rhythm))))

(setf wk (mrr::windowed-kurtosis (mrr::odf rhythm) :onset-window-duration window-size))
(setf onsets (.* (.< wk 0) (mrr::extrema-points-vector
						     (mrr::odf rhythm))))

(plot (list (mrr::odf rhythm) onsets) nil :aspect-ratio 0.2)
|#

(defun beat-prototype-envelope (odf beat-periods)
  "Given the ODF and the periods of the beat, compute a prototype beat by multiplying"
    (loop
       ;; TODO We could also do this by gradually expanding the window.
       with max-window-length = (.max beat-periods)
       with min-window-length = (.min beat-periods)
       for beat-period across (val beat-periods)
       for beat-window-start = 0 then (+ beat-window-start beat-period)
       for odf-beat-fragment = (.subseq odf beat-window-start (+ beat-window-start beat-period))
       for padded-fragment = (end-pad odf-beat-fragment max-window-length :value 1.0d0)
       for prototype-window = (make-double-array max-window-length :initial-element 1.0d0) then
	 (.* prototype-window padded-fragment)
       finally (return prototype-window)))

(defun windowed-significance (odf-signal &key (threshold) (window-length))
  "Mark those onsets which are significant within a perceptual window"
  (loop
     with windowed-significance = (make-double-array (list (.length odf-signal)))
     for window-start from 0 below (- (.length odf-signal) window-length) by window-length
     for window-end = (+ window-start window-length)
     for windowed-signal = (.subseq odf-signal window-start window-end)
     do ; (format t "~a ~a~%" window-start window-end))
       (setf (.subarray windowed-significance (list 0 (list window-start window-end))) 
	     (significant windowed-signal :threshold threshold))
     finally (return windowed-significance)))

(defmethod onsets-from-odf ((rhythm-for-onsets odf-rhythm))
  "Determine the onset times from the salience trace"
  (let* (;; (a (analysis-of-rhythm rhythm-for-onsets))
	 ;; (npc (phase-congruency a)) ; if we include phase congruency to filter the ODF-RHYTHM.
	 ;; (filtered-odf (.* (normalise (odf rhythm-for-onsets)) npc))
	 (filtered-odf (odf rhythm-for-onsets))
	 (onset-signal (.* (windowed-significance filtered-odf :threshold 0.75
						  :window-length (floor (* 3.0d0 (sample-rate rhythm-for-onsets))))
			   (extrema-points-vector filtered-odf)))
	 (onset-times (.find onset-signal)))
    (diag-plot 'onset-signal 
      (plot (list (normalise filtered-odf) onset-signal) nil :aspect-ratio 0.2))
    (remove-double-onsets rhythm-for-onsets onset-times)))
;; onset-times))

(defun rhythm-from-odf (salience-filepath &key (sample-rate 200.0d0) (description "") (weighted t))
  "Reads the onset detection function (perceptual salience) data and returns a odf-rhythm instance.
    Weighted keyword produces onsets which are weighted by relative values of the saliency measure."
  (let* ((the-odf-matrix (.load salience-filepath :format :text))
	 (the-odf (.column the-odf-matrix 0))
	 (the-odf-rhythm
	  ;; Even though we have assumed rhythm is a set of dirac fns, we can cheat a bit.
	  (make-instance 'odf-rhythm 
			 :name (pathname-name salience-filepath)
			 :description description
			 :odf the-odf
			 :onset-time-signal (make-double-array (.length the-odf))
			 :sample-rate sample-rate))
	 (onset-indices (onsets-from-odf the-odf-rhythm)))
    ;; Assign the onsets from the computed times derived from the salience trace.
    (setf (.arefs (onset-time-signal the-odf-rhythm) onset-indices)
	  (if weighted
	      (.arefs the-odf onset-indices)
	      (make-double-array (.length onset-indices) :initial-element 1d0)))
    the-odf-rhythm))

(defun rhythm-from-ircam-odf (&rest params)
  "IRCAMbeat ODF has 100mS silence appended onto the audio file before calculating the
  ODF, which is then compensated for when calculating the markers. We remove it."
  (let* ((leading-silence-seconds 0.1d0)
	 (raw-odf (apply #'rhythm-from-odf params)))
    (subset-of-rhythm raw-odf (list (round (* leading-silence-seconds (sample-rate raw-odf))) t))))

(defun plymouth-perceptual-salience-rhythm (salience-filepath 
					    onsets-filepath 
					    &key 
					      (sample-rate 200) 
					      (description "")
					      (weighted t))
  "Reads the Plymouth salience data and returns a odf-rhythm instance. 
   Weighted keyword produces onsets which are weighted by relative values of the saliency measure."
  (let* ((perceptual-salience-matrix (.load salience-filepath :format :text))
	 (perceptual-onsets (.load onsets-filepath :format :text))
	 (perceptual-salience (.column perceptual-salience-matrix 0))
	 ;; Assumes onset times are in seconds, converts to samples
	 (onset-times (.floor (.* (.column perceptual-onsets 0) sample-rate)))
	 ;; TODO perhaps rhythm-of-weighted-onsets can be used instead?
	 (onset-time-signal (make-double-array (.length perceptual-salience))))
    (setf (.arefs onset-time-signal onset-times)
	  (if weighted
	      (.column perceptual-onsets 1)
	      (make-double-array (.length onset-times) :initial-element 1.0d0)))
    ;; Even though we have assumed rhythm is a set of dirac fns, we can cheat a bit.
    (make-instance 'odf-rhythm 
		   :name (pathname-name salience-filepath)
		   :description description
		   :odf perceptual-salience
		   :onset-time-signal onset-time-signal
		   :sample-rate sample-rate)))

;;; A specialised version of the scaleogram computation for ODF rhythm signals.
(defmethod scaleogram-of-rhythm ((analysis-rhythm odf-rhythm) &key (voices-per-octave 16)
				 (padding #'mirror-pad))
  "Scaleogram of ODF rhythms should analyse the ODF, rather than onsets determined from that."
  (format t "Length of rhythm \"~a\" is ~f seconds~%" (name analysis-rhythm) (duration analysis-rhythm))
  (cwt (odf analysis-rhythm) voices-per-octave :padding padding))
