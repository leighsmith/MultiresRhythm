;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for reading and processing the perceptual salience signals produced by the
;;;; Plymouth model.
;;;;
;;;; In nlisp (Matlab-like Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2007
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

;; We have a subclass of rhythm that holds the full salience trace and the onsets.
(defclass salience-trace-rhythm (rhythm)
  ((onsets-time-signal :initarg :onsets-time-signal :accessor onsets-time-signal :initform (make-double-array '(1)))))

#|
(defun perceptual-onsets-to-rhythm (filename &key (sample-rate 200) (description "") (weighted t))
  (let* ((file-path (make-pathname 
		     :directory (list :absolute "/Volumes/iDisk/Research/Data/PerceptualOnsets/") 
		     :name filename
		     :type "mat"))
	 (perceptual-onsets (.load-octave-file file-path))
	 ;; Assumes onset times are in milliseconds
	 (onset-times (.floor (.* (.column perceptual-onsets 0) sample-rate)))
	 (binary-grid (make-narray (onsets-to-grid (nlisp::array-to-list onset-times))))
	 (weighted-grid (.* binary-grid 1d0))
	 (rhythm-name (concatenate 'string (if weighted "weighted " "unweighted ") filename)))
    (setf (.arefs weighted-grid onset-times) (.column perceptual-onsets 1))
    (make-instance 'rhythm 
		   :name rhythm-name
		   :description description
		   :time-signal (if weighted weighted-grid binary-grid)
		   :sample-rate sample-rate)))
|#

;; (setf res1 (perceptual-onsets-to-rhythm "res1_1_pOnsets" :weighted nil))
;; (setf res1 (perceptual-onsets-to-rhythm "res1_1_pOnsets" :weighted t))
;; (plot-rhythm res1)
;; (plot (time-signal res1) nil :aspect-ratio 0.66)

(defun perceptual-salience-to-rhythm (salience-filename onsets-filename &key 
				      (sample-rate 200) (description "") (weighted t))
  (let* ((data-directory "/Volumes/iDisk/Research/Data/PerceptualOnsets/")
	 (perceptual-salience-matrix (.load-octave-file (make-pathname 
							 :directory (list :absolute data-directory) 
							 :name salience-filename
							 :type "mat")))
	 (perceptual-salience (.row perceptual-salience-matrix 0))
	 (perceptual-onsets (.load-octave-file (make-pathname 
						:directory (list :absolute data-directory) 
						:name onsets-filename
						:type "mat")))
	 ;; Assumes onset times are in milliseconds
	 (onset-times (.floor (.* (.column perceptual-onsets 0) sample-rate)))
	 (binary-grid (make-narray (onsets-to-grid (nlisp::array-to-list onset-times))))
	 (weighted-grid (.* binary-grid 1d0)))
    (plot perceptual-salience nil 
	  :aspect-ratio 0.66 :title (format nil "salience trace of ~a" salience-filename))
    (setf (.arefs weighted-grid onset-times) (.column perceptual-onsets 1))
    ;; Even though we have assumed rhythm is a set of dirac fns, we can cheat a bit.
    (make-instance 'salience-trace-rhythm 
		   :name salience-filename
		   :description description
		   :time-signal perceptual-salience
		   :onsets-time-signal (if weighted weighted-grid binary-grid)
		   :sample-rate sample-rate)))

;; (plot perceptual-salience nil :aspect-ratio 0.66)
;; (plot-cwt salience-scaleogram :title "res(1,1) continuous salience")

(defun create-beat-ridge (rhythm-to-analyse analysis)
  "Creates a ridge using the maximum value of the cumulative sum of the scaleogram energy"
  (let* ((scaleogram (scaleogram analysis))
	 (cumulative-scale-persistency (cumsum (scaleogram-magnitude scaleogram))))
	 ;; (vpo (voices-per-octave scaleogram)))
	 ;; (sample-rate (sample-rate rhythm-to-analyse))
    (loop
       for time from 0 below (duration-in-samples scaleogram)
       for scale-persistency-profile = (.column cumulative-scale-persistency time)
       ;; Maximum peak of energy is assumed to be the beat scale.
       collect (position (.max scale-persistency-profile) (val scale-persistency-profile)) into beat-scales
       finally (return (make-instance 'ridge
				      :start-sample 0 ; could be when beat period is confirmed.
				      :scales beat-scales)))))

(defmethod create-weighted-beat-ridge ((rhythm-to-analyse rhythm) (analysis multires-analysis))
  "Creates a ridge using the maximum value of the cumulative sum of the scaleogram energy"
  (let* ((scaleogram (scaleogram analysis))
	 (cumulative-scale-persistency (cumsum (scaleogram-magnitude scaleogram)))
	 (vpo (voices-per-octave scaleogram))
	 (sample-rate (sample-rate rhythm-to-analyse))
	 (salient-scale (preferred-tempo-scale vpo sample-rate))
	 (tempo-beat-preference (tempo-salience-weighting salient-scale (.array-dimensions cumulative-scale-persistency)))
 	 (weighted-persistency-profile (.* cumulative-scale-persistency tempo-beat-preference)))
    (if (find 'weighted-beat-ridge *plotting*)
	(progn
	  (window)
	  (plot-image #'magnitude-image (list weighted-persistency-profile) '((1.0 0.5) (0.0 0.3))
		      (axes-labelled-in-seconds scaleogram sample-rate 4)
		      :title (format nil "weighted persistency profile of ~a" (name rhythm-to-analyse)))
	  (close-window)))
    (loop
       for time from 0 below (duration-in-samples scaleogram)
       for scale-persistency-profile = (.column weighted-persistency-profile time)
       ;; Maximum peak of energy is assumed to be the beat scale.
       collect (position (.max scale-persistency-profile) (val scale-persistency-profile)) into beat-scales
       finally (return (make-instance 'ridge
				      :start-sample 0 ; could be when beat period is confirmed.
				      :scales beat-scales)))))

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
    (image weighted-persistency-profile nil nil :aspect-ratio 0.2)
    (loop
       for time from 0 below (duration-in-samples scaleogram)
       for scale-persistency-profile = (.column weighted-persistency-profile time)
       ;; Maximum peak of energy is assumed to be the beat scale.
       collect (position (.max scale-persistency-profile) (val scale-persistency-profile)) into beat-scales
       finally (return (make-instance 'ridge
				      :start-sample 0 ; could be when beat period is confirmed.
				      :scales beat-scales)))))

;;; Really we just want the clapping, not the ridge to be multiples thereof. So this
;;; routine is crap. 
(defmethod create-beat-multiple-ridge ((rhythm-to-analyse rhythm) (analysis multires-analysis))
  "Creates a ridge using the maximum value of the cumulative sum of the scaleogram energy"
  (let* ((scaleogram (scaleogram analysis))
	 (cumulative-scale-persistency (cumsum (scaleogram-magnitude scaleogram)))
	 (vpo (voices-per-octave scaleogram))
	 (beat-multiple 4))
	 ;; (sample-rate (sample-rate rhythm-to-analyse))
    (loop
       for time from 0 below (duration-in-samples scaleogram)
       for scale-persistency-profile = (.column cumulative-scale-persistency time)
       ;; Maximum peak of energy is assumed to be the beat scale.
       for beat-scale = (position (.max scale-persistency-profile) (val scale-persistency-profile))
       for beat-period = (* (time-support beat-scale vpo) beat-multiple) ; TODO hardwired as 4/4
       collect (round (scale-from-period beat-period vpo)) into beat-scales
       finally (return (make-instance 'ridge
				      :start-sample 0 ; could be when beat period is confirmed.
				      :scales beat-scales)))))

;;; Can return bar-scale to emulate create-beat-ridge
(defun determine-beat-scale-at-time (cumulative-scale-persistency time vpo &key diagnose)
  (let* ((scale-persistency-profile (.column cumulative-scale-persistency time))
	 (max-period (time-support (1- (.length scale-persistency-profile)) vpo))
	 (average-persistency (mean scale-persistency-profile))
	 ;; Maximum peak of energy is assumed to be the beat scale.
	 (beat-scale (position (.max scale-persistency-profile) (val scale-persistency-profile)))
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
	 (bar-scale-index (position highest-bar-scale-profile (val bar-scale-profiles)))
	 (bar-scale (.aref candidate-bar-scales bar-scale-index)))
    (if diagnose
	(let* ((selected-scales (make-double-array (.array-dimensions scale-persistency-profile))))
	  (format t "average-persistency ~a beat-scale ~a beat-period ~a bar-scale ~a~%candidate-bar-periods ~a~%candidate-bar-scales ~a~%bar-scale-profiles ~a~%"
		  average-persistency beat-scale beat-period bar-scale candidate-bar-periods candidate-bar-scales bar-scale-profiles)
	  (setf (.arefs selected-scales candidate-bar-scales) bar-scale-profiles)
	  (nplot (list scale-persistency-profile selected-scales) nil
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

;; (intervals-in-samples (nlisp::array-to-list (.column perceptual-onsets 0)) :sample-rate 100)
;; (onsets-to-grid (nlisp::array-to-list (.floor (.column perceptual-onsets 0))))

(defun compute-perceptual-versions (salience-filename onsets-filename original-sound-filename 
				    &key (start-from-beat 0) (beat-multiple 1 multiple-supplied-p)) 
  (let* ((original-sound-path (make-pathname :directory "/Volumes/iDisk/Research/Data/PerceptualOnsets/"
					     :name original-sound-filename
					     :type "wav"))
	 (accompaniment-sound-path (make-pathname :directory "/Volumes/iDisk/Research/Data/Handclap Examples"
						  :name (concatenate 'string original-sound-filename "_mixed")
						  :type "wav"))
	 (salience-trace-rhythm (perceptual-salience-to-rhythm salience-filename onsets-filename :weighted nil))
	 (salience-trace-claps (if multiple-supplied-p 
				   (clap-to-rhythm salience-trace-rhythm 
						   :start-from-beat start-from-beat
						   :beat-multiple beat-multiple
						   :tactus-selector #'create-beat-ridge) ; TODO we use unweighted ridges!
				   (clap-to-rhythm salience-trace-rhythm 
						   :start-from-beat start-from-beat
						   :tactus-selector #'create-weighted-beat-ridge)))
;;					       :tactus-selector #'create-beat-ridge))
;;					       :tactus-selector #'create-weighted-windowed-beat-ridge))
;;					       :tactus-selector #'select-longest-lowest-tactus)))
;;					       :tactus-selector #'select-tactus-by-beat-multiple)))
;;					       :tactus-selector #'create-bar-ridge)))
;;					       :tactus-selector #'create-beat-multiple-ridge)))
	 ;; TODO gotta be a better way for division than making it a double-float.
	 (clap-times-in-seconds (./ salience-trace-claps (* 1d0 (sample-rate salience-trace-rhythm)))))
    ;; Plot the salience trace, the produced onsets, & zero phase.
    (plot-claps salience-trace-rhythm 
		(.find (onsets-time-signal salience-trace-rhythm))
		;; empty phase
		(make-double-array (duration-in-samples salience-trace-rhythm))
		:signal-name (format nil "perceptual onsets plot of ~a" (name salience-trace-rhythm)))

    ;;(plot (.column perceptual-onsets 1) (.find weighted-onsets-onsets 0) :style "impulses"
    ;;				    :title rhythm-name :aspect-ratio 0.66)

    ;; TODO Should have a save-rhythm-and-claps method specialised on
    ;; salience-trace-rhythm that uses the weighted onsets rhythm so that we know the
    ;; onsets are as the detector computes them.
    ;; (save-rhythm-and-claps salience-trace-rhythm salience-trace-claps)
    (format t "Beat times of ~a in seconds:~%~a~%" (name salience-trace-rhythm) clap-times-in-seconds)
    ; (save-rhythm salience-trace-claps)	; just write out the claps as a scorefile alone.
    (save-rhythm-mix accompaniment-sound-path original-sound-path clap-times-in-seconds)
    (format t "Wrote rhythm as scorefile ~a~%" salience-filename)
    (format t "Wrote mix as soundfile ~a~%" accompaniment-sound-path)))

;;; TODO This should be replaced when we integrate Plymouth's onsets thresholding function
;;; into our code.
(defmethod onset-time-of-beat ((rhythm salience-trace-rhythm) beat-numbers)
  "Returns the sample number of the beat-number'th beat in the given rhythm"
  (let* ((onsets-rhythm (make-instance 'rhythm 
				       :time-signal (onsets-time-signal rhythm)
				       :sample-rate (sample-rate rhythm))))
    (onset-time-of-beat onsets-rhythm beat-numbers)))


#|
;;; TODO this is a total hack and is disgusting and should be replaced ASAP when we
;;; integrate Plymouth's onsets thresholding function into our code. 
(defmethod onset-time-of-beat ((rhythm salience-trace-rhythm) beat-numbers)
  "Returns the sample number of the beat-number'th beat in the given rhythm"
  (let* ((original-rhythm-filename (name rhythm))
	 (prefix (subseq original-rhythm-filename 0 (search "_resp_text" original-rhythm-filename)))
	 (onsets-filename (concatenate 'string prefix "_pOnsets_text"))
	 (onsets-rhythm (perceptual-onsets-to-rhythm onsets-filename :weighted nil)))
    (onset-time-of-beat onsets-rhythm beat-numbers)))

;; Problem is, 6 times the beat-period is typically longer than the maximum scale, so
;; there will be less examples of energy. If there is, say due to a longer analysis
;; window, it is likely the energy will be more due to low frequency signals, than
;; necessarily longer periods.

;; Simply sums the magnitude modulus contributions at multiples of the candidate periods.
(defun meter-of-scale-profile (scale-profile beat-scale voices-per-octave)
  "Look for harmonicity of the beat, either duple or triple. Returns the selected beat multiple."
  (let* ((beat-period (time-support beat-scale voices-per-octave))
	 (max-period (time-support (1- (.length scale-profile)) voices-per-octave))
	 (max-multiple (floor max-period beat-period))
	 ;; Ensure there are no candidate bar scales that exceed the total scales, but
	 ;; check all multiples up to 7/8 periods.
	 ;; (beat-multiples (.iseq (min 2 max-multiple) (min 7 max-multiple)))
	 ;;TODO  must check if these exceed max-multiple.
	 (beat-multiples (make-instance 'n-fixnum-array :ival #2A((2 4) (3 6))))
	 (candidate-bar-periods (.* beat-period beat-multiples))
	 (candidate-bar-scales (.round (scale-from-period candidate-bar-periods voices-per-octave)))
	 ;; look at the scale persistency profiles at candidate-bar-scales locations
	 (bar-scale-profiles (.arefs scale-profile candidate-bar-scales))
	 (meter-evidence (.partial-sum (.transpose bar-scale-profiles)))
	 (meter-index (position (.max meter-evidence) (val meter-evidence)))) ; (argmax meter-evidence)
    (plot scale-profile nil)
    (format t "candidate bar periods ~a bar-scale-profiles ~a meter evidence ~a~%"
	    candidate-bar-periods bar-scale-profiles meter-evidence)
    (.aref beat-multiples meter-index 0)))
|#
