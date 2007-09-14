; -*- Lisp -*-

(in-package :multires-rhythm)

(defun perceptual-onsets-to-rhythm (filename &key (sample-rate 200) (description "")
				    (weighted t))
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
    ;;(plot (.column perceptual-onsets 1) (.column perceptual-onsets 0) :style "impulses"
    ;;				    :title rhythm-name :aspect-ratio 0.66)
    (make-instance 'rhythm 
		   :name rhythm-name
		   :description description
		   :time-signal (if weighted weighted-grid binary-grid)
		   :sample-rate sample-rate)))

;; (setf res1 (perceptual-onsets-to-rhythm "res1_1_pOnsets" :weighted nil))
;; (setf res1 (perceptual-onsets-to-rhythm "res1_1_pOnsets" :weighted t))
;; (plot-rhythm res1)
;; (plot (time-signal res1) nil :aspect-ratio 0.66)

(defun perceptual-salience-to-rhythm (filename &key (sample-rate 200) (description ""))
  (let* ((file-path (make-pathname 
		     :directory (list :absolute "/Volumes/iDisk/Research/Data/PerceptualOnsets/") 
		     :name filename
		     :type "mat"))
	 (perceptual-salience-matrix (.load-octave-file file-path))
	 (perceptual-salience (.row perceptual-salience-matrix 0)))
    (plot perceptual-salience nil 
	  :aspect-ratio 0.66 :title (format nil "salience trace of ~a" filename))
    ;; Even though we have assumed rhythm is a set of dirac fns, we can cheat a bit.
    (make-instance 'rhythm 
		   :name filename
		   :description description
		   :time-signal perceptual-salience
		   :sample-rate sample-rate)))

;; (plot perceptual-salience nil :aspect-ratio 0.66)
;; (plot-cwt salience-scaleogram :title "res(1,1) continuous salience")

(defmethod create-beat-ridge ((rhythm-to-analyse rhythm) (analysis multires-analysis))
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
    (image weighted-persistency-profile nil nil :aspect-ratio 0.2)
    (loop
       for time from 0 below (duration-in-samples scaleogram)
       for scale-persistency-profile = (.column weighted-persistency-profile time)
       ;; Maximum peak of energy is assumed to be the beat scale.
       collect (position (.max scale-persistency-profile) (val scale-persistency-profile)) into beat-scales
       finally (return (make-instance 'ridge
				      :start-sample 0 ; could be when beat period is confirmed.
				      :scales beat-scales)))))

(defmethod create-weighted-windowed-beat-ridge ((rhythm-to-analyse rhythm) (analysis multires-analysis))
  "Creates a ridge using the maximum value of the cumulative sum of the scaleogram energy"
  (let* ((window-in-seconds 2d0)
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

(defmethod create-beat-multiple-ridge ((rhythm-to-analyse rhythm) (analysis multires-analysis))
  "Creates a ridge using the maximum value of the cumulative sum of the scaleogram energy"
  (let* ((scaleogram (scaleogram analysis))
	 (cumulative-scale-persistency (cumsum (scaleogram-magnitude scaleogram)))
	 (vpo (voices-per-octave scaleogram)))
	 ;; (sample-rate (sample-rate rhythm-to-analyse))
    (loop
       for time from 0 below (duration-in-samples scaleogram)
       for scale-persistency-profile = (.column cumulative-scale-persistency time)
       ;; Maximum peak of energy is assumed to be the beat scale.
       for beat-scale = (position (.max scale-persistency-profile) (val scale-persistency-profile))
       for beat-period = (* (time-support beat-scale vpo) 4) ; TODO hardwired as 4/4
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

#|
(defmethod create-bar-ridge ((rhythm-to-analyse rhythm) (analysis multires-analysis))
  "Creates a ridge using the maximum value of the cumulative sum of the scaleogram energy"
  (let* ((window-in-seconds 2d0)
	 (scaleogram (scaleogram analysis))
	 ;; Cumulative persistency should be a moving window.
	 (cumulative-scale-persistency (cumsum (scaleogram-magnitude scaleogram)))
	 (vpo (voices-per-octave scaleogram)))
    (loop
       for time from 0 below (duration-in-samples scaleogram)
       collect (determine-beat-scale-at-time cumulative-scale-persistency time vpo) into beat-scales
       finally (return (make-instance 'ridge
				      :start-sample 0 ; could be when beat period is confirmed.
				      :scales beat-scales)))))
|#

;; (intervals-in-samples (nlisp::array-to-list (.column perceptual-onsets 0)) :sample-rate 100)
;; (onsets-to-grid (nlisp::array-to-list (.floor (.column perceptual-onsets 0))))

(defun sample-at-times (length-of-sound sample-sound times-in-seconds)
  "Returns a sound with sample-sound placed beginning at each time specified in seconds. Uses the sample rate of the sample-sound."
  (let* ((sample-rate (sample-rate sample-sound))
	 (sample-length (sound-length sample-sound))
	 (attack-times-in-frames (.floor (.* times-in-seconds sample-rate)))
	 (full-duration (make-double-array length-of-sound)))
    (loop
       for attack-time across (val attack-times-in-frames)
       for region-to-copy = (list attack-time (min (1- length-of-sound) (+ attack-time sample-length -1)))
       do
	 ;; (format t "region to copy ~a~%" region-to-copy)
	 (setf (.subarray full-duration (list 0 region-to-copy)) (sound-signal sample-sound)))
    (make-instance 'sound
		   :description (format nil "~a duplications of ~a"
					(.length times-in-seconds) (description sample-sound))
		   :sound-signal full-duration
		   :sample-rate sample-rate)))

;; (setf hihat (sound-from-file #P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff"))
;; (setf hats (sample-at-times 44100 hihat (make-narray '(0.0 0.33 0.66 0.8))))

(defun save-rhythm-mix (filename-to-write original-rhythm-file clap-times 
			&key (clap-sample-file #P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff"))
  (let* ((original-rhythm-sound (sound-from-file original-rhythm-file)) ; load original file
	 (clap-sample (sound-from-file clap-sample-file))
	 ;; create an sound vector with our clap-sample
	 (clapping-sound (sample-at-times (sound-length original-rhythm-sound) clap-sample clap-times))
	 (clapping-mix (sound-mix original-rhythm-sound clapping-sound)))
    (save-to-file clapping-mix filename-to-write)))

;; (save-rhythm-mix #P"/Users/leigh/test-hats.wav" #P"/Volumes/iDisk/Research/Data/PerceptualOnsets/results1_3.wav"(make-narray '(0.0 0.33 0.66 0.8)))

(defun compute-perceptual-versions (salience-filename onsets-filename original-sound-filename 
				    &key (start-from-beat 0)) 
  (let* ((original-sound-path (make-pathname :directory "/Volumes/iDisk/Research/Data/PerceptualOnsets"
					     :name original-sound-filename
					     :type "wav"))
	 (accompaniment-sound-path (make-pathname :directory "/Volumes/iDisk/Research/Data/Handclap Examples"
						  :name (concatenate 'string original-sound-filename "_mixed")
						  :type "wav"))
	 (weighted-onsets-rhythm (perceptual-onsets-to-rhythm onsets-filename :weighted t))
	 (salience-trace-rhythm (perceptual-salience-to-rhythm salience-filename))
	 (salience-trace-claps (clap-to-rhythm salience-trace-rhythm 
					       :start-from-beat start-from-beat 
;;					       :tactus-selector #'select-longest-lowest-tactus)))
;;					       :tactus-selector #'select-tactus-by-beat-multiple)))
;;					       :tactus-selector #'create-beat-ridge)))
;;					       :tactus-selector #'create-weighted-windowed-beat-ridge)))
					       :tactus-selector #'create-weighted-beat-ridge)))
;;					       :tactus-selector #'create-bar-ridge)))
;;					       :tactus-selector #'create-beat-multiple-ridge)))
    ;; Change the name so the filename saved is distinguished
    (setf (name weighted-onsets-rhythm) (name salience-trace-rhythm))
    ;; We use the weighted onsets rhythm so that we know the onsets are as the detector computes them.
    ;; TODO use (save-rhythm-and-claps (threshold-rhythm salience-trace-rhythm) salience-trace-claps)
    (save-rhythm-and-claps weighted-onsets-rhythm salience-trace-claps)
    (save-rhythm-mix accompaniment-sound-path
		     original-sound-path
		     (./ salience-trace-claps (* 1d0 (sample-rate salience-trace-rhythm)))) ; TODO gotta be a better way
    (format t "Wrote rhythm as scorefile ~a~%" salience-filename)
    (format t "Wrote mix as soundfile ~a~%" accompaniment-sound-path)))

;;; TODO this is a total hack and is disgusting and should be replaced ASAP!
(defun onset-time-of-beat (rhythm beat-numbers)
  "Returns the sample number of the beat-number'th beat in the given rhythm"
  (let* ((original-rhythm-filename (name rhythm))
	 (prefix (subseq original-rhythm-filename 0 (search "_resp_text" original-rhythm-filename)))
	 (onsets-filename (concatenate 'string prefix "_pOnsets_text"))
	 (onsets-rhythm (perceptual-onsets-to-rhythm onsets-filename :weighted nil)))
    (time-of-beat onsets-rhythm beat-numbers)))



