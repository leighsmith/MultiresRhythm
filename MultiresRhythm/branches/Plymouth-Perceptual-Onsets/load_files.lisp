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
    (plot (.column perceptual-onsets 1) (.column perceptual-onsets 0) :style "impulses"
				    :title rhythm-name :aspect-ratio 0.66)
    (make-instance 'rhythm 
		   :name rhythm-name
		   :description description
		   :time-signal (if weighted weighted-grid binary-grid)
		   :sample-rate sample-rate)))

;; (setf res1 (perceptual-onsets-to-rhythm "res1_1_pOnsets" :weighted nil))
;; (setf res1 (perceptual-onsets-to-rhythm "res1_1_pOnsets" :weighted t))
;; (plot-rhythm res1)
;; (plot (time-signal res1) nil :aspect-ratio 0.66)
;; (setf res1-claps (clap-to-rhythm res1))
;; (save-rhythm-and-claps res1 res1-claps)

;; (clap-to-rhythm res1 :start-from-beat 0)

;; (setf res3 (perceptual-onsets-to-rhythm "res3_1_pOnsets_text" :weighted t))
;; (plot (time-signal res3) nil :aspect-ratio 0.66)
;; (setf res3-claps (clap-to-rhythm res3))
;; (setf res3-claps (clap-to-rhythm res3 :start-from-beat 0))
;; (save-rhythm-and-claps res3 res3-claps)
;; (setf res3 (perceptual-onsets-to-rhythm "res3_1_pOnsets_text" :weighted nil))
;; (plot-rhythm res3)

;; (setf res3 (perceptual-onsets-to-rhythm "res3_20_pOnsets_text" :weighted t))
;; (plot (time-signal res3) nil :aspect-ratio 0.66)
;; (setf res3-claps (clap-to-rhythm res3))
;; (setf res3-claps (clap-to-rhythm res3 :start-from-beat 1))
;; (save-rhythm-and-claps res3 res3-claps)
;; (setf res3 (perceptual-onsets-to-rhythm "res3_1_pOnsets_text" :weighted nil))

;; (setf res3 (perceptual-onsets-to-rhythm "res3_11_pOnsets_text" :weighted t))
;; (plot (time-signal res3) nil :aspect-ratio 0.66)
;; (setf res3-claps (clap-to-rhythm res3 :start-from-beat 0))
;; (save-rhythm-and-claps res3 res3-claps)
;; (setf res3 (perceptual-onsets-to-rhythm "res3_1_pOnsets_text" :weighted nil))

(defun perceptual-salience-to-rhythm (filename &key (sample-rate 200) (description ""))
  (let* ((file-path (make-pathname 
		     :directory (list :absolute "/Volumes/iDisk/Research/Data/PerceptualOnsets/") 
		     :name filename
		     :type "mat"))
	 (perceptual-salience-matrix (.load-octave-file file-path))
	 (perceptual-salience (.row perceptual-salience-matrix 0)))
    ;; Even though we have assumed rhythm is a set of dirac fns, we can cheat a bit.
    (make-instance 'rhythm 
		   :name filename
		   :description description
		   :time-signal perceptual-salience
		   :sample-rate sample-rate)))

;; (plot perceptual-salience nil :aspect-ratio 0.66)
;; (plot-cwt salience-scaleogram :title "res(1,1) continuous salience")

;; (setf res1-salience (perceptual-salience-to-rhythm "res1_1_continuous_salience"))
;; (setf res1-salience-claps (clap-to-rhythm res1-salience :start-from-beat 0))
;; (save-rhythm-and-claps res1 res1-salience-claps)

;; (setf res3-salience (perceptual-salience-to-rhythm "res3_1_resp_text"))
;; (setf res3-salience-claps (clap-to-rhythm res3-salience :start-from-beat 0))
;; (save-rhythm-and-claps (threshold-rhythm res3-salience) res3-salience-claps)

;; (setf res3-salience (perceptual-salience-to-rhythm "res3_20_resp_text"))
;; (setf res3-salience-claps (clap-to-rhythm res3-salience :start-from-beat 0))
;; (save-rhythm-and-claps (threshold-rhythm res3-salience) res3-salience-claps)
;; (save-rhythm-and-claps res3 res3-salience-claps)

;; (setf res3-salience (perceptual-salience-to-rhythm "res3_11_resp_text"))
;; (setf res3-salience-claps (clap-to-rhythm res3-salience :start-from-beat 0))
;; (save-rhythm-and-claps (threshold-rhythm res3-salience) res3-salience-claps)
;; (save-rhythm-and-claps res3 res3-salience-claps)

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

(defmethod create-bar-ridge ((rhythm-to-analyse rhythm) (analysis multires-analysis))
  "Creates a ridge using the maximum value of the cumulative sum of the scaleogram energy"
  (let* ((scaleogram (scaleogram analysis))
	 ;; Cumulative persistency should be a moving window.
	 (cumulative-scale-persistency (cumsum (scaleogram-magnitude scaleogram)))
	 (vpo (voices-per-octave scaleogram))
	 ;; (sample-rate (sample-rate rhythm-to-analyse))
 	 ;; (beat-scale (scale-from-period beat-period vpo))
 	 (beat-multiples (.iseq 1 7)))  ; determines the bar periods
    (loop
       for time from 0 below (duration-in-samples scaleogram)
       for scale-persistency-profile = (.column cumulative-scale-persistency time)
       ;; Maximum peak of energy is assumed to be the beat scale.
       for beat-scale = (position (.max scale-persistency-profile) (val scale-persistency-profile))
       for beat-period = (time-support beat-scale vpo)
       for candidate-bar-scales = (scale-from-period (.* beat-period beat-multiples) vpo)
       ;; TODO look at candidate-bar-scales locations
       ;; (.arefs scale-persistency-profile candidate-bar-scales)
       collect beat-scale into beat-scales
       finally (return (make-instance 'ridge
				      :start-sample 0 ; could be when beat period is confirmed.
				      :scales beat-scales)))))

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
       do (setf (.subarray full-duration (list 0 (list attack-time (+ attack-time sample-length -1))))
		(sound-signal sample-sound)))
    (make-instance 'sound
		   :sound-signal full-duration
		   :sample-rate sample-rate)))

;; (setf hats (sample-at-times 44100 hihat (make-narray '(0.0 0.33 0.66 0.8))))

(defun save-rhythm-mix (filename-to-write original-rhythm-file clap-times 
			&key (clap-sample-file #P"/Users/leigh/Library/Sounds/hihat_closed.aiff"))
  (let* ((original-rhythm-sound (sound-from-file original-rhythm-file)) ; load original file
	 (clap-sample (sound-from-file clap-sample-file))
	 ;; create an sound vector with our clap-sample
	 (clapping-sound (sample-at-times (sound-length original-rhythm-sound) clap-sample clap-times))
	 (clapping-mix (sound-mix original-rhythm-sound clapping-sound)))
    (save-to-file clapping-mix filename-to-write)))

;; (save-rhythm-mix #P"/Users/leigh/test-hats.wav" #P"/Volumes/iDisk/Research/Data/PerceptualOnsets/results1_3.wav"(make-narray '(0.0 0.33 0.66 0.8)))


(defun compute-perceptual-versions (salience-filename onsets-filename &key (start-from-beat 0)) 
  (let* ((weighted-onsets-rhythm (perceptual-onsets-to-rhythm onsets-filename :weighted t))
	 (salience-trace-rhythm (perceptual-salience-to-rhythm salience-filename))
	 (salience-trace-claps (clap-to-rhythm salience-trace-rhythm 
					       :start-from-beat start-from-beat 
;;					       :tactus-selector #'select-tactus-by-beat-multiple)))
					       :tactus-selector #'create-beat-ridge)))
    (plot (time-signal salience-trace-rhythm) nil 
	  :aspect-ratio 0.66 :title (format nil "salience trace of ~a" salience-filename))
    ;; Change the name so the filename saved is distinguished
    (setf (name weighted-onsets-rhythm) (name salience-trace-rhythm))
    ;; We use the weighted onsets rhythm so that we know the onsets are as the detector computes them.
    ;; TODO use (save-rhythm-and-claps (threshold-rhythm salience-trace-rhythm) salience-trace-claps)
    (save-rhythm-and-claps weighted-onsets-rhythm salience-trace-claps)
    (save-rhythm-mix #P"/Volumes/iDisk/Research/Data/Handclap Examples/results1_3_mixed.wav"
		     #P"/Volumes/iDisk/Research/Data/PerceptualOnsets/results1_3.wav"
		     (./ salience-trace-claps (* 1d0 (sample-rate salience-trace-rhythm)))) ; TODO gotta be a better way
    (format t "Wrote ~a as scorefile~%" salience-filename)))

;; (compute-perceptual-versions "res3_1_resp_text" "res3_1_pOnsets_text")
;; (compute-perceptual-versions "res3_7_resp_text" "res3_7_pOnsets_text")
;; (compute-perceptual-versions "res3_9_resp_text" "res3_9_pOnsets_text")
;; (compute-perceptual-versions "res3_11_resp_text" "res3_11_pOnsets_text" :start-from-beat 1)
;; (compute-perceptual-versions "res3_20_resp_text" "res3_20_pOnsets_text")

;; (compute-perceptual-versions "res1_1_resp_text" "res1_1_pOnsets_text")
;; (compute-perceptual-versions "res1_2_resp_text" "res1_2_pOnsets_text")
;; (compute-perceptual-versions "res1_3_resp_text" "res1_3_pOnsets_text")

