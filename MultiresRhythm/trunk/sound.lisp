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

;; Perhaps needs to be a list of mono channels, or just a single matrix, cols = frames, rows = channels.
;; Everything held as normalised floating point
(defclass sound ()
  ((sample-rate 
    :documentation "Assumes single sample rate for all channels."
    :initarg :sample-rate
    :accessor sample-rate
    :initform 44100.0)
   (description 
    :documentation "An optional character string describing the sound."
    :initarg :description 
    :accessor description 
    :initform "")
   (sound-signal
    :documentation "An n-double-array matrix holding normalised samples in columns, channels in rows"
    :initarg :sound-signal
    :accessor sound-signal)))

;; (defgeneric sound-from-file 

(defgeneric channel-count (sound-to-analyse)
  (:documentation "Returns the number of channels in the sound"))

(defgeneric frame-count (sound-to-analyse)
  (:documentation "Returns the number of sample frames (indepenent of channels) in the sound"))

;;;; Implementation.

(defmethod channel-count ((sound-to-analyse sound))
  (.row-count (sound-signal sound-to-analyse)))

(defmethod frame-count ((sound-to-analyse sound))
  (.column-count (sound-signal sound-to-analyse)))

(defmethod print-object ((sound-to-print sound) stream)
  (call-next-method sound-to-print stream) ;; to print the superclass.
  (format stream " ~a frames ~a sample-rate ~a channels ~a"
	  (description sound-to-print) 
	  (frame-count sound-to-print)
	  (sample-rate sound-to-print)
	  (channel-count sound-to-print)))

(defmethod monophonic-p ((sound-to-analyse sound))
  (= (channel-count sound-to-analyse) 1))

(defmethod sound-from-file ((path pathname))
  (multiple-value-bind (sound-signal sample-rate) (load-audio path)
    (make-instance 'sound 
		   :description (namestring path)
		   :sound-signal sound-signal
		   :sample-rate sample-rate)))

(defmethod save-to-file ((sound-to-save sound) (path pathname))
  (save-audio path (sound-signal sound-to-save) :sample-rate (sample-rate sound-to-save)))

(defmethod onset-times ((sound-to-analyse sound))
  (./ (.* (.find (sound-signal sound-to-analyse)) 1d0) (sample-rate sound-to-analyse)))

;;; Should be .length
(defmethod sound-length ((sound-to-analyse sound))
  (.length (sound-signal sound-to-analyse)))

(defmethod sound-channel ((sound-to-retrieve sound) channel-number)
  "Returns an narray of sound from a given (base 0) channel"
  (.row (sound-signal sound-to-retrieve) channel-number))

;; TODO Should accept arbitary # of sounds. Should test number of channels are equal.
(defmethod sound-mix ((sound-to-mix1 sound) (sound-to-mix2 sound)) ; &rest sounds
  (make-instance 'sound
		 :sound-signal (.+ (sound-signal sound-to-mix1) (sound-signal sound-to-mix2))
		 :sample-rate (sample-rate sound-to-mix1))) ; TODO should test SRs are all equal.

(defmethod normalise ((sound-to-normalise sound))
  "Normalise over the entire sound to a bipolar range (between -1.0 and 1.0)"
  (let* ((sound-vector (sound-signal sound-to-normalise))
	 (maximum-displacement (max (.max sound-vector) (abs (.min sound-vector)))))
    (setf (sound-signal sound-to-normalise) (./ sound-vector maximum-displacement))))

(defmethod monophonic ((sound-to-make-mono sound))
  "Reduce multiple channels to a single channel sound"
  (let* ((sound-matrix (sound-signal sound-to-make-mono))
	 (number-of-channels (.row-count sound-matrix)))
    (loop
       for channel-index from 1 below number-of-channels
       for mono-signal = (.row sound-matrix 0) then (.+ mono-signal (.row sound-matrix channel-index))
       finally (setf (sound-signal sound-to-make-mono) mono-signal))))

(defmethod multichannel ((sound-to-increase sound) new-channel-count)
  "Duplicates the existing channels of the sound to return a new sound with the new number of channels."
  (let* ((original-channels (channel-count sound-to-increase))
	 (new-audio-matrix (make-double-array (list new-channel-count (frame-count sound-to-increase)))))
    (loop
	 for repetition-row from 0 below new-channel-count by original-channels
	 for how-many = (if (< (- new-channel-count repetition-row) original-channels)
			    (- new-channel-count repetition-row)
			    original-channels)
;;	 do (setf (.subseq new-audio-matrix (list (list repetition-row 0) 
;;						  (list (+ repetition-row how-many) 0)))
;;		  (sound-signal sound-to-increase)))
	 do (setf (.row new-audio-matrix repetition-row) (sound-signal sound-to-increase)))
    (make-instance 'sound 
		   :sound-signal new-audio-matrix
		   :sample-rate (sample-rate sound-to-increase)
		   :description (format nil "~a channels of ~a" new-channel-count (description sound-to-increase)))))

(defun sample-at-times (length-of-sound sample-sound times-in-seconds 
			&key (amplitudes (make-double-array (.length times-in-seconds) :initial-element 1.0d0)))
  "Returns a sound with sample-sound placed beginning at each time specified in seconds. Uses the sample rate of the sample-sound."
  (let* ((sample-rate (sample-rate sample-sound))
	 (sample-length (sound-length sample-sound))
	 (attack-times-in-frames (.floor (.* times-in-seconds sample-rate)))
	 (full-duration (make-double-array length-of-sound)))
    (loop
       for attack-time across (val attack-times-in-frames)
       for amplitude-scaler across (val amplitudes)
       for region-to-copy = (list attack-time (min (1- length-of-sound) (+ attack-time sample-length -1)))
       do
	 ;; (format t "region to copy ~a~%" region-to-copy)
	 (setf (.subarray full-duration (list 0 region-to-copy)) (.* (sound-signal sample-sound) amplitude-scaler)))
    (make-instance 'sound
		   :description (format nil "~a duplications of ~a"
					(.length times-in-seconds) (description sample-sound))
		   :sound-signal full-duration
		   :sample-rate sample-rate)))

;; (setf hihat (sound-from-file #P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff"))
;; (setf hats (sample-at-times 44100 hihat (make-narray '(0.0 0.33 0.66 0.8))))

(defun save-rhythm-mix (filename-to-write original-rhythm-file clap-times-seconds 
			&key (clap-sample-file #P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff")
			(clap-amplitudes nil amplitudes-supplied))
  "Mix the sound file original-rhythm-file with the claps marked by clap-sample-file at times clap-times-seconds."
  (let* ((original-rhythm-sound (sound-from-file original-rhythm-file)) ; load original file
	 (clap-sample (sound-from-file clap-sample-file))
	 ;; create an sound vector with our clap-sample
	 (clapping-sound (if amplitudes-supplied 
			     (sample-at-times (sound-length original-rhythm-sound)
					      clap-sample 
					      clap-times-seconds 
					      :amplitudes clap-amplitudes)
			     (sample-at-times (sound-length original-rhythm-sound) clap-sample clap-times-seconds)))
	 (clapping-mix (sound-mix original-rhythm-sound
				  (if (< (channel-count clapping-sound) (channel-count original-rhythm-sound))
				      (multichannel clapping-sound (channel-count original-rhythm-sound))
				      clapping-sound))))
    (normalise clapping-mix)
    (save-to-file clapping-mix filename-to-write)))

;; (save-rhythm-mix #P"/Users/leigh/test-hats.wav" #P"/Volumes/iDisk/Research/Data/PerceptualOnsets/res1/res1_3.wav" (make-narray '(0.0 0.33 0.66 0.8)))


;; (setf down-sampled (.floor (.* (onset-times jw-clicks) 200)))
;; (setf down-sampled-length (floor (* (.length (sound-signal jw-clicks)) (/ 200.0 44100.0))))
;; (setf down-sampled-rhythm (make-double-array down-sampled-length))
;; (setf (.arefs down-sampled-rhythm down-sampled) (make-double-array (.length down-sampled) :initial-element 1d0))
;; (plot down-sampled-rhythm nil)
;; (setf jw-prelude-3 (make-instance 'rhythm :time-signal down-sampled-rhythm :name "jw prelude 3"))

(defmethod down-sample ((sound-to-decimate sound) factor)
  "Downsample the sound by a given integer factor"
  (setf (sound-signal sound-to-decimate) (sound-signal sound-to-decimate))
  (setf (sample-rate sound-to-decimate) (/ (sample-rate sound-to-decimate) factor)))

;; (setf noisy-dialog (load-audio #P"/Users/leigh/Research/Data/RoomTone/noisy_dialog.aiff"))
;; (setf noisy-dialog-initial (.subarray noisy-dialog '(0 (0 32767))))
;; (plot noisy-dialog-initial nil :aspect-ratio 0.2)
;; (setf noisy-dialog-scaleogram (cwt noisy-dialog-initial 8))
;; (plot-cwt noisy-dialog-scaleogram)
;; (plot-scale-energy-at-times noisy-dialog-scaleogram (list 2048 8192))

;; 20mS
(defun make-envelope (amplitude &key (sample-rate 200) (attack 0.020) (decay 0.0) (sustain 0.0) (release 0.020))
  "Returns a non-exponential ADSR envelope."
  (declare (ignore decay))
  (let* ((attack-samples (round (* attack sample-rate)))
	 (release-samples (round (* release sample-rate)))
	 (duration-samples (round (* sustain sample-rate)))
	 (sustain-samples (- duration-samples attack-samples release-samples)))
    (.concatenate (.rseq 0.0 amplitude attack-samples) 
		  (.rseq amplitude amplitude sustain-samples)
		  (.rseq amplitude 0.0 release-samples))))

(defun square-wave (frequency duration sample-rate)
  "Returns a full amplitude square wave"
  (let* ((wavelength-in-samples (/ sample-rate frequency))
	 (half-wavelength (/ wavelength-in-samples 2.0))
	 (duration-in-samples (floor (* sample-rate duration)))
	 (sound-signal (make-double-array duration-in-samples)))
    (loop
       for sample-index from 0 below duration-in-samples
       do (setf (.aref sound-signal sample-index) (if (> (mod sample-index wavelength-in-samples)
							 half-wavelength)
						      1.0d0
						      -1.0d0))
       finally (return sound-signal))))

(defmethod plot-sound ((sound-to-plot sound))
  (let ((frames (frame-count sound-to-plot)))
    (nplot (loop 
	      for channel-index from 0 below (channel-count sound-to-plot) 
	      collect (sound-channel sound-to-plot channel-index))
	  (.rseq 0 (/ (coerce frames 'double-float) (sample-rate sound-to-plot)) frames)
	  :title (description sound-to-plot)
	  :xlabel "Time (seconds)"
	  :ylabel "Amplitude"
	  :legends (loop for channel-index from 0 below (channel-count sound-to-plot) 
		      collect (format nil "channel ~a" channel-index))
	  :aspect-ratio 0.15)))

