;;;; -*- Lisp -*-
;;;;
;;;; $Id: multires_rhythm.lisp 285 2007-07-24 10:00:51Z leigh $
;;;; 
;;;; Beat tracking (i.e. "clapping along") to musical rhythm using multiresolution model of musical rhythm.
;;;; Uses the tactus ridges to produce beats ("hand-claps", or "foot-taps").
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006, 2007 All Rights Reserved.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; See multiresrhythm.asd for further info.
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

;;; TODO Should have a clapping behaviour class which specifies which beat to start clapping on
;; and how often (it's a production task).

(defgeneric clap-to-rhythm (rhythm-to-analyse &key tactus-selector start-from-beat)
  (:documentation "Returns a set of sample times to clap to given the supplied rhythm"))

;; TODO Return ((time intensity) (time intensity) ...)
;; use constant intensity claps but weight the amplitude for when we mix in Common Music.
(defun clap-to-tactus-phase (original-rhythm rhythm-scaleogram tactus 
			     &key (start-from-beat 0) (beat-multiple 1))
  "Function to compute times to clap and how hard from the extracted tactus.
   Returns a matrix where row 1 is the time to tap at, row 2 is the intensity."
  (let* ((time-frequency-dimensions (.array-dimensions (scaleogram-magnitude rhythm-scaleogram)))
	 (time-in-samples (second time-frequency-dimensions))

	 ;; The right way to do this is with a tight (well, two octave)
	 ;; Gaussian envelope over it.
	 ;; tactus-mag (gaussian-envelope tactus voices-per-octave)
	 ;; But just a single voice alone produces the correct oscillation, with lower amplitude:
	 (empty-magnitude (make-double-array time-frequency-dimensions))
	 (tactus-mag (dolist (tactus-ridge (if (listp tactus) tactus (list tactus)) empty-magnitude)
			     (insert-ridge tactus-ridge empty-magnitude :constant-value 1d0)))

	 ;; TODO To be completely correct the original padded output from dyadic-cwt should be used
	 ;; for input to the dyadic-icwt.
	 ;; TODO create a scalogram
	 ;; (clamped-magnitude-scaleogram (copy-instance rhythm-scaleogram))
	 ;; (setf (scaleogram-magnitude clamped-magnitude-scalogram) tactus-mag)
	 ;; (icwt clamped-magnitude-scaleogram)
	 ;; Use the modified magnitude and original phase to compute a sinusoid and it's
	 ;; Hilbert transform, from that, determine the phase of the sinusoid.
	 (foot-tap-phase (.phase (icwt tactus-mag
				       (scaleogram-phase rhythm-scaleogram)
				       (voices-per-octave rhythm-scaleogram))))

	 ;; Note the phase of the oscillating sinusoid at the beat to start tapping from.
	 ;; (down-beat-sample (time-of-beat original-rhythm start-from-beat))
	 (down-beat-sample (onset-time-of-beat original-rhythm start-from-beat))
	 (clap-on-phase-datum (.aref foot-tap-phase down-beat-sample))

	 ;; check clap-on-phase-datum >= current and < next phase measure.
	 ;; TODO this could be a problem on the last phase point before 2 pi wrap.
	 ;; identify reoccurance of the initial clap phase across the translation (time) axis
	 (phase-reoccured (.and (.>= clap-on-phase-datum 
				     (.subarray foot-tap-phase (list 0 (list 0 (- time-in-samples 2))))) 
				(.< clap-on-phase-datum 
				    (.subarray foot-tap-phase (list 0 (list 1 (1- time-in-samples)))))))
	 ;; Stops any clapping before the downbeat.
	 (valid-phase (.and phase-reoccured 
			    (.>= (.rseq 0d0 (1- (.length phase-reoccured)) (.length phase-reoccured)) down-beat-sample)))
	 (valid-claps (.find valid-phase))
	 (subdivided-beats (.* (.iseq 0 (1- (floor (.length valid-claps) beat-multiple))) beat-multiple))
	 (clap-at (.arefs valid-claps subdivided-beats))
	 ;; Create a clapping rhythm
	 ;; (clap-rhythm (rhythm-of-onsets (name original-rhythm) clap-at))
	 )
    (format t "Handclapping every ~d beat from beat ~d of original rhythm, sample ~d~%"
	    beat-multiple start-from-beat down-beat-sample)
    (plot-claps original-rhythm clap-at foot-tap-phase)
    clap-at))

(defmethod clap-to-rhythm ((performed-rhythm rhythm) &key 
			   (start-from-beat 1 downbeat-supplied-p)
			   (tactus-selector #'select-longest-lowest-tactus))
  "Returns a set of sample times to clap to given the supplied rhythm"
  (multiple-value-bind (computed-tactus rhythm-analysis)
      (tactus-for-rhythm performed-rhythm :tactus-selector tactus-selector)
;;    (let* ((beat-period (beat-period-of-rhythm performed-rhythm (skeleton rhythm-analysis)))
    (let* ((beat-period (unweighted-beat-period-of-rhythm performed-rhythm (scaleogram rhythm-analysis)))
	   (found-downbeat (if downbeat-supplied-p 
			       start-from-beat 
			       (find-downbeat performed-rhythm beat-period :strategy #'is-greater-rhythmic-period))))
      (clap-to-tactus-phase performed-rhythm (scaleogram rhythm-analysis) computed-tactus
			   :start-from-beat found-downbeat
			   :beat-multiple 1))))

(defun save-rhythm-and-claps (original-rhythm clap-at)
  "Writes out the rhythm and the handclaps to a scorefile"
  (save-scorefile (format nil "/Volumes/iDisk/Research/Data/Handclap Examples/~a.handclap.score" (name original-rhythm)) 
		  (list (nlisp::array-to-list (onsets-in-seconds original-rhythm)) 
			(nlisp::array-to-list (./ clap-at (* (sample-rate original-rhythm) 1d0))))
		  :instrument "midi"
		  :midi-channel 10
		  :key-numbers (list *low-woodblock* *closed-hi-hat*)
		  :description (format nil "Handclapping to ~a" (name original-rhythm))))

