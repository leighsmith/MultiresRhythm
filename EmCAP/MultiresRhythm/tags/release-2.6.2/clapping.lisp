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

(defgeneric clap-to-rhythm (rhythm-to-analyse &key tactus-selector start-from-beat beat-multiple)
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
	 (down-beat-sample (onset-time-of-note original-rhythm start-from-beat))
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
    (format t "Handclapping every ~d beats from beat ~d of original rhythm, sample ~d~%"
	    beat-multiple start-from-beat down-beat-sample)
    (diag-plot 'claps (plot-claps original-rhythm clap-at foot-tap-phase))
    clap-at))

(defun beat-multiple-for-clapping (tactus vpo sample-rate)
  "Compute a beat multiple we should clap at, based on the chosen tactus beat period compared to the preferred tempo"
  (let* ((preferred-beat-period (time-support (preferred-tempo-scale vpo sample-rate) vpo))
	 (tactus-beat-period (time-support (average-scale (first tactus)) vpo))) ; TODO using first is a hack
    (format t "Preferred clapping beat period ~,3f seconds actual tactus beat period ~,3f seconds, ratio ~,4f~%" 
	    (/ preferred-beat-period sample-rate)
	    (/ tactus-beat-period sample-rate)
	    (/ preferred-beat-period tactus-beat-period))
    ;; Establish a minimum of 1, since crazy tactus selectors can have round return 0
    ;; which freaks out division...
    (max 1 (round preferred-beat-period tactus-beat-period))))

(defmethod clap-to-rhythm ((performed-rhythm rhythm) &key 
			   (beat-multiple 1 multiple-supplied-p)
			   (start-from-beat 0 downbeat-supplied-p)
			   (tactus-selector #'select-longest-lowest-tactus))
  "Returns a set of sample times to clap to given the supplied rhythm"
  (multiple-value-bind (computed-tactus rhythm-analysis)
      (tactus-for-rhythm performed-rhythm :tactus-selector tactus-selector)
    (let* ((scaleogram (scaleogram rhythm-analysis))
	   ;; (beat-period (beat-period-of-rhythm performed-rhythm (skeleton rhythm-analysis)))
	   (beat-period (unweighted-beat-period-of-rhythm performed-rhythm scaleogram))
	   (found-downbeat (if downbeat-supplied-p 
			       start-from-beat 
			       (find-downbeat performed-rhythm beat-period :strategy #'is-greater-rhythmic-period)))
	   (clapping-beat-multiple (beat-multiple-for-clapping computed-tactus 
							       (voices-per-octave scaleogram) 
							       (sample-rate performed-rhythm))))
      (format t "Suggested beat multiple ~f~%" clapping-beat-multiple)
      (format t "Possible metrical divisor ~a~%" (meter-of-analysis rhythm-analysis computed-tactus))
      ;; TODO find-downbeat is inclined to crash horribly because of the comparison - must fix.
      ;; (format t "Found downbeat is ~d~%" (find-downbeat performed-rhythm beat-period :strategy #'is-greater-rhythmic-period))
      (clap-to-tactus-phase performed-rhythm scaleogram computed-tactus
			   :start-from-beat found-downbeat
			   :beat-multiple (if multiple-supplied-p beat-multiple clapping-beat-multiple)))))

;;; Needs to have remaining time 
(defun clap-to-iois (name iois &key (shortest-ioi (/ 120 17)))
  (clap-to-rhythm (iois-to-rhythm name iois :shortest-ioi shortest-ioi)))

(defun save-rhythm-and-claps (original-rhythm clap-at)
  "Writes out the rhythm and the handclaps to a scorefile"
  (save-scorefile (format nil "/Volumes/iDisk/Research/Data/Handclap Examples/~a.handclap.score" (name original-rhythm)) 
		  (list (nlisp::array-to-list (onsets-in-seconds original-rhythm)) 
			(nlisp::array-to-list (./ clap-at (* (sample-rate original-rhythm) 1d0))))
		  :instrument "midi"
		  :midi-channel 10
		  :key-numbers (list *low-woodblock* *closed-hi-hat*)
		  :description (format nil "Handclapping to ~a" (name original-rhythm))))

