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

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :multires-rhythm)
(use-package :nlisp)

(defclass salience-trace-rhythm (rhythm)
  ((onsets-time-signal :initarg :onsets-time-signal :accessor onsets-time-signal :initform (make-double-array '(1))))
  (:documentation "A subclass of rhythm that holds the full salience trace and the onset data."))

;; (setf ps (perceptual-salience-rhythm "/Volumes/iDisk/Research/Sources/OtherResearchers/UoP/AuditorySaliencyModel/ines1.saliency" "/Volumes/iDisk/Research/Sources/OtherResearchers/UoP/AuditorySaliencyModel/ines1.onsets" :weighted t))
;; (setf res1 (perceptual-onsets-to-rhythm "res1/res1_1_resp_text" "res1/res1_1_pOnsets" :weighted t))

(defmethod plot-rhythm ((rhythm-to-plot salience-trace-rhythm) &key (reset t) (time-in-seconds nil))
  (if reset (reset-plot))
  (if time-in-seconds
      (plot-command "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
		    (label-samples-as-seconds (duration-in-samples rhythm-to-plot)
					      (sample-rate rhythm-to-plot))))
  (nplot (list (.* (onsets-time-signal rhythm-to-plot) (time-signal rhythm-to-plot)) 
	       (time-signal rhythm-to-plot))
	 ;; TODO need to have different x axes for each y parameters in the list.
	 ;; (list (onsets-in-samples rhythm-to-plot) (.iseq 0 (duration-in-samples rhythm-to-plot))) 
	 nil
	 :aspect-ratio 0.2 
	 ;; :styles '("lines" "impulses linetype 3")
	 :styles '("points pointtype 3" "lines linetype 3")
	 :legends '("Onsets" "Saliency")
	 :reset nil
	 :title (format nil "Salience trace and onsets of ~a" (name rhythm-to-plot))))

;; (plot-rhythm res1)
;; (plot (time-signal res1) nil :aspect-ratio 0.66)
;; (plot-cwt salience-scaleogram :title "res(1,1) continuous salience")

;; (intervals-in-samples (nlisp::array-to-list (.column perceptual-onsets 0)) :sample-rate 100)
;; (onsets-to-grid (nlisp::array-to-list (.floor (.column perceptual-onsets 0))))

(defun clap-to-salience-rhythm-files (saliency-path onsets-path original-sound-path accompaniment-sound-path
				      &key 
				      (sample-rate 200)
				      (start-from-beat 0) 
				      (beat-multiple 1))
  (let* ((salience-trace-rhythm (perceptual-salience-rhythm saliency-path onsets-path
							    :weighted nil :sample-rate sample-rate))
	 (salience-trace-claps (if (/= beat-multiple 1)
				   (clap-to-rhythm salience-trace-rhythm 
						   :start-from-beat start-from-beat
						   :beat-multiple beat-multiple
						   :tactus-selector #'create-weighted-beat-ridge)
				   (clap-to-rhythm salience-trace-rhythm 
						   :start-from-beat start-from-beat
						   :tactus-selector #'create-weighted-beat-ridge)))
;;					       :tactus-selector #'create-beat-ridge)))
;; Not used:
;;					       :tactus-selector #'create-weighted-windowed-beat-ridge))
;;					       :tactus-selector #'select-longest-lowest-tactus)))
;;					       :tactus-selector #'select-tactus-by-beat-multiple)))
;;					       :tactus-selector #'create-bar-ridge)))
;;					       :tactus-selector #'create-beat-multiple-ridge)))
	 ;; TODO gotta be a better way for division than making it a double-float.
	 (clap-times-in-seconds (./ salience-trace-claps (* 1d0 (sample-rate salience-trace-rhythm)))))
    (diag-plot 'perceptual-salience
      (plot-rhythm salience-trace-rhythm))
    (diag-plot 'onsets ;; Plot the salience trace, the produced onsets, & zero phase.
      (plot-claps salience-trace-rhythm 
		  (.find (onsets-time-signal salience-trace-rhythm))
		  ;; empty phase
		  (make-double-array (duration-in-samples salience-trace-rhythm))
		  :signal-name (format nil "perceptual onsets plot of ~a" (name salience-trace-rhythm))))

    ;; TODO Should have a save-rhythm-and-claps method specialised on
    ;; salience-trace-rhythm that uses the weighted onsets rhythm so that we know the
    ;; onsets are as the detector computes them.
    ;; (save-rhythm-and-claps salience-trace-rhythm salience-trace-claps)
    ;; (format t "Wrote rhythm as scorefile ~a~%" ) ; TODO
    ;; just write out the claps as a scorefile alone:
    ;; (save-rhythm (make-instance 'rhythm salience-trace-claps))
    (format t "Beat times of ~a in seconds:~%~a~%" (name salience-trace-rhythm) clap-times-in-seconds)
    (format t "Tempo estimate: ~,2f BPM~%" (tempo-from-claps salience-trace-claps (sample-rate salience-trace-rhythm)))
    ;; Make the perceptual onset detector results audible
    (save-rhythm-mix accompaniment-sound-path original-sound-path clap-times-in-seconds
		     :clap-sample-file #P"/Volumes/iDisk/Research/Data/Handclap Examples/cowbell.aiff")
    (format t "Wrote mix as soundfile ~a~%" accompaniment-sound-path)))

;;; TODO This should be replaced when we integrate Plymouth's onsets thresholding function
;;; into our code.
;; (defmethod onset-time-of-note ((rhythm salience-trace-rhythm) note-numbers)
;;   "Returns the sample number of the beat-number'th beat in the given rhythm"
;;   (let* ((onsets-rhythm (make-instance 'rhythm 
;; 				       :time-signal (onsets-time-signal rhythm)
;; 				       :sample-rate (sample-rate rhythm))))
;;     (onset-time-of-note onsets-rhythm note-numbers)))

(defmethod onset-time-of-note ((rhythm-to-analyse salience-trace-rhythm) (note-numbers n-array))
  "Returns the sample number of the beat-number'th beat in the given rhythm"
  (.arefs (.find (onsets-time-signal rhythm-to-analyse)) note-numbers))

(defmethod onset-time-of-note ((rhythm-to-analyse salience-trace-rhythm) note-number)
  "Returns the sample number of the beat-number'th beat in the given rhythm"
  (.aref (.find (onsets-time-signal rhythm-to-analyse)) note-number))

(defmethod onsets-in-samples ((rhythm-to-analyse salience-trace-rhythm))
  (.find (onsets-time-signal rhythm-to-analyse)))

(defun remove-double-onsets (rhythm onset-times &key (filter-window-in-seconds 0.05))
  "Remove all but the highest valued onsets that fall within the filter-window (specified in samples)"
  (let* ((filter-window-in-samples (* filter-window-in-seconds (sample-rate rhythm)))
	 ;; Find IOIs below the threshold.
	 (within-window (.find (.< (.diff onset-times) filter-window-in-samples))))
    (if (zerop (.length within-window))	; Exit out when within-window is empty.
	onset-times
	;; Determine the reliability (amplitude) of the within window onset candidates and the
	;; following candidates.
	(let* ((reliability-within-window (.arefs (time-signal rhythm) (.arefs onset-times within-window)))
	       (next-reliability-within-window (.arefs (time-signal rhythm) (.arefs onset-times (.+ within-window 1))))
	       ;; Remove whichever is less. The inequality operator will return a binary increment value.
	       (first-are-greater (.> reliability-within-window next-reliability-within-window))
	       ;; Form the array of indices of onset-times to remove.
	       (to-remove (.+ within-window (.floor first-are-greater))))
	  (remove-arefs onset-times to-remove)))))

;;; TODO Perhaps we are beginning to reach the limit of the representation, where we are swapping
;;; the discrete time values for the onsets. Probably the solution is to factor the
;;; rerieval methods so that onsets are more clearly separated from a rhythm time signal.
(defmethod subset-of-rhythm ((rhythm-to-subset salience-trace-rhythm) time-region)
  "Subset the onsets-time-signal as well as the time-signal"
  (let ((subset-rhythm (call-next-method rhythm-to-subset time-region)))
    (setf (onsets-time-signal subset-rhythm) (.subarray (onsets-time-signal rhythm-to-subset) (list 0 time-region)))
    subset-rhythm))

(defmethod onsets-of-salience ((odf-rhythm salience-trace-rhythm))
  "Determine the onset times from the salience trace"
  (let* (;; (a (analysis-of-rhythm odf-rhythm))
	 ;; (npc (normalised-phase-congruency a)) ; if we include phase congruency to filter the ODF-RHYTHM.
	 ;; (filtered-odf (.* (normalise (time-signal odf-rhythm)) npc))
	 (filtered-odf (time-signal odf-rhythm))
	 (onset-signal (.* (significant filtered-odf :threshold 0.75) (extrema-points-vector filtered-odf)))
	 (onset-times (.find onset-signal)))
    (diag-plot 'onset-signal 
      (nplot (list (normalise filtered-odf) onset-signal) nil :aspect-ratio 0.2))
    (remove-double-onsets odf-rhythm onset-times)))
    ;; onset-times))


(defun rhythm-from-odf (salience-filepath &key (sample-rate 200.0d0) (description "") (weighted t))
  "Reads the onset detection function (perceptual salience) data and returns a salience-trace-rhythm instance. Weighted keyword
   produces onsets which are weighted by relative values of the saliency measure."
  (let* ((perceptual-salience-matrix (.load salience-filepath :format :text))
	 (perceptual-salience (.column perceptual-salience-matrix 0))
	 (perceptual-salience-rhythm
	  ;; Even though we have assumed rhythm is a set of dirac fns, we can cheat a bit.
	  (make-instance 'salience-trace-rhythm 
			 :name (pathname-name salience-filepath)
			 :description description
			 :time-signal perceptual-salience
			 :onsets-time-signal (make-double-array (.length perceptual-salience))
			 :sample-rate sample-rate))
	 (onset-indices (onsets-of-salience perceptual-salience-rhythm)))
    ;; Assign the onsets from the computed times derived from the salience trace.
    (setf (.arefs (onsets-time-signal perceptual-salience-rhythm) onset-indices)
	  (if weighted
	      (.arefs perceptual-salience onset-indices)
	      (make-double-array (.length onset-indices) :initial-element 1d0)))
    perceptual-salience-rhythm))

(defun perceptual-salience-rhythm (salience-filepath onsets-filepath &key 
				      (sample-rate 200) (description "") (weighted t))
  "Reads the salience data and returns a salience-trace-rhythm instance. Weighted keyword
   produces onsets which are weighted by relative values of the saliency measure."
  (let* ((perceptual-salience-matrix (.load salience-filepath :format :text))
	 (perceptual-onsets (.load onsets-filepath :format :text))
	 (perceptual-salience (.column perceptual-salience-matrix 0))
	 ;; Assumes onset times are in seconds, converts to samples
	 (onset-times (.floor (.* (.column perceptual-onsets 0) sample-rate)))
	 ;; TODO perhaps rhythm-of-weighted-onsets can be used instead?
	 (onsets-time-signal (make-double-array (.length perceptual-salience))))
    (setf (.arefs onsets-time-signal onset-times)
	  (if weighted
	      (.column perceptual-onsets 1)
	      (make-double-array (.length onset-times) :initial-element 1.0d0)))
    ;; Even though we have assumed rhythm is a set of dirac fns, we can cheat a bit.
    (make-instance 'salience-trace-rhythm 
		   :name (pathname-name salience-filepath)
		   :description description
		   :time-signal perceptual-salience
		   :onsets-time-signal onsets-time-signal
		   :sample-rate sample-rate)))
