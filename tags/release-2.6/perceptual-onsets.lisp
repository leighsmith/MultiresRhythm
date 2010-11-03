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

(defclass salience-trace-rhythm (rhythm)
  ((onsets-time-signal :initarg :onsets-time-signal :accessor onsets-time-signal :initform (make-double-array '(1))))
  (:documentation "A subclass of rhythm that holds the full salience trace and the onset data."))

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
	 (binary-grid (make-narray (onsets-to-grid (nlisp::array-to-list onset-times))))
	 (weighted-grid (.* binary-grid 1d0)))
    (setf (.arefs weighted-grid onset-times) (.column perceptual-onsets 1))
    ;; Even though we have assumed rhythm is a set of dirac fns, we can cheat a bit.
    (make-instance 'salience-trace-rhythm 
		   :name (pathname-name salience-filepath)
		   :description description
		   :time-signal perceptual-salience
		   :onsets-time-signal (if weighted weighted-grid binary-grid)
		   :sample-rate sample-rate)))

;; (setf ps (perceptual-salience-rhythm "/Volumes/iDisk/Research/Sources/OtherResearchers/UoP/AuditorySaliencyModel/ines1.saliency" "/Volumes/iDisk/Research/Sources/OtherResearchers/UoP/AuditorySaliencyModel/ines1.onsets" :weighted t))
;; (setf res1 (perceptual-onsets-to-rhythm "res1/res1_1_resp_text" "res1/res1_1_pOnsets" :weighted t))

(defmethod plot-rhythm ((rhythm-to-plot salience-trace-rhythm) &key (reset t))
  ;; TODO add (onsets-time-signal rhythm-to-plot) when we can plot different sized signals.
  (nplot (list (time-signal rhythm-to-plot)) nil 
	:aspect-ratio 0.66 
	:styles '("lines" "impulses") 
	:reset reset
	:title (format nil "salience trace of ~a" (name rhythm-to-plot))))

;; (plot-rhythm res1)
;; (plot (time-signal res1) nil :aspect-ratio 0.66)
;; (plot-cwt salience-scaleogram :title "res(1,1) continuous salience")

;; (intervals-in-samples (nlisp::array-to-list (.column perceptual-onsets 0)) :sample-rate 100)
;; (onsets-to-grid (nlisp::array-to-list (.floor (.column perceptual-onsets 0))))

(defun clap-to-salience-rhythm-files (saliency-path onsets-path original-sound-path accompaniment-sound-path
				    &key 
				    (start-from-beat 0) 
				    (beat-multiple 1 multiple-supplied-p))
  (let* ((salience-trace-rhythm (perceptual-salience-rhythm saliency-path onsets-path :weighted nil))
	 (salience-trace-claps (if multiple-supplied-p 
				   (clap-to-rhythm salience-trace-rhythm 
						   :start-from-beat start-from-beat
						   :beat-multiple beat-multiple
						   :tactus-selector #'create-beat-ridge) ; TODO we use unweighted ridges!
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
    (format t "Beat times of ~a in seconds:~%~a~%" (name salience-trace-rhythm) clap-times-in-seconds)
    ; (save-rhythm salience-trace-claps)	; just write out the claps as a scorefile alone.
    (save-rhythm-mix accompaniment-sound-path original-sound-path clap-times-in-seconds)
    (format t "Wrote mix as soundfile ~a~%" accompaniment-sound-path)))

(defun compute-perceptual-versions (salience-filename onsets-filename original-sound-filename 
				    &key 
				    (start-from-beat 0) 
				    (beat-multiple 1)
				    (data-directory "/Volumes/iDisk/Research/Data/PerceptualOnsets/"))
  (let* ((original-sound-path (make-pathname :directory (list :absolute data-directory)
					     :name original-sound-filename
					     :type "wav"))
	 (accompaniment-sound-path (make-pathname :directory "/Volumes/iDisk/Research/Data/Handclap Examples"
						  :name (concatenate 'string original-sound-filename "_mixed")
						  :type "wav"))
	 (saliency-path (make-pathname :directory (list :absolute data-directory) 
				       :name salience-filename
				       :type "saliency"))
	 (onsets-path (make-pathname :directory (list :absolute data-directory) 
				     :name onsets-filename
				     :type "onsets")))
    (clap-to-salience-rhythm-files saliency-path onsets-path original-sound-path accompaniment-sound-path
				   :start-from-beat start-from-beat
				   :beat-multiple beat-multiple)))

;;; TODO This should be replaced when we integrate Plymouth's onsets thresholding function
;;; into our code.
(defmethod onset-time-of-note ((rhythm salience-trace-rhythm) note-numbers)
  "Returns the sample number of the beat-number'th beat in the given rhythm"
  (let* ((onsets-rhythm (make-instance 'rhythm 
				       :time-signal (onsets-time-signal rhythm)
				       :sample-rate (sample-rate rhythm))))
    (onset-time-of-note onsets-rhythm note-numbers)))


(defun pOnset-times (onsets-filename)
  (let* ((data-directory "/Volumes/iDisk/Research/Data/PerceptualOnsets/")
	 (perceptual-onsets (.load (make-pathname :directory (list :absolute data-directory)
						  :name onsets-filename
						  :type "mat") :format :octave)))
    (.column perceptual-onsets 0)))


;;; Make the perceptual onset detector results audible
(defun mix-pOnsets-with-sound (original-sound-filename)
  (let* ((original-sound-path (make-pathname :directory "/Volumes/iDisk/Research/Data/PerceptualOnsets/"
					     :name original-sound-filename
					     :type "wav"))
	 (accompaniment-sound-path (make-pathname :directory "/Volumes/iDisk/Research/Data/Handclap Examples"
						  :name (concatenate 'string original-sound-filename "_pOnsets_mixed")
						  :type "wav"))
	 (onset-times-in-seconds (pOnset-times (concatenate 'string original-sound-filename "_pOnsets_text"))))
    (save-rhythm-mix accompaniment-sound-path original-sound-path onset-times-in-seconds)))

