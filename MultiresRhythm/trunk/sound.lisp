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

(defmethod print-object ((sound-to-print sound) stream)
  (call-next-method sound-to-print stream) ;; to print the superclass.
  (let ((sound-array (sound-signal sound-to-print)))
    (format stream " frames ~a sample-rate ~a channels ~a"
	    (.column-count sound-array) (sample-rate sound-to-print) (.row-count sound-array))))

(defmethod sound-from-file ((path pathname))
  (multiple-value-bind (sound-signal sample-rate) (load-audio (namestring path))
    (make-instance 'sound 
		   :sound-signal sound-signal
		   :sample-rate sample-rate)))

(defmethod onset-times ((sound-to-analyse sound))
  (./ (.* (.find (sound-signal sound-to-analyse)) 1d0) (sample-rate sound-to-analyse)))

;; (setf noisy-dialog (load-audio "/Users/leigh/Research/Data/RoomTone/noisy_dialog.aiff"))
;; (setf noisy-dialog-initial (.subarray noisy-dialog '(0 (0 32767))))
;; (plot noisy-dialog-initial nil :aspect-ratio 0.2)
;; (setf noisy-dialog-scaleogram (cwt noisy-dialog-initial 8))
;; (plot-cwt noisy-dialog-scaleogram)
;; (plot-scale-energy-at-times noisy-dialog-scaleogram (list 2048 8192))
