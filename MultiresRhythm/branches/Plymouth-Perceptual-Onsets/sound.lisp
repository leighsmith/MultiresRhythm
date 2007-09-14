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
    (format stream " ~a frames ~a sample-rate ~a channels ~a"
	    (description sound-to-print) 
	    (.column-count sound-array)
	    (sample-rate sound-to-print)
	    (.row-count sound-array))))

(defmethod sound-from-file ((path pathname))
  (multiple-value-bind (sound-signal sample-rate) (load-audio path)
    (make-instance 'sound 
		   :description (namestring path)
		   :sound-signal sound-signal
		   :sample-rate sample-rate)))

(defmethod onset-times ((sound-to-analyse sound))
  (./ (.* (.find (sound-signal sound-to-analyse)) 1d0) (sample-rate sound-to-analyse)))

;;; Should be .length
(defmethod sound-length ((sound-to-analyse sound))
  (.length (sound-signal sound-to-analyse)))

;; TODO SHould accept arbitary # of sounds.
(defmethod sound-mix ((sound-to-mix1 sound) (sound-to-mix2 sound)) ; &rest sounds
  (make-instance 'sound
		 :sound-signal (.+ (sound-signal sound-to-mix1) (sound-signal sound-to-mix2))
		 :sample-rate (sample-rate sound-to-mix1))) ; TODO should test SRs are all equal.

		 
(defmethod save-to-file ((sound-to-save sound) (path pathname))
  (save-audio path (sound-signal sound-to-save) :sample-rate (sample-rate sound-to-save)))

;; (setf down-sampled (.floor (.* (onset-times jw-clicks) 200)))
;; (setf down-sampled-length (floor (* (.length (sound-signal jw-clicks)) (/ 200.0 44100.0))))
;; (setf down-sampled-rhythm (make-double-array down-sampled-length))
;; (setf (.arefs down-sampled-rhythm down-sampled) (make-double-array (.length down-sampled) :initial-element 1d0))
;; (plot down-sampled-rhythm nil)
;; (setf jw-prelude-3 (make-instance 'rhythm :time-signal down-sampled-rhythm :name "jw prelude 3"))



;; (setf noisy-dialog (load-audio #P"/Users/leigh/Research/Data/RoomTone/noisy_dialog.aiff"))
;; (setf noisy-dialog-initial (.subarray noisy-dialog '(0 (0 32767))))
;; (plot noisy-dialog-initial nil :aspect-ratio 0.2)
;; (setf noisy-dialog-scaleogram (cwt noisy-dialog-initial 8))
;; (plot-cwt noisy-dialog-scaleogram)
;; (plot-scale-energy-at-times noisy-dialog-scaleogram (list 2048 8192))
