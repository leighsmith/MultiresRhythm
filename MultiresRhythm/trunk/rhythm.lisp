;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Rhythm class and utility methods.
;;;;
;;;; Leigh Smith <lsmith@science.uva.nl>
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

;;;; Declarations

;;; A rhythm includes a textual description, the sample rate and a general description of
;;; a signal. This allows representing a rhythm as a continuum between
;;; a signal processing representation (high sample rate) and a symbolic representation
;;; (low sample rate).
(defclass rhythm ()
  ((name        :initarg :name        :accessor name        :initform "unnamed")
   (description :initarg :description :accessor description :initform "")
   (time-signal :initarg :time-signal :accessor time-signal :initform (make-double-array '(1)))
   (sample-rate :initarg :sample-rate :accessor sample-rate :initform 200)))

(defgeneric duration (rhythm-to-analyse)
  (:documentation "Returns the length of the rhythm in seconds."))

(defgeneric sample-length (rhythm-to-analyse)
  (:documentation "Returns the length of the rhythm in samples."))

(defgeneric rhythm-iois (rhythm-to-analyse)
  (:documentation "Given a rhythm, returns IOIs specified in seconds."))

(defgeneric save-rhythm (rhythm-to-save)
  (:documentation "Writes the rhythm to a MusicKit scorefile."))

(defgeneric plot-rhythm (rhythm-to-plot)
  (:documentation "Plot locations of beats of the given rhythm."))

;;;; Implementation

(defun intervals-in-samples (intervals &key ((:tempo tempo-in-bpm) 60)
			     (ioi 1.0 interval-supplied-p)
			     (sample-rate 1000)) ; in Hz (samples per second)
  "Converts a given rhythm (in relative interval values, 1.0 = the shortest interval) to
   samples, defaulting to milliseconds for 1000Hz."
  (let* ((shortest-interval-samples (if interval-supplied-p ioi (/ (* 60 sample-rate) tempo-in-bpm))))
    (mapcar #'(lambda (interval) (truncate (* shortest-interval-samples interval))) intervals)))

(defun iois-to-onsets (iois &optional (onset 0))
  (if iois
    (cons onset (iois-to-onsets (rest iois) (+ onset (first iois))))
    (list onset)))

;; (iois-to-onsets '(1 3 2)) -> (0 1 4 6)

;; TODO generate vector as well as list
(defun onsets-to-grid (onsets)
  "Returns the sample grid given a list of onsets"
  (loop with rhythm-grid = (make-list (1+ (first (last onsets))) :initial-element 0)
        for onset in onsets
        do (setf (elt rhythm-grid onset) 1)
        finally (return rhythm-grid)))

;; (onsets-to-grid '(0 1 4 6)) -> (1 1 0 0 1 0 1)

(defun grid-to-onsets (grid)
  "From a binary grid returns onset locations of non-zero values"
  (loop
     for beat in grid
     counting beat into ioi
     when (not (zerop beat))
     collect ioi))

(defun onsets-to-iois (onsets)
  "From a set of onset times, returns the interonset intervals, the first derivative thereof"
  (if (null (second onsets))
      nil
      (cons (- (second onsets) (first onsets)) (onsets-to-iois (rest onsets)))))

(defun repeat-rhythm (rhythm repeat)
  (if (> repeat 0)
      (append rhythm (repeat-rhythm rhythm (1- repeat)))))

(defun time-of-beat (rhythm beat-number)
  "Returns the sample number of the beat-number'th beat in the given rhythm"
  (let* ((beat-positions (.find (time-signal rhythm))))
    (.aref beat-positions beat-number)))

(defun iois-to-rhythm (name iois &key (shortest-ioi 1.0) (sample-rate 200))
  "Returns a rhythm instance given a list of inter-onset intervals"
    (make-instance 'rhythm 
		   :name name
		   :description name ; TODO transliterate '-' for ' '.
		   :time-signal (nlisp::list-to-array 
				 (butlast 
				  (onsets-to-grid 
				   (iois-to-onsets 
				    (intervals-in-samples iois :ioi shortest-ioi)))))
		   :sample-rate sample-rate))

(defun rhythm-of-onsets (name onsets &key (sample-rate 200))
  "Given an narray of onsets, creates a rhythm instance"
  (iois-to-rhythm name (nlisp::array-to-list (.diff onsets)) :sample-rate sample-rate))

(defun rhythm-of-grid (name grid &key (tempo 80 tempo-supplied-p)
				(shortest-ioi 1.0)
				(sample-rate 200))
  "Given a rhythmic grid list, returns a rhythm instance"
  (if tempo-supplied-p (setf shortest-ioi (/ (* sample-rate 60) tempo)))
  (iois-to-rhythm name 
		  ;; Append an impulse to ensure trailing silences are counted.
		  (onsets-to-iois (grid-to-onsets (append grid '(1))))
		  :shortest-ioi shortest-ioi
		  :sample-rate sample-rate))

;; (clap-signal (make-double-array (.array-dimensions rhythm-signal) :initial-element 0d0))
;; (map nil (lambda (index) (setf (.aref clap-signal index) max-computed-scale)) (val onsets))

(defmethod sample-length ((rhythm-to-analyse rhythm))
  (.length (time-signal rhythm-to-analyse)))

(defmethod duration ((rhythm-to-analyse rhythm))
  (/ (sample-length rhythm-to-analyse) (.* 1.0 (sample-rate rhythm-to-analyse))))

(defmethod rhythm-iois ((rhythm-to-analyse rhythm))
  (./ (.* 1d0 (.diff (.find (time-signal rhythm-to-analyse))))
      (sample-rate rhythm-to-analyse)))

(defmethod plot-rhythm ((rhythm-to-plot rhythm))
  (plot (time-signal rhythm-to-plot) nil
	 :label (format nil "Rhythm onsets of ~a" (description rhythm-to-plot))
	 :style "impulses linetype 6"
	 :xlabel "Time"
	 :ylabel "Scaled Intensity"
	 :title (format nil "Rhythm of ~a" (name rhythm-to-plot))
	 :aspect-ratio 0.66))

(defmethod save-rhythm ((rhythm-to-save rhythm))
  (save-scorefile (format nil "/Users/leigh/~a.score" (name rhythm-to-save)) 
		  ;; Convert to seconds.
		(nlisp::array-to-list (rhythm-iois rhythm-to-save))
		:instrument "midi"
		:midi-channel 10
		:description (description rhythm-to-save)))

