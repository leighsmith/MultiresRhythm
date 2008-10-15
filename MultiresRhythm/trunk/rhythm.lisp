;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Rhythm class and utility methods.
;;;;
;;;; Leigh M. Smith <lsmith@science.uva.nl>
;;;;
;;;; Copyright (c) 2006, 2007 All Rights Reserved.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; See multiresrhythm.asd for further info.
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

;;;; Declarations

;;; TODO Do we need a distinction between a description and a name, if we use the name of
;;; the variable pointing to the rhythm object anyway?
(defclass rhythm ()
  ((name        :initarg :name        :accessor name        :initform "unnamed")
   (description :initarg :description :accessor description :initform "")
   (time-signal :initarg :time-signal :accessor time-signal :initform (make-double-array '(1)))
   (sample-rate :initarg :sample-rate :accessor sample-rate :initform 200))
  (:documentation "A rhythm includes a textual description, the sample rate and a signal. This allows representing a rhythm as a continuum between a signal processing representation (high sample rate) and a symbolic representation
(low sample rate)."))

(defgeneric duration (rhythm-to-analyse)
  (:documentation "Returns the length of the rhythm in seconds."))

;; (defgeneric duration-in-samples (rhythm-to-analyse)
;;   (:documentation "Returns the length of the rhythm in samples."))

(defgeneric onsets-in-samples (rhythm-to-analyse)
  (:documentation "Returns the sample indexes of each onset."))

(defgeneric onsets-in-seconds (rhythm-to-analyse)
  (:documentation "Returns the location of each onset in seconds."))

(defgeneric onset-time-of-note (rhythm-to-analyse note-numbers)
  (:documentation "Returns the sample number of the note-number'th note in the given rhythm"))

(defgeneric last-onset-time (rhythm-to-analyse)
  (:documentation "Returns the sample number of the last note in the given rhythm"))

(defgeneric rhythm-iois (rhythm-to-analyse)
  (:documentation "Given a rhythm, returns IOIs specified in seconds."))

(defgeneric rhythm-iois-samples (rhythm-to-analyse)
  (:documentation "Given a rhythm, returns IOIs specified in samples."))

;;; TODO should it become save-to-file?
(defgeneric save-rhythm (rhythm-to-save &key directory)
  (:documentation "Writes the rhythm to a MusicKit scorefile."))

(defgeneric plot-rhythm (rhythm-to-plot &key reset time-in-seconds)
  (:documentation "Plot locations of notes of the given rhythm."))

(defgeneric scale-amplitude (rhythm-to-scale scale-factor)
  (:documentation "Scales the amplitude of each onset by the given scale-factor."))

(defgeneric sound-of (rhythm-to-sonify clap-pathname)
  (:documentation "Returns a sound instance of the rhythm."))

(defgeneric write-as-audio (rhythm-to-save filepath-to-save clap-pathname)
  (:documentation "Write the rhythm to the filename, using the clapping sound."))

(defgeneric hear (rhythm-to-hear)
  (:documentation "Convenience function to make a rhythm audible"))

;;;; Implementation

(defmethod print-object ((rhythm-to-print rhythm) stream)
  (call-next-method rhythm-to-print stream) ;; to print the superclass.
  (format stream " ~a ~a sample-rate ~f" 
	  (name rhythm-to-print) (description rhythm-to-print) (sample-rate rhythm-to-print)))

(defun intervals-in-samples (intervals &key ((:tempo tempo-in-bpm) 60)
			     (ioi 1.0 interval-supplied-p)
			     (sample-rate 1000)) ; in Hz (samples per second)
  "Converts a given rhythm (in relative interval values, 1.0 = the shortest interval) to
   samples, defaulting to milliseconds for 1000Hz."
  (let* ((shortest-interval-samples (if interval-supplied-p ioi (/ (* 60 sample-rate) tempo-in-bpm))))
    (mapcar #'(lambda (interval) (round (* shortest-interval-samples interval))) intervals)))

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
     for note in grid
     counting note into ioi
     when (not (zerop note))
     collect (1- ioi)))

(defun onsets-to-iois (onsets)
  "From a set of onset times, returns the interonset intervals, the first derivative thereof"
  (if (null (second onsets))
      nil
      (cons (- (second onsets) (first onsets)) (onsets-to-iois (rest onsets)))))

(defun repeat-rhythm (rhythm repeat)
  (if (> repeat 0)
      (append rhythm (repeat-rhythm rhythm (1- repeat)))))

;; TODO This could be distinct, at the moment we are assuming the 'time' of the note is the
;; same as it's onset time.
;; (defun onset-time-of-note (rhythm note-numbers)

(defun threshold-rhythm (rhythm &key (threshold 0.75d0))
  "Returns a binary valued rhythm based on the threshold"
  (make-instance 'rhythm
		 :name (name rhythm)
		 :time-signal (.> (time-signal rhythm) threshold)
		 :description ""
		 :sample-rate (sample-rate rhythm)))

(defun iois-to-rhythm (name iois &key (shortest-ioi 1.0) (sample-rate 200) 
		       ((:tempo tempo-in-bpm) 60 tempo-supplied-p))
  "Returns a rhythm instance given a list of inter-onset intervals"
  (make-instance 'rhythm 
		 :name name
		 :description name ; TODO transliterate '-' for ' '.
		 :time-signal (.* (make-narray 
				   (butlast 
				    (onsets-to-grid 
				     ;; We compute the intervals in samples after the onsets to avoid
				     ;; the propagating error from truncation that occurs in intervals-in-samples.
				     (if tempo-supplied-p
					 (intervals-in-samples (iois-to-onsets iois) 
							       :sample-rate sample-rate
							       :tempo tempo-in-bpm)
					 (intervals-in-samples (iois-to-onsets iois) 
							       :sample-rate sample-rate
							       :ioi shortest-ioi))))) 1.0d0)
		 :sample-rate sample-rate))

(defun rhythm-of-onsets (name onsets &key (sample-rate 200))
  "Given an narray of onsets, creates a rhythm instance"
  (make-instance 'rhythm 
		 :name name
		 :description name ; TODO transliterate '-' for ' '.
		 :time-signal (.* 1d0 (make-narray (onsets-to-grid (nlisp::array-to-list (.round (.* onsets sample-rate))))))
		 :sample-rate sample-rate))

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

(defun rhythm-of-weighted-onsets (name note-list &key (sample-rate 200))
  "Accepts a list of lists, each containing a time and a normalised intensity"
  (let* ((times (mapcar (lambda (time) (round (* sample-rate (first time)))) note-list))
	 (amp (make-narray (mapcar #'second note-list)))
	 (time-sig (make-double-array (1+ (first (last times))))))
    (setf (.arefs time-sig (make-narray times)) amp)
    (make-instance 'rhythm 
		   :name name
		   :description name
		   :time-signal time-sig
		   :sample-rate sample-rate)))

;; (clap-signal (make-double-array (.array-dimensions rhythm-signal) :initial-element 0d0))
;; (map nil (lambda (index) (setf (.aref clap-signal index) max-computed-scale)) (val onsets))

(defmethod duration-in-samples ((rhythm-to-analyse rhythm))
  (.length (time-signal rhythm-to-analyse)))

(defmethod duration ((rhythm-to-analyse rhythm))
  (/ (duration-in-samples rhythm-to-analyse) (.* 1.0 (sample-rate rhythm-to-analyse))))

(defmethod onsets-in-samples ((rhythm-to-analyse rhythm))
  (.find (time-signal rhythm-to-analyse)))

(defmethod onset-time-of-note ((rhythm-to-analyse rhythm) note-number)
  "Returns the sample number of the note-number'th note in the given rhythm"
  (let* ((note-positions (onsets-in-samples rhythm-to-analyse)))
    (.aref note-positions note-number)))

(defmethod onset-time-of-note ((rhythm-to-analyse rhythm) (note-numbers n-array))
  "Returns the sample number of the note-number'th note in the given rhythm"
  (let* ((note-positions (onsets-in-samples rhythm-to-analyse)))
    (.arefs note-positions note-numbers)))

(defmethod number-of-onsets ((rhythm-to-analyse rhythm))
  "Returns the number of onsets in the rhythm"
  (.length (onsets-in-samples rhythm-to-analyse)))

(defmethod last-onset-time ((rhythm-to-analyse rhythm))
  "Returns the sample number of the last note in the given rhythm"
  (let* ((note-positions (onsets-in-samples rhythm-to-analyse)))
    (.aref note-positions (1- (number-of-onsets rhythm-to-analyse)))))

(defmethod onsets-in-seconds ((rhythm-to-analyse rhythm))
  (./ (onsets-in-samples rhythm-to-analyse) (* (sample-rate rhythm-to-analyse) 1d0)))

(defmethod rhythm-iois ((rhythm-to-analyse rhythm))
  (.diff (onsets-in-seconds rhythm-to-analyse)))

(defmethod rhythm-iois-samples ((rhythm-to-analyse rhythm))
  (.diff (onsets-in-samples rhythm-to-analyse)))

(defmethod intervals-as-ratios ((rhythm-to-analyse rhythm))
  "Return the IOIs of the rhythm as ratios to the shortest interval"
  (let* ((iois-in-samples (rhythm-iois-samples rhythm-to-analyse)))
    (./ iois-in-samples (.min iois-in-samples) 1d0)))

(defmethod plot-rhythm ((rhythm-to-plot rhythm) &key 
			(reset t)
			(time-in-seconds nil)
			(aspect-ratio 0.1))
  (if reset (reset-plot))
  ;; "set size 0.7,0.7"
  ;; "set origin 0.1,0.1"
  (plot-command "set xtics font \"Times,10\"")
  (plot-command "set ytics font \"Times,10\"")
  (if time-in-seconds
      (plot-command "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
		    (label-samples-as-seconds (duration-in-samples rhythm-to-plot)
					      (sample-rate rhythm-to-plot))))
  (plot-command "set xrange [0:~d]" (duration-in-samples rhythm-to-plot)) ; ensures last label plotted
  (plot-command "set yrange [0:1.2]")	; Gives some height for the key.
  (plot-command "set format y \"%3.1f\"")
  (plot-command "set ytics 0.0, 0.20, 1.0")
  (plot-command "set size 0.83,0.35")
  (plot-command "set origin 0.055,0.65")
  (plot-command "unset key")
  (plot (time-signal rhythm-to-plot) nil
	;; :label (format nil "Rhythm onsets of ~a" (description rhythm-to-plot))
	:style "impulses linetype 6"
	:xlabel (if time-in-seconds "Time in seconds"
		    (format nil "Time in samples (~dHz sample rate)" (sample-rate rhythm-to-plot)))
	:ylabel "Normalised Intensity"
	:title (format nil "Rhythm of ~a" (name rhythm-to-plot))
	:aspect-ratio aspect-ratio
	:reset nil))

;;; TODO Should make this a method. Use window dimensions?
(defun rhythm-plot (rhythm-to-plot window-dimensions title &key 
		    (time-in-seconds t)
		    (aspect-ratio 0.1))
  (plot-command "set xrange [0:~d]" (duration-in-samples rhythm-to-plot)) ; ensures last label plotted
  (plot-command "set yrange [0:1.2]")	; Gives some height for the key.
  (plot-command "set format y \"%3.1f\"")
  (plot-command "set ytics 0.0, 0.20, 1.0")
  (plot-command "unset key")
  (plot (time-signal rhythm-to-plot) nil
	;; :label (format nil "Rhythm onsets of ~a" (description rhythm-to-plot))
	:style "impulses linetype 6"
	:xlabel (if time-in-seconds "Time in seconds"
		    (format nil "Time in samples (~dHz sample rate)" (sample-rate rhythm-to-plot)))
	:ylabel "Normalised Intensity"
	:title (format nil "Rhythm of ~a" title)
	:aspect-ratio aspect-ratio
	:reset nil))

(defun rhythm-of-part (name note-list &key 
		       (sample-rate 200) 
		       (attack 0.020) 
		       (release 0.020)
		       (make-envelope-p nil))
  "Generate a rhythm from a note-list specifying time, duration (both in seconds) & amplitude"
  (let* ((last-note (first (last note-list)))
	 (rhythm-length (ceiling (* (+ (first last-note) (second last-note)) sample-rate)))
	 (time-signal (make-double-array rhythm-length)))
    (loop
       for (time duration amplitude) in note-list
       for onset-time-in-samples = (round (* time sample-rate))
       for duration-samples = (round (* duration sample-rate))
       do (if make-envelope-p
	      (setf (.subarray time-signal (list 0 (list onset-time-in-samples 
							 (+ onset-time-in-samples duration-samples))))
		    (make-envelope amplitude :attack attack :release release :sustain (- duration attack release)))
	      (setf (.aref time-signal onset-time-in-samples) (coerce amplitude 'double-float))))
    (make-instance 'rhythm
		   :name name
		   :description name
		   :time-signal time-signal
		   :sample-rate sample-rate)))

(defun part-from-rhythm (rhythm &key (fixed-duration 0.25) (fixed-key-number *low-woodblock*)
			 (durations (make-list (number-of-onsets rhythm) :initial-element fixed-duration))
			 (key-numbers (make-list (number-of-onsets rhythm) :initial-element fixed-key-number)))
  "Returns a part created from a rhythm with a key number and duration list"
  (map 'list (lambda (onset-time-seconds duration key-number) 
	       (list onset-time-seconds 
		     duration
		     1.0 ; hacked for now
		     key-number))
       (nlisp::array-to-list (onsets-in-seconds rhythm)) durations key-numbers))

(defmethod save-rhythm ((rhythm-to-save rhythm) &key (directory "/Users/leigh/"))
  (let ((scorefile-pathname (make-pathname :directory directory :name (name rhythm-to-save) :type "score")))
    (save-scorefile scorefile-pathname
		    (part-from-rhythm rhythm-to-save 
				      :fixed-duration 0.25 ; make sufficient so QuickTime Player doesn't clip a MIDI version.
				      :fixed-key-number *low-woodblock*)
		    :instrument "midi"
		    :midi-channels (list 10)
		    :description (description rhythm-to-save))
    scorefile-pathname))

(defmethod sound-of ((rhythm-to-sonify rhythm) (clap-pathname pathname))
  "Create a sound instance from the rhythm"
  (let* ((clap-sound (sound-from-file clap-pathname))
	 (length-of-sound (round (+ (* (duration rhythm-to-sonify) (sample-rate clap-sound))
				    (sound-length clap-sound))))
	 (note-times (onsets-in-seconds rhythm-to-sonify))
	 (note-amplitudes (.arefs (time-signal rhythm-to-sonify) (onsets-in-samples rhythm-to-sonify))))
    (sample-at-times length-of-sound clap-sound note-times :amplitudes note-amplitudes)))

(defmethod write-as-audio ((rhythm-to-save rhythm) (filepath-to-save pathname) (clap-pathname pathname))
  "Write the given rhythm as an audio file"
  (save-to-file (sound-of rhythm-to-save clap-pathname) filepath-to-save))

(defmethod hear ((rhythm-to-hear rhythm))
  "Convenience function to make a rhythm audible"
  (write-as-audio rhythm-to-hear 
		  (make-pathname :directory "/Volumes/iDisk/Research/Data/Handclap Examples/"
				 :name (name rhythm-to-hear)
				 :type "wav")
		  #P"/Volumes/iDisk/Research/Data/Handclap Examples/cowbell.aiff"))

(defun add-rhythm (&rest rhythms-to-add)
  "Adds multiple rhythms together. Returns the shortest? longest? rhythm."
  (let* ((added-rhythms (apply #'.+ (mapcar #'time-signal rhythms-to-add)))
	(clamped-rhythms (clamp-to-bounds added-rhythms added-rhythms :high-bound 1.0d0 :clamp-high 1.0d0)))
    (make-instance 'rhythm 
		   :name "polyrhythm" 
		   :description "polyrhythm"
		   :time-signal clamped-rhythms
		   :sample-rate 200)))

(defmethod scale-amplitude ((rhythm-to-scale rhythm) scale-factor)
  "Scales the amplitude of each onset by the given scale-factor"
  (setf (time-signal rhythm-to-scale) (.* (time-signal rhythm-to-scale) scale-factor))
  rhythm-to-scale)

(defmethod subset-of-rhythm ((rhythm-to-subset rhythm) time-region)
  "Returns a new rhythm instance with a sub-region of the given rhythm. The time region
  consists of the start and end specifiers. T defaults to the start or finish."
    (make-instance 'rhythm 
		   :name (name rhythm-to-subset) 
		   :description (format nil "subset of ~a" (name rhythm-to-subset))
		   :time-signal (.subarray (time-signal rhythm-to-subset) (list 0 time-region))
		   :sample-rate (sample-rate rhythm-to-subset)))

;;; TODO this should also allow limitation by the number of notes, number of bars etc.
(defmethod limit-rhythm ((rhythm-to-limit rhythm) &key maximum-samples)
  "Returns a rhythm that is bounded in it's length to a maximum number of samples"
  (if (< (duration-in-samples rhythm-to-limit) maximum-samples)
      rhythm-to-limit	; under maximum, no change
      (subset-of-rhythm rhythm-to-limit (list 0 (1- maximum-samples)))))

;;; A dummy method to keep the plotting happy. For now...
(defmethod .decimate ((rhythm-to-decimate rhythm) reduce-list &key start-indices)
  (declare (ignore reduce-list start-indices))
  rhythm-to-decimate)

(defun append-rhythm (&rest rhythms)
  "Append each rhythm onto the first returning a new instance, assumes all the sample-rates are equal."
  (make-instance 'rhythm 
		 :name (name (first rhythms)) 
		 :description (name (first rhythms))
		 :time-signal (apply #'.concatenate (mapcar #'time-signal rhythms))
		 :sample-rate (sample-rate (first rhythms))))
