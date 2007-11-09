;;;; -*- Lisp -*-
;;;; $Id$
;;;; Minimal rhythm generating functions. Can be played with play-timed-notes.
;;;;
;;;; Leigh M. Smith <lsmith@science.uva.nl>
;;;;

(in-package :cl-musickit)

;;; If a tempo parameter is not specified, a default of 60BPM is used, assuming a beat is
;;; an interval of 1.  If an IOI is instead not specified, a default of 1 second is used.
(defun note-list-of-rhythm-intervals (intervals &key ((:tempo tempo-in-bpm) 60)
				      (ioi 1.0 interval-supplied-p)
				      (pitch 60))
  "Plays a given rhythm (in relative interval values, 1.0 = the shortest interval) to the MIDI device."
  (let* ((shortest-interval-milliseconds (if interval-supplied-p ioi
					     (/ 60000 tempo-in-bpm)))
	 (intervals-milliseconds (mapcar #'(lambda (x) (truncate (* shortest-interval-milliseconds x))) intervals)))
    (loop 
       with start-note = (make-midi-note 0
					 0
					 :key-number pitch
					 :velocity 127
					 :duration (truncate shortest-interval-milliseconds 2))
       for interval in intervals-milliseconds
       for abs-time = interval then (+ abs-time interval)
       for note-tag = 1 then (1+ note-tag)
       collect (make-midi-note abs-time
			       note-tag
			       :key-number pitch
			       :velocity 127
			       :duration (truncate shortest-interval-milliseconds 2)) into note-list
       finally (return (cons start-note note-list)))))

; (play-timed-notes (note-list-of-rhythm-intervals '(1 1 2 1 3 1) :ioi 250))
; (play-timed-notes (note-list-of-rhythm-intervals '(1 1 1 3 1 2) :ioi 250))

(defun note-list-of-rhythm-grid (grid)
  "Given a binary rhythmic grid, returns a note list with the timing set"
  (make-midi-note abs-time
		  note-tag
		  :velocity 127
		  :duration (truncate shortest-interval-milliseconds 2))
(setf (gethash 'duration midi-parameters) duration)
)

(defun set-drum-instrument (notes drum-key-number &key (percussion-channel 9))
  "Assigns the General MIDI percussion channel and the given drum note number to all notes in the list"
  (dolist (note notes)
    (let ((midi-parameters (parameters note)))
      (setf (gethash 'key-number midi-parameters) drum-key-number)
      (setf (gethash 'midi-channel midi-parameters) percussion-channel))))
 
