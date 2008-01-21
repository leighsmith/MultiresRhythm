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
			       :duration (floor shortest-interval-milliseconds 2)) into note-list
       finally (return (cons start-note note-list)))))

; (play-timed-notes (note-list-of-rhythm-intervals '(1 1 2 1 3 1) :ioi 250))
; (play-timed-notes (note-list-of-rhythm-intervals '(1 1 1 3 1 2) :ioi 250))

(defun grid-to-onsets (grid)
  "From a binary grid returns onset locations of non-zero values"
  (loop
     for note in grid
     counting note into ioi
     when (not (zerop note))
     collect (1- ioi)))

(defun note-list-of-rhythm-grid (grid &key ((:tempo tempo-in-bpm) 60) 
				 ;; number of grid elements making up a quarter-note, which is the beat for BPM.
				 (grids-per-crotchet 4) 
				 (ioi 1.0 interval-supplied-p))
  "Given a binary rhythmic grid, returns a note list with the timing set"
  (loop
     with onsets = (grid-to-onsets grid) ; (append grid '(1))
     with shortest-interval-milliseconds = (if interval-supplied-p 
					       ioi 
					       (/ 60000 tempo-in-bpm grids-per-crotchet))
     for onset-time in onsets
     for note-tag = 1 then (1+ note-tag)
     collect (make-midi-note (floor (* onset-time shortest-interval-milliseconds))
			     note-tag
			     :velocity 127
			     :duration (floor shortest-interval-milliseconds 2))))

(defun set-drum-instrument (notes drum-key-number &key (percussion-channel 9))
  "Assigns the General MIDI percussion channel and the given drum note number to all notes in the list"
  (dolist (note notes)
    (let ((midi-parameters (parameters note)))
      (setf (gethash 'key-number midi-parameters) drum-key-number)
      (setf (gethash 'midi-channel midi-parameters) percussion-channel)))
  notes)

(defun renumber-part (note-list)
  "Rewrites the note tag, typically only needed after merging note lists"
  (loop 
     for note in note-list
     for new-note-tag = 1 then (1+ new-note-tag) ; why start at 1 & not 0?
     do (setf (note-tag note) new-note-tag)
     finally (return note-list)))

;;; TODO perhaps this should be mix-parts
;;; Perhaps just concatenate and sort by time?
(defun mix-note-lists (note-lists)
  "Returns a single note list combining the supplied list of separate note lists ordered by time"
  (loop 
     for part in note-lists
     and time-ordered-part = '() then (merge 'list time-ordered-part part #'compare)
     finally (return (renumber-part (merge 'list time-ordered-part part #'compare)))))

;(defun time-cut (note-list start-time cut-duration)
;  "Removes cut-duration time from the note-list starting at start-time") 
