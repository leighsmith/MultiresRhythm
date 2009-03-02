;;;; -*- Lisp -*-
;;;; $Id: rhythm-parts.lisp 5234 2008-01-22 13:34:00Z leigh $
;;;; Routines to play parts.
;;;;
;;;; Leigh M. Smith <lsmith@science.uva.nl>
;;;;

(in-package :cl-musickit)

(defun note-list-of-part (part)
  "Convert a DORYS 'part' (list of lists of positional parameters) into a cl-musickit note-list of note instances"
  (let ((note-tag 0))
    (mapcar (lambda (note-parameter-list) 
	      (destructuring-bind (onset-time duration amplitude midi-key-number) note-parameter-list
		(make-midi-note (floor (* onset-time 1000)) ; convert back to mSec.
				(incf note-tag)
				:velocity (round (* amplitude 127))
				:duration (floor (* duration 1000)) ; convert back to mSec.
				:key-number midi-key-number
				:midi-channel 1)))
	    part)))
