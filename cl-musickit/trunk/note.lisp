;;;; -*- Lisp -*-
;;;; $Id: playing.lisp 236 2007-03-09 14:21:51Z leigh $
;;;;
;;;; General purpose note, which has a continuous time, an identifier (tag) and a hash
;;;; table of parameters.
;;;;
;;;; Leigh M. Smith <lsmith@science.uva.nl>
;;;;

(in-package :cl-musickit)

(defclass note ()
  ;; :note-duration :note-on :note-off :note-update
  ((note-type  :initarg :note-type  :accessor note-type :initform :note-duration) 
   (note-tag   :initarg :note-tag   :accessor note-tag)
   (play-time  :initarg :play-time  :accessor play-time)
   (parameters :initarg :parameters :accessor parameters :initform (make-hash-table))))

(defmethod note-duration ((the-note note))
  (gethash 'duration (parameters the-note)))

(defmethod set-note-duration ((the-note note) duration)
  (setf (gethash 'duration (parameters the-note)) duration))
  
(defun print-note-parameters (parameter-hash-table)
  (let ((parameter-string ""))
    (with-hash-table-iterator (parameter-retriever parameter-hash-table)
      (loop
	 (multiple-value-bind (more? parameter-name parameter-value) (parameter-retriever)
	   (if more?
	       (setf parameter-string (concatenate 'string parameter-string
						   (format nil "~a: ~a, " parameter-name parameter-value)))
	       (return parameter-string)))))))

(defmethod print-object ((note-object note) stream)
  (call-next-method note-object stream) ;; to print the superclass.
  (let ((note-tag-list (list (note-tag note-object))))
    (if (equal (note-type note-object) :note-duration) 
	(push (note-duration note-object) note-tag-list))
    (format stream " at ~f: ~a ~a~%~a~%" 
	    (play-time note-object) (note-type note-object) note-tag-list 
	    (print-note-parameters (parameters note-object)))))

(defun make-midi-note (time note-tag &key (key-number 60) (velocity 127) (duration 60) (midi-channel 0))
  "Creates a note instance using MIDI parameters"
  (let ((midi-parameters (make-hash-table)))
    (setf (gethash 'key-number midi-parameters) key-number)
    (setf (gethash 'velocity midi-parameters) velocity)
    (setf (gethash 'midi-channel midi-parameters) midi-channel)
    (setf (gethash 'duration midi-parameters) duration)	; in mSec.
    (make-instance 'note 
		   :note-type :note-duration
		   :note-tag note-tag
		   :play-time time
		   :parameters midi-parameters)))


