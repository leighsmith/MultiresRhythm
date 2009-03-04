;;;; -*- Lisp -*-
;;;; $Id$
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
   (time-tag   :initarg :time-tag   :accessor time-tag)
   (parameters :initarg :parameters :accessor parameters :initform (make-hash-table))))

(defmethod note-duration ((the-note note))
  (gethash 'duration (parameters the-note)))

(defmethod set-note-duration ((the-note note) duration)
  (setf (gethash 'duration (parameters the-note)) duration))

(defmethod compare ((first-note note) (second-note note))
  "Compare the time-tags"
  (< (time-tag first-note) (time-tag second-note)))

(defmethod note-in-region ((note-to-check note) region-start region-end)
  "Predicate checking if a note is within a temporal region"
  (and (>= (time-tag note-to-check) region-start)
       (< (time-tag note-to-check) region-end)))

(defun parameter-list (parameter-hash-table)
  "Return a two level list of parameters and values, given the hash table"
  (with-hash-table-iterator (parameter-retriever parameter-hash-table)
    (loop
       for (more? parameter-name parameter-value) = (multiple-value-list (parameter-retriever))
       while more?
       collect (list parameter-name parameter-value))))

(defun print-note-parameters (parameter-hash-table)
  (format nil "~:{~a: ~a~:^, ~}" (parameter-list parameter-hash-table)))

(defmethod print-object ((note-object note) stream)
  (call-next-method note-object stream) ;; to print the superclass.
  (let ((note-tag-list (list (note-tag note-object))))
    (if (equal (note-type note-object) :note-duration) 
	(push (note-duration note-object) note-tag-list))
    (format stream " at ~f: ~a ~a~%~a~%" 
	    (time-tag note-object) (note-type note-object) note-tag-list 
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
		   :time-tag time
		   :parameters midi-parameters)))


