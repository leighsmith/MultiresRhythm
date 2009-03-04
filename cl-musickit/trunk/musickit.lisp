;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; The Common Lisp bridge to Objective C MusicKit classes.
;;;;
;;;; By Leigh M. Smith <leigh@leighsmith.com> 
;;;;
;;;; Copyright (c) 2009, All Rights Reserved.
;;;;

(in-package :cl-musickit)
(use-package :cl-objc)
;; (use-package :objc)

(push "SND" cl-objc:*acronyms*)
(push "MK" cl-objc:*acronyms*)

;;; Forces the internment of alloc etc into the OBJC package.
(objc-cffi:import-framework "Foundation" t)

;; We need a highest level autorelease pool, otherwise many warnings are generated
(defparameter *top-level-pool* (cl-objc:invoke (cl-objc:invoke 'objc:ns-autorelease-pool objc:alloc) objc:init))

;; TODO perhaps we should check these and attempt to compile them if they don't exist.
(objc-cffi:import-framework "MKPerformSndMIDI")
(objc-cffi:import-framework "SndKit" t) ; T to load the CLOS bindings.
(objc-cffi:import-framework "MKDSP")
(objc-cffi:import-framework "MusicKit" t) ; T to load the CLOS bindings.

(defun close-musickit ()
  (cl-objc:invoke *top-level-pool* release))

;; These values are determined from settings in MKNote.h
(defparameter *mk-note-dur* 257)
(defparameter *mk-note-on* 258)
(defparameter *mk-note-off* 259)
(defparameter *mk-note-update* 260)
(defparameter *mk-mute* 261)

;; These parameters are determined from settings in params.h
(defparameter *mk-no-par* 0)		; Begin marker 
;; MIDI opcodes are represented by the presence of one of the following 12 parameters, along with the noteType 
(defparameter *mk-key-pressure* 1)	; MIDI voice msg. (See MIDI spec) 
(defparameter *mk-after-touch* 2)	; MIDI voice msg 
(defparameter *mk-control-change* 3)	; MIDI voice msg 
(defparameter *mk-pitch-bend* 4)        ; MIDI voice msg. Stored as 14-bit signed quantity, centered on 0x2000. 
(defparameter *mk-program-change* 5)	; MIDI voice msg  
(defparameter *mk-time-code-q* 6)	; MIDI time code) quarter frame 
(defparameter *mk-song-position* 7)	; MIDI system common msg (See MIDI spec) 
(defparameter *mk-song-select* 8)	; MIDI system common msg 
(defparameter *mk-tune-request* 9)	; MIDI system common message.
(defparameter *mk-sys-exclusive* 10)	; MIDI system exclusive string (See MIDI Spec) 
(defparameter *mk-chan-mode* 11)	; MIDI chan mode msg: takes a MKMidiParVal val 
(defparameter *mk-sys-real-time* 12)	; MIDI real time msg: takes a MKMidiParVal  

(defun compile-musickit ()
  "Compiles the MusicKit frameworks. Only needs to be called once to compile the
frameworks into Common Lisp interfaces."
  (objc-cffi:compile-framework ("MKPerformSndMIDI"))
  (objc-cffi:compile-framework ("SndKit"))
  (objc-cffi:compile-framework ("MKDSP"))
  (objc-cffi:compile-framework ("MusicKit")))

(defun read-score-named (filename)
  "Read the filename into the score object and return it"
  (cl-objc:objc-let ((scorefile-name 'objc:ns-string :init-with-utf8-string filename)
		     (score 'mk-score objc:init))
    (format t "Reading from ~a~%" (cl-objc:invoke scorefile-name objc:utf8-string))
    (cl-objc:invoke score :read-scorefile scorefile-name)
    score))

(defun read-smf-named (filename)
  "Read the filename of the standard MIDI file into the score object and return it"
  (cl-objc:objc-let ((midifile-name 'objc:ns-string :init-with-utf8-string filename)
		     (score 'mk-score objc:init))
    (format t "Reading from ~a~%" (cl-objc:invoke midifile-name objc:utf8-string))
    (cl-objc:invoke score :read-midifile midifile-name)
    score))

(defun onsets-of-mkpart (part)
  "Retrieve the time-tags of all noteOn and noteDur's."
  (loop
     for note-index from 0 below (cl-objc:invoke part objc:note-count)
     for note = (cl-objc:invoke part :nth note-index)
     for note-type = (cl-objc:invoke note objc:note-type)
     ;; do (format t "note ~a~%" (cl-objc:invoke (cl-objc:invoke note description) utf8-string))
     when (or (= note-type *mk-note-dur*) (= note-type *mk-note-on*))
     ;; (cl-objc:invoke note dur)
     collect (cl-objc:invoke note objc:time-tag)))

(defun onsets-of-score (score)
  "Retrieve the onset times of all parts"
  (loop
     with midi-parts = (cl-objc:invoke score objc:parts)
     for part-index from 0 below (cl-objc:invoke midi-parts objc:count)
     for part = (cl-objc:invoke midi-parts :object-at-index part-index)
     ;; do (format t "Part ~d ~a~%" part-index (cl-objc:invoke part info-note))
     append (onsets-of-mkpart part)))

(defun pitchbend-of-mkpart (part)
  "Retrieve a time-tagged list of pitchbends"
  (loop
     for note-index from 0 below (cl-objc:invoke part objc:note-count)
     for note = (cl-objc:invoke part :nth note-index)
     ;; do (format t "note ~a pitch bend present ~a~%" (cl-objc:invoke (cl-objc:invoke note description) utf8-string))
     when (and (= (cl-objc:invoke note objc:note-type) *mk-note-update*)
	       (= (cl-objc:invoke note :is-par-present *mk-pitch-bend*) 1))
     ;; (cl-objc:invoke note dur)
     collect (cl-objc:invoke note objc:time-tag) into times
     and collect (cl-objc:invoke note :par-as-int *mk-pitch-bend*) into pitch-bends
     finally (return (list times pitch-bends))))

(defun pitchbends-of-score (score)
  "Retrieve the onset times of all parts"
  (loop
     with parts = (cl-objc:invoke score objc:parts)
     for part-index from 0 below (cl-objc:invoke parts objc:count)
     for part = (cl-objc:invoke parts :object-at-index part-index)
     ;; do (format t "Part ~d ~a~%" part-index (cl-objc:invoke part info-note))
     for (times pitchbends) = (pitchbend-of-mkpart part)
     unless (null times)
     append times into all-bend-times
     append pitchbends into all-pitchbends
     finally (return (list all-bend-times all-pitchbends))))

(defun map-mkscore (score func)
  "Map the function func across each part in the score"
  (loop
     with parts = (cl-objc:invoke score parts)
     for part-index from 0 below (cl-objc:invoke parts count)
     for part = (cl-objc:invoke parts :object-at-index part-index)
     ;; do (format t "Part ~d ~a~%" part-index (cl-objc:invoke part info-note))
     append (funcall func part)))

(defun notes-of-score (score)
  ;; Can we use the automatic conversion from NSArrays to Lisp lists?
  (loop 
     with notes = (cl-objc:invoke score objc:notes)
     with note-count = (cl-objc:invoke notes objc:count)
     for note-index from 0 below note-count
     collect (cl-objc:invoke notes :object-at-index note-index)))

;; (defmethod print-object ((note mk-note))
;; (format t "~a~%" (cl-objc:invoke (cl-objc:invoke note objc:description) objc:utf8-string)))

