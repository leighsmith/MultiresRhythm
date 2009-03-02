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
;;;;

(in-package :cl-musickit)
;;(in-package :cl-objc)
(use-package :cl-objc)

(push "SND" cl-objc:*acronyms*)
(push "MK" cl-objc:*acronyms*)

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

;; (defun read-score-named (filename)
;;   "Read the filename into the score object and return it"
;;   (let* ((scorefile-name (invoke (invoke 'ns-string objc-cffi::alloc) :init-with-utf8-string filename))
;; 	 (score (invoke (invoke 'mk-score cl-objc::alloc) objc-cffi::init)))
;;     (format t "Reading from ~a~%" (invoke scorefile-name objc-cffi::utf8-string))
;;     (invoke score :read-scorefile scorefile-name)
;;     score))

(defun read-score-named (filename)
  "Read the filename into the score object and return it"
  (objc-let ((scorefile-name 'ns-string :init-with-utf8-string filename)
	     (score 'mk-score objc-cffi::init))
    (format t "Reading from ~a~%" (invoke scorefile-name objc-cffi::utf8-string))
    (invoke score :read-scorefile scorefile-name)
    score))

(defun read-smf-named (filename)
  "Read the filename of the standard MIDI file into the score object and return it"
  (objc-let ((midifile-name 'ns-string :init-with-utf8-string filename)
	     (score 'mk-score objc-cffi::init))
    (format t "Reading from ~a~%" (invoke midifile-name objc-cffi::utf8-string))
    (invoke score :read-midifile midifile-name)
    score))

(defun onsets-of-mkpart (part)
  "Retrieve the time-tags of all noteOn and noteDur's."
  (loop
     for note-index from 0 below (invoke part objc-cffi::note-count)
     for note = (invoke part :nth note-index)
     for note-type = (invoke note objc-cffi::note-type)
     ;; do (format t "note ~a~%" (invoke (invoke note description) utf8-string))
     when (or (= note-type *mk-note-dur*) (= note-type *mk-note-on*))
     ;; (invoke note dur)
     collect (invoke note objc-cffi::time-tag)))

(defun onsets-of-mkscore (score)
  "Retrieve the onset times of all parts"
  (loop
     with midi-parts = (invoke score objc-cffi::parts)
     for part-index from 0 below (invoke midi-parts objc-cffi::count)
     for part = (invoke midi-parts :object-at-index part-index)
     ;; do (format t "Part ~d ~a~%" part-index (invoke part info-note))
     append (onsets-of-mkpart part)))

(defun pitchbend-of-mkpart (part)
  "Retrieve a time-tagged list of pitchbends"
  (loop
     for note-index from 0 below (invoke part objc-cffi::note-count)
     for note = (invoke part :nth note-index)
     ;; do (format t "note ~a pitch bend present ~a~%" (invoke (invoke note description) utf8-string))
     when (and (= (invoke note objc-cffi::note-type) *mk-note-update*)
	       (= (invoke note :is-par-present *mk-pitch-bend*) 1))
     ;; (invoke note dur)
     collect (invoke note objc-cffi::time-tag) into times
     and collect (invoke note :par-as-int *mk-pitch-bend*) into pitch-bends
     finally (return (list times pitch-bends))))

(defun pitchbends-of-mkscore (score)
  "Retrieve the onset times of all parts"
  (loop
     with parts = (invoke score objc-cffi::parts)
     for part-index from 0 below (invoke parts objc-cffi::count)
     for part = (invoke parts :object-at-index part-index)
     ;; do (format t "Part ~d ~a~%" part-index (invoke part info-note))
     for (times pitchbends) = (pitchbend-of-mkpart part)
     unless (null times)
     append times into all-bend-times
     append pitchbends into all-pitchbends
     finally (return (list all-bend-times all-pitchbends))))

(defun map-mkscore (score func)
  "Map the function func across each part in the score"
  (loop
     with parts = (invoke score parts)
     for part-index from 0 below (invoke parts count)
     for part = (invoke parts :object-at-index part-index)
     ;; do (format t "Part ~d ~a~%" part-index (invoke part info-note))
     append (funcall func part)))

;; We need a highest level autorelease pool, otherwise many warnings are generated
(let ((top-level-pool nil))

  (defun initialise-musickit ()
    "Loads the frameworks and creates an autorelease pool"
    (unless top-level-pool 
      (setf top-level-pool (invoke (invoke 'ns-autorelease-pool cl-objc::alloc) cl-objc::init))
      ;; TODO perhaps we should check these and attempt to compile them if they don't exist.
      (objc-cffi:import-framework "MKPerformSndMIDI")
      (objc-cffi:import-framework "SndKit")
      (objc-cffi:import-framework "MKDSP")
      (objc-cffi:import-framework "MusicKit"))) ;; T to load the CLOS bindings.

  (defun close-musickit ()
    (invoke top-level-pool release)))
