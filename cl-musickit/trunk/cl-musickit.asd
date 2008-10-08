;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; ASDF definition file for Common Lisp MusicKit routines.
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2007
;;;;
;;;;

(defpackage #:cl-musickit (:use #:cl #:asdf)
	    (:export :drum-machine
		     :make-midi-note
		     :play-timed-notes))

(in-package :cl-musickit)

(defsystem :cl-musickit
  :description "Common Lisp MusicKit playing"
  :version "1.0"
  :author "Leigh M. Smith"
  :depends-on (:portmidi)
  :serial t
  :components ((:file "note")
	       (:file "playing")
	       (:file "rhythm-parts")
	       (:file "drum-patterns")))
