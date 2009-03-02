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

(defsystem :cl-musickit
  :description "Common Lisp interface to the MusicKit"
  :version "1.0"
  :author "Leigh M. Smith"
  :depends-on (:portmidi :cl-objc)
  :serial t
  :components ((:file "package")
	       (:file "musickit")
	       (:file "note")
	       (:file "parts")
	       (:file "playing") ; Perhaps this can be merged with musickit.lisp
	       (:file "rhythm-parts")
	       (:file "drum-patterns")))
