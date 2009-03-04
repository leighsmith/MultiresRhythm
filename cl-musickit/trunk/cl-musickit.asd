;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; ASDF definition file for Common Lisp MusicKit routines.
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2007-2009
;;;;

(require 'cl-objc)

(defsystem :cl-musickit
  :description "Common Lisp interface to the MusicKit"
  :version "1.0"
  :author "Leigh M. Smith"
  :depends-on (:portmidi :cl-objc)
  :serial t
  :components ((:file "package")
	       (:file "musickit" :depends-on ("package"))
	       ;; (:file "note") ; Lisp only version of Note class
	       ;; (:file "parts") ; Lisp only version of Part class
	       ;; (:file "playing") ; TODO use MusicKit for all I/O.
	       ;; (:file "rhythm-parts")
	       ;; (:file "drum-patterns")))
))
