;;;; -*- Lisp -*-
;;;;
;;;; $Id: syncopation.asd 253 2007-04-13 16:35:02Z leigh $
;;;;
;;;; ASDF definition file for Common Lisp MusicKit routines.
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2007
;;;;
;;;;

(defpackage #:cl-musickit (:use #:cl #:asdf))

(in-package :cl-musickit)

(defsystem :cl-musickit
  :description "Common Lisp MusicKit playing"
  :version "1.0"
  :author "Leigh M. Smith"
  :depends-on (:portmidi)
  :serial t
  :components ((:file "note")
	       (:file "playing")))
