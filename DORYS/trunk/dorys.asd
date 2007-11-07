;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; ASDF definition file for Database of Rhythmic Stimuli.
;;;;
;;;; by Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006, 2007
;;;;
;;;; Known to work on SBCL 1.0.10
;;;;

(defpackage #:dorys (:use #:cl #:asdf)
	    (:export :*national-anthems*
		     :anthem-named
		     :anthem-beat-duration
		     :read-esac-file))

(in-package :dorys)

(defsystem :dorys
  :description "A Database of Rhythmic Stimuli"
  :version "1.0"
  :author "Leigh M. Smith"
  :license "Copyright (c) 2005-2007"
  :components ((:file "national anthems")
	       (:file "anthem-accessors" :depends-on ("national anthems"))
	       (:file "EsAC")))
