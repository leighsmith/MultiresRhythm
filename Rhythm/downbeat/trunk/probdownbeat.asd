;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; ASDF definition file for probabilistic model of downbeat.
;;;;
;;;; by Leigh M. Smith <leigh.smith@ircam.fr> 
;;;;
;;;; Copyright (c) 2009 All Rights Reserved.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; Known to work on SBCL 1.0.29
;;;;
;;;; See for background:
;;;;   author =  {Leigh M. Smith},
;;;;

(require 'nlisp)
(require 'multiresrhythm)
(require 'cxml)				; For IRCAM beat I/O and other XML formats.

(defsystem :probdownbeat
  :description "A Probabilistic Model of Downbeat"
  :version "1.0"
  :author "Leigh M. Smith"
  :license "Copyright (c) 2009"
  :depends-on (:nlisp :multiresrhythm :cxml)
  :properties ((#:author-email . "Leigh.Smith@ircam.fr")
	       (#:date . "2009")	; Compute this automatically?
	       ((#:albert #:output-dir) . "documentation")
	       ((#:albert #:formats) . ("docbook")))
  :serial t
  :components ((:file "package")
	       (:file "viterbi")
	       (:file "ircam-xml")
	       (:file "rhythm-description" :depends-on ("ircam-xml"))
	       (:file "probabilistic-downbeat" :depends-on ("viterbi"))))
