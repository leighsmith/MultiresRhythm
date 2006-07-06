;;; -*- Lisp -*-
;;;
;;; $Id$
;;;
(defpackage #:multiresrhythm-system (:use #:cl #:asdf))
(in-package :multiresrhythm-system)

(defsystem :multiresrhythm
           :version "1.0"
	   :author "Leigh M. Smith"
	   :depends-on (:nlisp :zlib :imago)
           :components ((:file "cwt")
			(:file "ridges")
			(:file "rhythm")
			(:file "plotting")
			(:file "multires_rhythm")))
