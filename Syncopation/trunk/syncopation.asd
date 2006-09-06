;;;; -*- Lisp -*-
;;;;
;;;; $Id: multiresrhythm.asd 80 2006-08-16 14:35:54Z leigh $
;;;;
;;;; ASDF definition file for Syncopation and Complexity models.
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;

(defpackage #:syncopation (:use #:cl #:asdf #:nlisp))

(in-package :syncopation)

;; This is required to get around a fucking Package Lock Violation deep within SBCL's CLOS?
;;(in-package :cl)
;;(sb-ext:add-implementation-package :multires-rhythm :cl)
;;(in-package :multires-rhythm)

(defsystem :syncopation
  :description "Syncopation and Rhythm Complexity models"
  :version "1.0"
  :author "Leigh M. Smith and Henkjan Honing"
  :depends-on (:multiresrhythm)
  :components ((:file "rhythm")
	       (:file "LHL84")
	       (:file "plotting.lisp")
	       ;;(:file "playing.lisp")
	       (:file "Shmulevich.lisp")
	       (:file "Essens.lisp")))
