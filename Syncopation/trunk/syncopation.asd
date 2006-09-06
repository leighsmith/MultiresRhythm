;;;; -*- Lisp -*-
;;;;
;;;; $Id$
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
;  :depends-on (:multiresrhythm)
  :serial t
  :components ((:file "rhythm")
	       (:file "LHL84")
	       (:file "plotting")
	       ;;(:file "playing")
	       ;(:file "Shmulevich")
	       (:file "Essens")))
