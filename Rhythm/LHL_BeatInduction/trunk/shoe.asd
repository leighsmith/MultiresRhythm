;;;; -*- Lisp -*-
;;;;
;;;; $Id: syncopation.asd 253 2007-04-13 16:35:02Z leigh $
;;;;
;;;; ASDF definition file for Syncopation and Complexity models.
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;

(defpackage #:shoe (:use #:cl #:asdf))

(in-package :shoe)

(defsystem :shoe
  :description "Longuet-Higgins footapping"
  :version "1.0"
  :author "Henkjan Honing and Leigh M. Smith"
  :serial t
  :components ((:file "01 rule-based system")
	       (:file "02 tap functions")
	       (:file "04 trace printer")
	       (:file "10 LHL82 microversion")
	       (:file "pattern_trees")
	       (:file "correct_fraction")
	       (:file "random_generators")))

;;; Original files:
;; "00 shoe-graphics"
;;  "03 foot tapper"
;;  "11 LH shoe micro version" 
;;  "12 LEE85 microversion"))

