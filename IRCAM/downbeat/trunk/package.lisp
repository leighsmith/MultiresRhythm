;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Defines the package and the exported symbols.
;;;;
;;;; By Leigh M. Smith <Leigh.Smith@ircam.fr> 
;;;;
;;;; Copyright (c) 2008
;;;;

(defpackage #:prob-downbeat
  (:nicknames #:downbeat)
  (:use #:cl #:asdf #:multires-rhythm)
  (:export :downbeat-estimation
	   :read-annotated-rhythm))
