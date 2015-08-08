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
  (:use #:cl #:asdf #:nlisp #:multires-rhythm)
  (:export :downbeat-estimation
	   :observe-evidence-of
	   :observe-onsets
	   :rhythm-description
	   :read-ircambeat-bpm
	   :read-ircam-marker-times
	   :read-ircam-annotation-timesignatures
	   :read-annotated-downbeats
	   :read-annotated-rhythm
	   :read-analysed-rhythm
	   :rhythm
	   :meter))
