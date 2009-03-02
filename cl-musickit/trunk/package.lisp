;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Package definition and exported symbols for Common Lisp MusicKit routines.
;;;;
;;;; By Leigh M. Smith <leigh@leighsmith.com> 
;;;;
;;;; Copyright (c) 2009
;;;;

(defpackage #:cl-musickit (:use #:cl #:asdf)
	    (:export :initialise-musickit
		     :drum-machine
		     :make-midi-note
		     :play-timed-notes
		     :read-score-named
		     :read-smf-named
		     :onsets-of-mkscore
		     :pitchbends-of-mkscore))
