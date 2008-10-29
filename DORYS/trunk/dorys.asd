;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; ASDF definition file for DORYS - A Database Of RhYthmic Stimuli.
;;;;
;;;; Comprises test data for experiments in the psychology and modelling of musical 
;;;; rhythm coded as Common Lisp data and programs to act as a set
;;;; of stimulus data to compare human and automatic perception
;;;; performances.
;;;;
;;;; by Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 1995-2007
;;;;
;;;; Known to work on SBCL 1.0.10
;;;;

(require 'shoe)				; For anthem comparisons.
(require 'nlisp)			; For mathematics functions.
(require 'multiresrhythm)		; For multires-rhythm library.

;; (setf (logical-pathname-translations "rhythm") '(("data;*.*.*" "/Volumes/iDisk/Research/Data/")))

(defpackage #:dorys (:use #:cl #:asdf #:nlisp #:multires-rhythm #:shoe)
	    (:export :*national-anthems*
		     :anthem-named :meter-numerator :anthem-name
		     :anthem-beat-duration :anthem-bar-duration :anthem-anacrusis-duration
		     :anthems-of-meter
		     :evaluation-of-anthems
		     :read-esac-file))

(in-package :dorys)

(defsystem :dorys
  :description "A Database of Rhythmic Stimuli"
  :version "1.1"
  :author "Leigh M. Smith"
  :license "Copyright (c) 2005-2008"
  :depends-on (:multiresrhythm :shoe :nlisp)
  :components ((:file "evaluation")
	       (:file "national anthems")
	       (:file "anthem-accessors" :depends-on ("national anthems" "evaluation"))
	       (:file "anthem-evaluation" :depends-on ("anthem-accessors"))
	       (:file "EsAC")
	       (:file "essen-meter-list")
	       (:file "test-multires-rhythm")
	       ;; (:file "JMM-diagrams")
	       (:file "klapuri")
	       ;; (:file "rhythm-psych-examples") ; Examples from published rhythm psychology literature
	       (:file "test-anthems" :depends-on ("national anthems" "anthem-evaluation"))
	       (:file "NIPS-examples")
	       (:file "metrical-expectancy")
	       (:file "ritard")
	       (:file "tempo-change")
	       (:file "test-clapping")))

#|

From the common music version:
;;; TODO - get all CLM output going to separate directory.

;;; TODO use a common rhythm instrument that can be substituted for
;;; midi-note, rhythm-tone, sampler etc.

;; Functions for defining accents
(:file "accentfns")


;; Examples of computed rhythms demonstrating various grouping and accenting
(:file "synthetic_rhythms")

;; Examples demonstrating rhythmic difference (beating) frequencies 
(:file "badcock_beating")

;; rhythms demonstrating the limits of perceptible tempos, streaming etc.
(:file "tempo_limits")

;; rhythms demonstrating agogic accentuation
(:file "agogics")

(:file "expression")
(:file "isochronous_drumming")

(:file "polyrhythms")

;; This seems to have just been a conversion from MIDI
;(:file "periodicity-pitch")

(:file "ExampleRhythms/FromTheRepertory/PinkPanther")

;; A quantized example exhibiting triplets and rests
;; there is a performance equivalent.
(:file "ExampleRhythms/FromTheRepertory/ThreeBlindMice")

;; A set of item streams containing versions of quantized Greensleeves rhythms
(:file "ExampleRhythms/FromTheRepertory/Greensleeves/greensleeves-item")

;; an incorrect version of the greensleeves rhythm
(:file "ExampleRhythms/FromTheRepertory/Greensleeves/greensleeves_near_performed")

(:file "ExampleRhythms/FromTheRepertory/Greensleeves/greensleeves_performed")

;; A set of three performances of the greensleeves rhythm at different
;; tempos.
(:file "ExampleRhythms/FromTheRepertory/Greensleeves/Performances/greensleeves_performed_new")

(:file "ExampleRhythms/FromTheRepertory/Greensleeves/greensleeves-rhy-gauss")

;(:file "ExampleRhythms/FromTheRepertory/Greensleeves/greensleeves_accent_missing_midi")
;(:file "ExampleRhythms/FromTheRepertory/Greensleeves/greensleeves_accent_midi")

;; MusicKit conversion...?
(:file "ExampleRhythms/FromTheRepertory/Greensleeves/greensleeves_drumming")


;;; Recordings of Human Performances
;(:file "ExampleRhythms/Performances/Quantised")

;;; Classical performance data by H. Christopher Longuet-Higgins.
(:file "ExampleRhythms/Performances/LonguetHiggins")

;(:file "ExampleRhythms/ComparativeExamples/LaMarseillaise")
;(:file "ExampleRhythms/ComparativeExamples/TurkishFolk")

|#
