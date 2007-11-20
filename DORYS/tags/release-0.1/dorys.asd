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

(defpackage #:dorys (:use #:cl #:asdf)
	    (:export :*national-anthems*
		     :anthem-named :meter-numerator :anthem-name
		     :anthem-beat-duration :anthem-bar-duration :anthem-anacrusis-duration
		     :evaluation-of-anthems
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

#|

From the common music version
;;; TODO - get all CLM output going to separate directory.


;;; The output instruments are typically a rhythm-onset instrument
;;; which is nominally an impulse at the onset time of the beat.
;;; check if this needs recompiling.
(load "rhythms.fas")

;;; TODO use a common rhythm instrument that can be substituted for
;;; midi-note, rhythm-tone, sampler etc.

;; Functions for defining accents
(load "accentfns.cm")

;; Examples from published rhythm psychology literature
(load "rhythm_psych.cm")

;; Examples of computed rhythms demonstrating various grouping and accenting
(load "synthetic_rhythms.cm")

;; Examples demonstrating rhythmic difference (beating) frequencies 
(load "badcock_beating.cm")

;; rhythms demonstrating the limits of perceptible tempos, streaming etc.
(load "tempo_limits.cm")

;; rhythms demonstrating agogic accentuation
(load "agogics.cm")

(load "expression.cm")
(load "isochronous_drumming.cm")

(load "polyrhythms.cm")

;; This seems to have just been a conversion from MIDI
;(load "periodicity-pitch.cm")

(load "ExampleRhythms/FromTheRepertory/PinkPanther.cm")

;; A quantized example exhibiting triplets and rests
;; there is a performance equivalent.
(load "ExampleRhythms/FromTheRepertory/ThreeBlindMice.cm")

;; A set of item streams containing versions of quantized Greensleeves rhythms
(load "ExampleRhythms/FromTheRepertory/Greensleeves/greensleeves-item.cm")

;; an incorrect version of the greensleeves rhythm
(load "ExampleRhythms/FromTheRepertory/Greensleeves/greensleeves_near_performed.cm")

(load "ExampleRhythms/FromTheRepertory/Greensleeves/greensleeves_performed.cm")

;; A set of three performances of the greensleeves rhythm at different
;; tempos.
(load "ExampleRhythms/FromTheRepertory/Greensleeves/Performances/greensleeves_performed_new.cm")

(load "ExampleRhythms/FromTheRepertory/Greensleeves/greensleeves-rhy-gauss.cm")

;(load "ExampleRhythms/FromTheRepertory/Greensleeves/greensleeves_accent_missing_midi.cm")
;(load "ExampleRhythms/FromTheRepertory/Greensleeves/greensleeves_accent_midi.cm")

;; MusicKit conversion...?
(load "ExampleRhythms/FromTheRepertory/Greensleeves/greensleeves_drumming.cm")


;;; Recordings of Human Performances
;(load "ExampleRhythms/Performances/Quantised.cm")

;;; Classical performance data by H. Christopher Longuet-Higgins.
(load "ExampleRhythms/Performances/LonguetHiggins.cm")

;(load "ExampleRhythms/ComparativeExamples/LaMarseillaise.cm")
;(load "ExampleRhythms/ComparativeExamples/TurkishFolk.cm")

|#
