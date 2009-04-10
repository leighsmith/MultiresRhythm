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

(defpackage #:multires-rhythm 
  (:nicknames #:mrr)
  (:use #:cl #:asdf #:nlisp #:cli-parser)
  (:export :rhythm 
	   :skeleton-of-rhythm
	   :tactus-for-rhythm
	   :rhythmic-grid-to-signal
	   :iois-to-rhythm
	   :rhythm-complexity
	   :rhythm-of-grid
	   :rhythm-of-onsets
	   :onsets-in-samples
	   :intervals-in-samples
	   :duration-in-samples
	   :scale-from-period
	   :time-support
	   :clap-to-rhythm
	   :save-scorefile
	   :generate-executable
	   :make-histogram
	   :add-to-histogram
	   :get-histogram
	   :get-histogram-counts
	   :get-histogram-elements
	   :plot-rhythm
	   :plot-analysis
	   :*plotting*
	   :plot-cwt+skeleton-of
	   :meter-for-name
	   :analysis-of-rhythm
	   :analysis-of-rhythm-cached
	   :skeleton
	   :voices-per-octave
	   :ridges-containing-scale
	   :make-monotone-ridge
	   :scaleogram
	   :hear
	   :.subseq
	   :subset-of-rhythm
	   :select-longest-lowest-tactus
	   :create-weighted-beat-ridge
	   :save-rhythm-and-claps
	   :save-rhythm-mix
	   :sample-rate
	   :time-signal
	   :last-expectations))
