;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; ASDF definition file for multiresolution time-frequency analysis and interpretation of musical rhythm.
;;;;
;;;; by Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006, 2007 All Rights Reserved.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; Known to work on SBCL 1.0.10
;;;;
;;;; See for background:
;;;;   author =  {Leigh M. Smith},
;;;;   title =   {A Multiresolution Time-Frequency Analysis and Interpretation of Musical Rhythm},
;;;;   school =  {Department of Computer Science, University of Western Australia},
;;;;   year =    2000,
;;;;   month =   {October},
;;;;   annote =  {\url{http://www.science.uva.nl/~lsmith/Papers/MultiresRhythm.pdf}}
;;;;

(require 'nlisp)			; For mathematics
(require 'cli-parser)			; For command line option interpretation.

;; Should probably put this in a separate package.lisp file.
(defpackage #:multires-rhythm (:use #:cl #:asdf #:nlisp #:cli-parser)
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
		     :.subseq))

(in-package :multires-rhythm)

(defsystem :multiresrhythm
  :description "A Multiresolution Time-Frequency Analysis and Interpretation of Musical Rhythm"
  :version "2.8"
  :author "Leigh M. Smith"
  :license "Copyright (c) 2005-2008"
  :depends-on (:nlisp)
  :properties ((#:author-email . "lsmith@science.uva.nl")
	       (#:date . "2008")	; Compute this automatically?
	       ((#:albert #:output-dir) . "documentation")
	       ((#:albert #:formats) . ("docbook")))
  :components ((:file "signalprocessing")
	       (:file "ridges")
	       (:file "scorefile")
	       (:file "sound")
	       (:file "histogram")
	       (:file "martin-trees")
	       (:file "skeleton" :depends-on ("ridges"))
	       (:file "plotting" :depends-on ("ridges" "signalprocessing"))
	       (:file "rhythm" :depends-on ("scorefile" "plotting" "signalprocessing" "sound"))
	       (:file "cwt" :depends-on ("plotting" "ridges" "rhythm"))
	       (:file "tempo" :depends-on ("cwt" "signalprocessing"))
	       (:file "multires_rhythm" 
		      :depends-on ("cwt" "skeleton" "plotting" "rhythm" "signalprocessing" "tempo"))
	       (:file "scaleogram_plotting" :depends-on ("multires_rhythm" "cwt" "rhythm" "plotting"))
	       (:file "tactus-selection" :depends-on ("multires_rhythm" "scaleogram_plotting"))
	       (:file "downbeat" :depends-on ("multires_rhythm"))
	       (:file "clapping" :depends-on ("multires_rhythm" "downbeat" "tactus-selection"))
	       (:file "perceptual-onsets" :depends-on ("clapping" "sound"))
	       (:file "expectancies" :depends-on ("clapping" "signalprocessing"))
	       (:file "meter" :depends-on ("multires_rhythm" "tactus-selection" "expectancies"))
	       (:file "metrical-expectancy" :depends-on ("expectancies" "martin-trees" "histogram"))
	       (:file "commandline" :depends-on ("expectancies"))))
