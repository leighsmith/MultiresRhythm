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
(require 'dorys)			; For databases to test against.
(require 'shoe)				; For anthem comparisons. TODO should be factored out.

;; (setf (logical-pathname-translations "rhythm") '(("data;*.*.*" "/Volumes/iDisk/Research/Data/")))

(defpackage #:multires-rhythm (:use #:cl #:asdf #:nlisp #:dorys #:cli-parser)
	    (:export :rhythm 
		     :skeleton-of-rhythm
		     :tactus-for-rhythm
		     :rhythmic-grid-to-signal
		     :rhythm-complexity
		     :rhythm-of-grid
		     :clap-to-rhythm
		     :save-scorefile
		     :generate-executable))

(in-package :multires-rhythm)

(defsystem :multiresrhythm
  :description "A Multiresolution Time-Frequency Analysis and Interpretation of Musical Rhythm"
  :version "2.6.2"
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
	       (:file "skeleton" :depends-on ("ridges"))
	       (:file "plotting" :depends-on ("ridges" "signalprocessing"))
	       (:file "rhythm" :depends-on ("scorefile" "plotting" "signalprocessing" "sound"))
	       (:file "cwt" :depends-on ("plotting" "ridges" "rhythm"))
	       (:file "multires_rhythm" :depends-on ("cwt" "skeleton" "plotting" "rhythm" "signalprocessing"))
	       (:file "scaleogram_plotting" :depends-on ("multires_rhythm" "cwt" "rhythm" "plotting"))
	       (:file "tactus-selection" :depends-on ("multires_rhythm" "scaleogram_plotting"))
	       (:file "meter" :depends-on ("multires_rhythm" "tactus-selection"))
	       (:file "downbeat" :depends-on ("multires_rhythm"))
	       (:file "clapping" :depends-on ("multires_rhythm" "downbeat" "tactus-selection"))
	       (:file "perceptual-onsets" :depends-on ("clapping" "sound"))
	       (:file "expectancies" :depends-on ("clapping"))
	       (:file "metrical-expectancy" :depends-on ("expectancies"))
	       (:file "test-examples" :depends-on ("rhythm" "multires_rhythm" "clapping"))
	       (:file "test-anthems" :depends-on ("multires_rhythm" "clapping" "meter"))
	       (:file "commandline" :depends-on ("expectancies"))))