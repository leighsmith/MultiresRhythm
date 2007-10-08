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
(require 'cl-fad)			; For file I/O

(defpackage #:multires-rhythm (:use #:cl #:asdf #:nlisp #:cl-fad)
	    (:export :rhythm 
		     :skeleton-of-rhythm
		     :tactus-for-rhythm
		     :rhythmic-grid-to-signal
		     :rhythm-complexity
		     :rhythm-of-grid
		     :clap-to-rhythm
		     :save-scorefile))

(in-package :multires-rhythm)

(defsystem :multiresrhythm
  :description "A Multiresolution Time-Frequency Analysis and Interpretation of Musical Rhythm"
  :version "2.1"
  :author "Leigh M. Smith"
  :depends-on (:nlisp)
  :properties ((#:author-email . "lsmith@science.uva.nl")
	       (#:date . "2007")	; Compute this automatically?
	       ((#:albert #:output-dir) . "documentation")
	       ((#:albert #:formats) . ("docbook")))
  :components ((:file "signalprocessing")
	       (:file "ridges")
	       (:file "scorefile")
	       (:file "sound")
	       (:file "skeleton" :depends-on ("ridges"))
	       (:file "plotting" :depends-on ("ridges" "signalprocessing"))
	       (:file "rhythm" :depends-on ("scorefile" "plotting" "signalprocessing"))
	       (:file "cwt" :depends-on ("plotting" "ridges" "rhythm"))
	       (:file "scaleogram_plotting" :depends-on ("cwt"))
	       (:file "multires_rhythm" :depends-on ("cwt" "skeleton" "plotting" "rhythm" "signalprocessing"))
	       (:file "downbeat" :depends-on ("multires_rhythm"))
	       (:file "clapping" :depends-on ("multires_rhythm" "downbeat"))
	       (:file "perceptual-onsets" :depends-on ("clapping" "sound"))
	       (:file "expectancies" :depends-on ("clapping"))
	       (:file "test-examples" :depends-on ("rhythm" "multires_rhythm" "clapping"))
	       (:file "national anthems")
	       (:file "test-anthems" :depends-on ("national anthems" "multires_rhythm" "clapping"))))