;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; ASDF definition file for Multiresolution Rhythm.
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006, 2007 All Rights Reserved.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; Known to work on SBCL 0.9.18
;;;;
;;;; See for background:
;;;;   author =  {Leigh M. Smith},
;;;;   title =   {A Multiresolution Time-Frequency Analysis and Interpretation of Musical Rhythm},
;;;;   school =  {Department of Computer Science, University of Western Australia},
;;;;   year =    1999,
;;;;   month =   {June},
;;;;   annote =  {\url{http://www.leighsmith.com/Research/Papers/MultiresRhythm.pdf}}
;;;;

(require 'nlisp)

(defpackage #:multires-rhythm (:use #:cl #:asdf #:nlisp)
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
  :depends-on (:nlisp :zlib :imago)
  :components ((:file "ridges")
	       (:file "file_io")
	       (:file "plotting" :depends-on ("ridges"))
	       (:file "rhythm" :depends-on ("file_io" "plotting"))
	       (:file "cwt" :depends-on ("plotting" "ridges" "rhythm"))
	       (:file "multires_rhythm" :depends-on ("cwt" "plotting" "rhythm"))
	       (:file "test-examples" :depends-on ("rhythm" "multires_rhythm"))))
