;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; ASDF definition file for Multiresolution Rhythm.
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
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
		     :clap-to-rhythm
		     :save-scorefile))

(in-package :multires-rhythm)

;; This is required to get around a fucking Package Lock Violation deep within SBCL's CLOS?
;;(in-package :cl)
(sb-ext:add-implementation-package :multires-rhythm :cl)
;;(in-package :multires-rhythm)


(defsystem :multiresrhythm
  :description "A Multiresolution Time-Frequency Analysis and Interpretation of Musical Rhythm"
  :version "2.1"
  :author "Leigh M. Smith"
  :depends-on (:nlisp :zlib :imago)
  :components ((:file "ridges")
	       (:file "rhythm")
	       (:file "plotting" :depends-on ("ridges"))
	       (:file "cwt" :depends-on ("plotting" "ridges"))
	       (:file "multires_rhythm" :depends-on ("cwt" "plotting" "rhythm"))
	       (:file "test-examples" :depends-on ("rhythm" "multires_rhythm"))
	       (:file "file_io")))
