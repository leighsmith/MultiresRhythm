;;;; -*- Lisp -*-
;;;;
;;;; ASDF definition file for multiresolution time-frequency analysis and interpretation of musical rhythm.
;;;;
;;;; by Leigh M. Smith <leigh@leighsmith.com> 
;;;;
;;;; Copyright (c) 2006-2014, All Rights Reserved.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; Known to work on SBCL 1.0.18
;;;;
;;;; See for background:
;;;;   author =  {Leigh M. Smith},
;;;;   title =   {A Multiresolution Time-Frequency Analysis and Interpretation of Musical Rhythm},
;;;;   school =  {Department of Computer Science, University of Western Australia},
;;;;   year =    2000,
;;;;   month =   {October},
;;;;   annote =  {\url{http://www.science.uva.nl/~lsmith/Papers/MultiresRhythm.pdf}}
;;;;

(require 'nlisp)			; For mathematics.
(require 'cli-parser)			; For command line option interpretation.

(defsystem :multiresrhythm
  :description "A Multiresolution Time-Frequency Analysis and Interpretation of Musical Rhythm"
  :version "2.8"
  :author "Leigh M. Smith"
  :license "Copyright (c) 2005-2014"
  :depends-on (:nlisp :cli-parser)
  :properties ((#:author-email . "leigh@leighsmith.com")
	       (#:date . "2014")	; Compute this automatically?
	       ((#:albert #:output-dir) . "documentation")
	       ((#:albert #:formats) . ("docbook")))
  :serial t
  :components ((:file "package")
	       (:file "signalprocessing")
	       (:file "ridges")
	       (:file "scorefile")
	       (:file "sound")
	       (:file "histogram")
	       (:file "martin-trees")
	       (:file "viterbi")
	       (:file "skeleton" :depends-on ("ridges"))
	       (:file "plotting" :depends-on ("ridges" "signalprocessing"))
	       (:file "rhythm" :depends-on ("scorefile" "plotting" "signalprocessing" "sound"))
	       (:file "cwt" :depends-on ("plotting" "ridges" "rhythm"))
	       (:file "tempo" :depends-on ("cwt" "signalprocessing"))
	       (:file "multires_rhythm" 
		      :depends-on ("cwt" "skeleton" "plotting" "rhythm" "signalprocessing" "tempo"))
	       (:file "scaleogram_plotting" :depends-on ("multires_rhythm" "cwt" "rhythm" "plotting"))
	       (:file "tactus-selection" :depends-on ("multires_rhythm" "scaleogram_plotting" "viterbi"))
	       (:file "rhythm-matching" :depends-on ("signalprocessing"))
	       (:file "downbeat" :depends-on ("multires_rhythm"))
	       (:file "clapping" :depends-on ("multires_rhythm" "downbeat" "tactus-selection" "cwt"))
	       (:file "perceptual-onsets" :depends-on ("clapping" "sound"))
	       (:file "expectancies" :depends-on ("clapping" "signalprocessing"))
	       (:file "meter" :depends-on ("multires_rhythm" "tactus-selection" "expectancies"))
	       (:file "metrical-expectancy" :depends-on ("expectancies" "martin-trees" "histogram"))
	       (:file "emcap-expectancy" :depends-on ("expectancies"))
	       (:file "commandline" :depends-on ("emcap-expectancy" "perceptual-onsets"))
))

