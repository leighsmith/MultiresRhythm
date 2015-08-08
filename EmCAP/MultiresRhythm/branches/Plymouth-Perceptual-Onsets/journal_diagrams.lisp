;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Diagrams for journal paper
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

;;; Figure 2 isochronous
(clap-to-rhythm (rhythm-of-grid "isochronous-rhythm" '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) :shortest-ioi 256 :sample-rate 200))
;;; Figure 4 
(clap-to-octave-file "longuet_cliche" :sample-rate 200)
;;; Figure 5
(clap-to-octave-file "intensity34_to_44" :sample-rate 200)
;;; Figure 6 Agogic phrasing.
(clap-to-rhythm (iois-to-rhythm "4-beat-lengthen-corr" (repeat-rhythm '(61 75 75 89) 12)))
;;; Figure 7 Agogic phrasing.
(clap-to-rhythm (iois-to-rhythm "3-beat-lengthen-corr" (repeat-rhythm '(61 75 89) 16)))

;;; Until we fix the over long ridge bug, this is a work around to select the right ridge.
;;; (clap-to-rhythm (iois-to-rhythm "3-beat-lengthen-corr" (repeat-rhythm '(61 75 89) 16)) :tactus-selector (lambda (skeleton) (ridge-containing-scale-and-time skeleton 94 0)))
;;;
;;; (setf 3-beat-rhythm (iois-to-rhythm "3-beat-lengthen-corr" (repeat-rhythm '(61 75 89) 16)))
;;; (setf analysis (analysis-of-rhythm 3-beat-rhythm :voices-per-octave 16))
;;; (setf longest-ridge (select-longest-lowest-tactus (skeleton analysis)))
;;; (duration-in-samples 3-beat-rhythm)
;;; (duration-in-samples longest-ridge) ; This seems wrong.

;;; Figure 8
(clap-to-octave-file "greensleeves-perform-medium" :sample-rate 400 :description "Greensleeves performed")

;;; How we get the locations of the semi-quavers:
;;; (setf greensleeves-rhythm (load-rhythm-file "greensleeves-perform-medium" :description "greensleeves" :sample-rate 400))
;;; (onsets-in-seconds greensleeves-rhythm)
;;; 1.74 3.6 9.07 10.91
;;; (setf greensleeves-tactus (tactus-for-rhythm greensleeves-rhythm))
;;; (./ (time-support (scales-as-array greensleeves-tactus) voices-per-octave) (sample-rate greensleeves-rhythm))
;;; (plot (./ (time-support (scales-as-array greensleeves-tactus) 16) 400.0) nil :aspect-ratio 0.15)
