;;;; -*- Lisp -*-
;;;;
;;;; $Id: multires_rhythm.lisp 356 2007-10-18 16:51:22Z leigh $
;;;; 
;;;; Meter determination.
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006, 2007 All Rights Reserved.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; See multiresrhythm.asd for further info.
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

#|

;; Calculates the persistency of period multiples in the skeleton.
(defun meter-evidence (analysis tactus beat-multiple)
  "Returns the evidence of harmonicity of the beat to the given beat-multiple."
  ;; Get the scales in the ridge and their relative ratios.
  ;; TODO using the first ridge in the list is a hack.
  (multiple-value-bind (tactus-scales scale-weights) (scales-and-weights-in-ridge (first tactus))
    (let* ((skeleton (skeleton analysis))
	   (vpo (voices-per-octave skeleton))
	   (persistency-profile (ridge-persistency-of analysis))
	   (beat-periods (time-support tactus-scales vpo))
	   (candidate-bar-periods (.* beat-periods beat-multiple))
	   (candidate-scales (prune-to-limit (.round (scale-from-period candidate-bar-periods vpo))
					     (1- (number-of-scales skeleton))))
;;	   (unweighted-meter-evidence (.sum (.arefs persistency-profile candidate-scales)))
	   (weighted-meter-evidence (if (> (.length candidate-scales) 0)
					(.sum (.* (.arefs persistency-profile candidate-scales) scale-weights))
					0d0)))
;;       (format t "beat-periods ~a candidate-bar-periods ~a scale-weights ~a scale-values ~a unweighted meter evidence ~a weighted meter evidence ~a~%" 
;; 	      beat-periods candidate-bar-periods scale-weights 
;; 	      (.arefs persistency-profile candidate-scales) unweighted-meter-evidence weighted-meter-evidence)
;;       (plot-command "set xtics (~{~{\"~5,2f\" ~5d~}~^, ~})~%" 
;; 		    (label-scale-as-time-support-seconds (skeleton analysis) (sample-rate analysis)))
;;       (plot (.reverse persistency-profile) nil :title (description analysis) :aspect-ratio 0.66 :reset nil)
      weighted-meter-evidence)))

(defun meter-of-analysis (analysis tactus)
  "Look for harmonicity of the beat, either duple or triple. Returns the selected beat multiple."
  (let* ((meters '(3 4))
	 (evidence (mapcar (lambda (beat-multiple) (meter-evidence analysis tactus beat-multiple)) meters)))
    (format t "meter evidence ~a~%" evidence)
    (nth (position (apply #'max evidence) evidence) meters)))
|#

;; Calculates the persistency of period multiples in the skeleton.
(defun meter-of-analysis (analysis tactus &key (meters '(3 4)))
  "Returns the evidence of harmonicity of the beat to the given beat-multiple."
  ;; Get the scales in the ridge and their relative ratios.
  ;; TODO using the first ridge in the list is a hack.
  (multiple-value-bind (tactus-scales scale-weights) (scales-and-weights-in-ridge (first tactus))
    (let* ((skeleton (skeleton analysis))
	   (vpo (voices-per-octave skeleton))
	   (persistency-profile (ridge-persistency-of analysis))
	   (beat-periods (time-support tactus-scales vpo))
	   (evidence (mapcar 
		      (lambda (beat-multiple)
			(let* ((candidate-bar-periods (.* beat-periods beat-multiple))
			       (candidate-scales (prune-to-limit (.round (scale-from-period candidate-bar-periods vpo))
								 (1- (number-of-scales skeleton)))))
			  ;; return the weighted meter evidence
			  (if (plusp (.length candidate-scales))
			      (.sum (.* (.arefs persistency-profile candidate-scales) scale-weights))
			      0d0)))
		      meters)))
      ;; (format t "meter evidence ~a~%" evidence)
      (nth (position (apply #'max evidence) evidence) meters))))

(defun meter-division (analysis tactus)
  "Returns whether the meter is duple or triple"
  (if (zerop (mod (meter-of-analysis analysis tactus) 2)) 2 3))

