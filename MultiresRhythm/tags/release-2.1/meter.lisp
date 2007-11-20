;;;; -*- Lisp -*-
;;;;
;;;; $Id$
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

