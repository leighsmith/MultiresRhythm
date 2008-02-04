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


#|

;; Problem is, 6 times the beat-period is typically longer than the maximum scale, so
;; there will be less examples of energy. If there is, say due to a longer analysis
;; window, it is likely the energy will be more due to low frequency signals, than
;; necessarily longer periods.

;; Simply sums the magnitude modulus contributions at multiples of the candidate periods.
(defun meter-of-scale-profile (scale-profile beat-scale voices-per-octave)
  "Look for harmonicity of the beat, either duple or triple. Returns the selected beat multiple."
  (let* ((beat-period (time-support beat-scale voices-per-octave))
	 (max-period (time-support (1- (.length scale-profile)) voices-per-octave))
	 (max-multiple (floor max-period beat-period))
	 ;; Ensure there are no candidate bar scales that exceed the total scales, but
	 ;; check all multiples up to 7/8 periods.
	 ;; (beat-multiples (.iseq (min 2 max-multiple) (min 7 max-multiple)))
	 ;;TODO  must check if these exceed max-multiple.
	 (beat-multiples (make-instance 'n-fixnum-array :ival #2A((2 4) (3 6))))
	 (candidate-bar-periods (.* beat-period beat-multiples))
	 (candidate-bar-scales (.round (scale-from-period candidate-bar-periods voices-per-octave)))
	 ;; look at the scale persistency profiles at candidate-bar-scales locations
	 (bar-scale-profiles (.arefs scale-profile candidate-bar-scales))
	 (meter-evidence (.partial-sum (.transpose bar-scale-profiles)))
	 (meter-index (argmax meter-evidence)))
    (plot scale-profile nil)
    (format t "candidate bar periods ~a bar-scale-profiles ~a meter evidence ~a~%"
	    candidate-bar-periods bar-scale-profiles meter-evidence)
    (.aref beat-multiples meter-index 0)))
|#
