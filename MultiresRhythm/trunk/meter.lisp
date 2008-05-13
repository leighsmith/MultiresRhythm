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


(defun scales-of-meter (candidate-meters beat-period vpo max-scale)
  "Given a set of candidate meters and a beat period, return the scales that the meters should have energy at"
  (declare (ignore candidate-meters))
  (loop
     for meter-multiples in '((2 4 12) (2 4 8 16) (2 6 12)) ; TODO kludged for (3/4 4/4 6/8)
     ;; for meter in candidate-meters
     ;; for meter-multiples = (make-narray (reverse (apply #'* meter))))
     ;; (apply #'* '(2 2 3) '(2 3 1))
     for meter-periods = (.* (make-narray meter-multiples) beat-period)
     ;; clip below the lowest freq. scale.
     collect (prune-to-limit (.round (scale-from-period meter-periods vpo)) max-scale)))

;;; TODO Perhaps should be meter-of-analysis
(defun meter-of-rhythm (rhythm-to-analyse &key (candidate-meters '((3 2 2) (2 2 2 2) (2 3 2))))
  "Computes the expectancies using the maximum
   value of the cumulative sum of the scaleogram energy" 
  (let* ((last-time (1- (duration-in-samples rhythm-to-analyse)))
	 (analysis (analysis-of-rhythm rhythm-to-analyse :padding #'causal-pad))
	 (scaleogram (scaleogram analysis))
	 (vpo (voices-per-octave scaleogram))
 	 (persistency-profile (./ (cumsum (scaleogram-magnitude scaleogram))
				  (duration-in-samples rhythm-to-analyse)))
	 (final-persistency-profile (.column persistency-profile last-time))
	 (scale-peaks (determine-scale-peaks persistency-profile))
	 (last-peaks (.column scale-peaks last-time))
	 (beat-period (time-support (argmax last-peaks) vpo))
	 (scales-to-check (scales-of-meter candidate-meters beat-period vpo (1- (number-of-scales scaleogram))))
	 (persistency-for-scales (mapcar (lambda (scales) (.arefs final-persistency-profile scales)) scales-to-check))
	 (evidence-for-meters (make-narray (mapcar #'mean persistency-for-scales))))
    (format t "beat period of maximum last cumulative scale peaks ~a~%" beat-period)
    (format t "scales to check ~a~%" scales-to-check)
    ;; Must normalize by the number of multipliers used otherwise 4/4 will always outgun 3/4.
    (format t "persistency ~a~%mean ~a~%" persistency-for-scales evidence-for-meters)
    (nth (argmax evidence-for-meters) candidate-meters)))

(defparameter *meter-names* 
  (let ((meter-names (make-hash-table :test #'equal)))
    (setf (gethash "3/4" meter-names) '(3 2 2))
    (setf (gethash "4/4" meter-names) '(2 2 2 2))
    (setf (gethash "6/8" meter-names) '(2 3 2))
    (setf (gethash "2/4" meter-names) '(2 2 2))
    (setf (gethash "3/8" meter-names) '(3 2))
    (setf (gethash "6/4" meter-names) '(2 3 2))
    (setf (gethash "3/2" meter-names) '(2 3 2))
    meter-names))

(defun meter-for-name (meter-name)
  "Returns the metrical subdivisions that the name of the meter implies"
  (gethash meter-name *meter-names*))

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
