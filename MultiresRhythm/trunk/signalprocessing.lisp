;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Miscellaneous signal processing routines. 
;;;; They aren't strictly musical, but they aren't general enough to put into NLISP itself.
;;;;
;;;; Leigh M. Smith <lsmith@science.uva.nl>
;;;;
;;;; Copyright (c) 2006, 2007 All Rights Reserved.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; See multiresrhythm.asd for further info.

(in-package :multires-rhythm)
(use-package :nlisp)

(defun range (matrix)
  "Determines the range of values in the matrix"
  (- (.max matrix) (.min matrix)))

;;thresholdMag = magnitude > threshold;
;;newphase = (thresholdMag .* phase) + (!thresholdMag .* clamp);
(defun clamp-to-bounds (signal test-signal 
			&key (low-bound 0) (clamp-low 0) (high-bound 0 high-bound-supplied-p) (clamp-high 0))
  "Clip a signal according to the bounds given, by testing another signal (which can be the same as signal). 
Anything clipped will be set to the clamp-low, clamp-high values"
  (let* ((above-low (.> test-signal low-bound))
	 (below-high (if high-bound-supplied-p
			 (.< test-signal high-bound)
			 1d0)))
    (.+ (.* (.and above-low below-high) signal)
	(.* (.not above-low) clamp-low) 
	(.* (.not below-high) clamp-high))))

;; (setf a (.rseq2 0 9 10))
;; (setf b (.rseq2 9 0 10))
;; (clamp-to-bounds b a :low-bound 5d0 :clamp-low -1d0)

;; Only good for vectors, of course.
(defun .subseq (a start &optional end)
  (make-instance (class-of a) :ival (subseq (val a) start end)))
