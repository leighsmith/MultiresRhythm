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

;;; Obsolete now we have a stats package access within NLISP/GSL
;;(defun mean (a)
;;  (/ (.sum a) (.length a)))

(defun prune-to-limit (a limit)
  "Remove any elements which are above the limit value"
  (make-instance (class-of a) :ival (remove-if-not (lambda (x) (<= x limit)) (val a))))

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

;;; Really good candidate to replace with a BLAS routine...
(defun .partial-sum (a &key (dimension 1)) 
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (let* ((result-length (.array-dimension a dimension))
	 (rows (.array-dimension a 0)) ; TODO HACK!
	 (r (make-double-array result-length)) ; (make-ninstance a result-length)
	 (a-val (val a))
	 (r-val (val r)))
    (dotimes (j result-length)
      (dotimes (i rows)
	(declare (type fixnum i j))
	(setf (aref r-val j) (+ (aref r-val j) (aref a-val i j))))) 
    r))

(defun extrema-points (matrix &key (extrema :max))
  "Returns a matrix of the same size as passed in, with peaks of the rows marked"
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (let* ((dims (.array-dimensions matrix))
	 (rows (.row-count matrix))
	 (r (make-double-array dims)) ; (make-ninstance a result-length)
	 (a-val (val matrix))
	 (r-val (val r))
	 (comparison-fn (if (eql extrema :max) #'> #'<)))
    (declare (type fixnum rows))
    (declare (type (simple-array double-float *) r-val a-val))
    (dotimes (c (.column-count matrix))
      (declare (fixnum c))
      (dotimes (r (- rows 3))
	(declare (fixnum r))
	(let* ((r+1 (1+ r))
	       (center (aref a-val r+1 c)))
	  (declare (type double-float center))
	  (setf (aref r-val r+1 c) (if (and (funcall comparison-fn center (aref a-val r c)) 
					    (funcall comparison-fn center (aref a-val (+ r 2) c))) 1d0 0d0)))))
    r))

(defun cumsum (a)
  "Computes the cumulative summation across each row of the matrix"
  (let* ((array-dimensions (.array-dimensions a))
	 (a-val (val a))
	 (cumsum (nlisp::narray-of-type a array-dimensions))
	 (cumsum-val (val cumsum)))
    (dotimes (r (first array-dimensions))
      (setf (aref cumsum-val r 0) (aref a-val r 0))
      (loop 
	 for c from 1 below (second array-dimensions)
	 do (setf (aref cumsum-val r c) (+ (aref cumsum-val r (1- c)) (aref a-val r c)))))
    cumsum))

;; (setf a (cumsum (make-double-array '(2 100) :initial-element 1d0)))

(defun window-integration (a width)
  "Computes the windowed integration across each row of the matrix to a maximum width"
  (let* ((array-dimensions (.array-dimensions a))
	 (a-val (val a))
	 (cumsum (nlisp::narray-of-type a array-dimensions))
	 (cumsum-val (val cumsum)))
    (dotimes (r (first array-dimensions))
      (setf (aref cumsum-val r 0) (aref a-val r 0))
      (loop 
	 for c from 1 below (second array-dimensions)
	 do (setf (aref cumsum-val r c) 
		  (+ (aref cumsum-val r (1- c)) (aref a-val r c)
		     (if (> c width) (- (aref a-val r (- c width))) 0)))))
    cumsum))

;; (setf a (window-integration (make-double-array '(2 100) :initial-element 1d0) 10))
