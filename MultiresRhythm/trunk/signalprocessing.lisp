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

(defun prune-to-limit (a limit &key (test #'<=))
  "Remove any elements which are above the limit value"
  (make-instance (class-of a) :ival (remove-if-not (lambda (x) (funcall test x limit)) (val a))))

(defun prune-outliers (a &key (upper-limit nil upper-limit-supplied) (lower-limit nil lower-limit-supplied))
  "Remove any elements which are above the limit value"
  (make-instance (class-of a) 
		 :ival (remove-if-not (lambda (x) (and (if upper-limit-supplied (<= x upper-limit) t)
						       (if lower-limit-supplied (>= x lower-limit) t)))
				      (val a))))

(defun pad-end-to-length (vector length)
  "Pad a vector with zeros out to the length given"
  (let ((pad-length (- length (.length vector))))
    (.concatenate vector (make-double-array pad-length))))

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

(defun rotate-vector (sig &key (by (round (.length sig) 2))) ; default to mirroring
  "Return a signal with elements rotated by the given number of places. Any elements shifted beyond the end of
  the vector will be shifted into the start and vice-versa. Positive :by values shift
  elements to the left, negative values shift elements to the right. Default reflects the
  signal around its middle point"
  (let ((rotate-by (if (plusp by) by (+ (.length sig) by))))
    (.concatenate (.subseq sig rotate-by) (.subseq sig 0 rotate-by))))

;;; Generalise over any dimension.
(defun reduce-dimension (a fn &optional (dimension 1))
  "Applies the given function to each column, storing the result in a vector"
  (loop
     with result = (nlisp::narray-of-type a (.array-dimension a dimension))
     for column-index from 0 below (.array-dimension a dimension)
     do (setf (.aref result column-index) (funcall fn (.column a column-index)))
     finally (return result)))

;;; Really good candidate to replace with a BLAS routine...
;;; Actually this should become a macro using (reduce-dimension a #'.+)
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

;;; We have to use two columns since NLISP otherwise reduces a 1 row x n column matrix
;;; into a vector.
;;; TODO this is backwards, we should do the inner maxima finding on a vector and then
;;; iterate that for a matrix. The problem is how to process each row of a matrix
;;; efficiently without a copy.
(defun extrema-points-vector (vector &rest arguments)
  "Find the extrema points of a vector by making it a two column matrix"
  (let* ((two-column-matrix (make-double-array (list 2 (.length vector)))))
    (setf (.subarray two-column-matrix '(t t)) vector)
    (.column (apply #'extrema-points (.transpose two-column-matrix) arguments) 0)))

;; Compare against:
;; (setf local-maxima (make-narray '(2.0 -34.0 9.0 -8.0 15.0 2.0 -1.0)))
;; (defun find-local-maxima (vector) (.+ (.find (.< (.diff (.signum (.diff vector))) 0)) 1))

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

;;; Obsolete now it is within NLISP.
;;(defmethod normalise ((vector n-array))
;;  "Normalisation"
;;  (./ vector (.max vector)))

(defun map-narray (func vector)
  (make-instance 'n-double-array :ival (map (type-of (val vector)) func (val vector))))

;;; TODO Surely there must exist something similar already?
(defun impulses-at (impulse-indices total-vector-length)
  "Return a vector with impulses at the locations in an array of total length"
  (let ((new-vector (make-double-array total-vector-length)))
    (setf (.arefs new-vector impulse-indices) 
	  (make-double-array (.length impulse-indices) :initial-element 1d0))))

(defun phase-wrap (phase-value-to-wrap)
  "Wraps values within a modulo +/- pi (phase) system."
  (.- (.mod (.+ phase-value-to-wrap pi) (* 2 pi)) pi))

(defun significant (signal &key (threshold 0.5))
  "Find all values greater than a threshold number of standard deviations above the mean"
  (.> signal (+ (mean signal) (* (stddev signal) threshold))))

(defun .remove-duplicates (a)
  (make-instance (class-of a) :ival (remove-duplicates (val a))))

(defun remove-arefs (narray indices-to-remove)
  "Returns a new array with the values at the indicated indices removed. Assumes indices-to-remove is sorted."
  (let* ((remove-count (.length indices-to-remove))
	 (thinned-array (nlisp::narray-of-type narray (- (.length narray) remove-count))))
    (loop 
       for array-index from 0 below (.length narray)
       with thinned-array-index = 0
       with remove-index = 0
       do 
	 (if (and (< remove-index remove-count) 
		  (= array-index (.aref indices-to-remove remove-index)))
	     (incf remove-index)
	     (prog1 (setf (.aref thinned-array thinned-array-index) (.aref narray array-index))
	       (incf thinned-array-index))))
    thinned-array))

(defun .sorted-indices (value-vector &key (ordering #'>))
  "Return the indices of the vector sorted in descending or ascending value, depending on ordering."
  (let* ((prominent-indices (.find value-vector))) ; TODO this is a problem with -ve values.
    (make-instance (class-of prominent-indices) 
		   :ival (sort (val prominent-indices) ordering :key (lambda (x) (.aref value-vector x))))))

(defun highest-peaks (value-vector)
  "Return the indices of the peaks sorted in descending value"
  (.sorted-indices (.* (extrema-points-vector value-vector :extrema :max) value-vector) :ordering #'>))

#|
(defun filter (numerator denominator input initial-state)
  "Filter input with the coefficients defined as numerator and denominator vectors with an
  initial state"
      (if (= (length numerator) (length denominator))
	(if (> (length numerator) 0)
	  T &zend = initial-state(initial-state.endC());
	  T &zstart = initial-state(0);
          typename Vec2::const_elem_iterator xel = input.elbegin();      
	  for(int ii =0; ii<len; ++ii,++xel) (
            typename Vec2::value_type xx = *xel;
	    Out(ii) = bstart * xx + zstart;
	    zr0   = zr1;	
	    zend = 0.0;
	    zbr  += xx * numerator - Out(ii) * denominator;	    
	  )	
	)
	(
	  Out = bstart * input;
	)	
      )
|#
