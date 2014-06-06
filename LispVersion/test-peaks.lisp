;;;; -*- Lisp -*-
;;;;
;;;; Functions for plotting various signals.
;;;;
;;;; In nlisp (Matlab-like Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <leigh@leighsmith.com> 
;;;;
;;;; Copyright (c) 2006-2014, All Rights Reserved.
;;;;

(in-package :multires-rhythm)


;; Or use list of 3 element arrays for faster access?
(defmacro neighbour-low (x) `second ,x)
(defmacro neighbour-high (x) `third ,x)

(defun update-extreme-neighbourhood (x extrema-tuple)
  (cond ((= (- (aref extrema-tuple 1) x) 1)
	 (setf (aref extrema-tuple 1) x))
	((= (- x (aref extrema-tuple 2)) 1)
	 (setf (aref extrema-tuple 2) x))
	(t nil)))			; x is outside the neighbourhood

(defun update-extrema (index extrema)
  ;; if no current extreme point was updated, add a new extreme point.
  (if (every #'not (mapcar (lambda (extrema-tuple) (update-extreme-neighbourhood index extrema-tuple)) extrema))
      ;; it's a new maximum,create a new extrema-tuple
      (push (make-array 3 :initial-contents (list index index index)) extrema)
      extrema))

(defun extrema-points-of-vector (signal-to-analyse)
  (let* ((index-value-pairs (loop 
			       for y across (val signal-to-analyse)
			       for x from 0 below (.length signal-to-analyse)
			       collect (list x y)))
	 (max-values (sort index-value-pairs #'> :key #'cadr))
	 (indexes (mapcar #'car max-values)) ; perhaps this can be done more efficiently?
	 (first-maximum (first indexes)))
    (loop
       for index in (rest indexes)
       for extrema = (list (make-array 3 :initial-contents (list first-maximum first-maximum first-maximum)))
       then (update-extrema index extrema)
       finally (return (make-narray (mapcar (lambda (x) (aref x 0)) extrema))))))

(defun extrema-points-sorting (matrix array-dimension)
  "Return maxima and minima matrices."
  (declare (ignore array-dimension))
  ;; (declare (optimize (speed 3) (space 0) (safety 1)))
  (let* ((dims (.array-dimensions matrix))
	 (rows (.row-count matrix))
	 (columns (.column-count matrix))
	 (r (make-double-array dims))) ; (make-ninstance a result-length)
    (dotimes (c columns)
      (declare (type fixnum c))
      (let* ((extrema-of-column (extrema-points-of-vector (.column matrix c)))
	     (impulse-column (make-double-array rows)))
	(setf (.arefs impulse-column extrema-of-column) (make-double-array (.length extrema-of-column) :initial-element 1d0))
	(setf (.subarray r (list t (list c c))) impulse-column)))
    r))

;;; Experimental windowing max version
(defun extrema-points-windowing (matrix array-dimension)
  "Return maxima and minima matrices."
  (declare (ignore array-dimension))
  ;; (declare (optimize (speed 3) (space 0) (safety 1)))
  (let* ((window 6)
	 ;; (overlap (floor window 2))
	 (dims (.array-dimensions matrix))
	 (rows (.row-count matrix))
	 (columns (.column-count matrix))
	 (r (make-double-array dims)) ; (make-ninstance a result-length)
	 (r-val (val r)))
    (dotimes (c columns)
      (declare (type fixnum c))
      (dotimes (window-index (floor rows window))
	(let* ((window-position (* window-index window))
	       (window-coords (list (list window-position (min (1- rows) (+ window-position window -1))) c))
	       (max-r (argmax (.subarray matrix window-coords))))
	  ;; this skips the maximum at the window boundary, deferring it to the next window
	  (unless (or (zerop max-r) (= max-r (1- window)))
	    (setf (aref r-val (+ max-r window-position) c) 1d0)
	    ;; (format t "max ~a window contents ~a~%" (+ max-r window-position) (.subarray matrix window-coords))
	    ))))
    r))



(setf sample-rate 200.0d0)
(setf events (.load #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/RobertRich/Java Gourd 01.corneronsetsqrt-p1-f30-s10-g075" :format :text))
(setf times-in-seconds (.column events 0))
(setf times-as-rhythm (rhythm-of-onsets "java gourd" times-in-seconds :sample-rate sample-rate))
(setf analysis (analysis-of-rhythm times-as-rhythm))
(setf scaleogram (scaleogram analysis))
(setf mag (scaleogram-magnitude scaleogram))

(defun create-likely-tactus (analysis)
"Most likely is most confident and most precise"
(setf maxima (ridge-peaks analysis))
(setf minima (ridge-troughs analysis))
(setf peak-scales (.find (.column maxima time)))
(setf peak-widths (width-of-peaks peak-scales (.find (.column minima time))))

(setf peak-heights (.arefs (.column maxima time) peak-scales))
(setf normalized-precision (.- 1d0 (./ peak-widths (* 1d0 (.sum peak-widths)))))


collect (.aref peak-scales (.* normalized-precision peak-heights)) into most-likely

collect (.aref peak-scales (argmax (.* normalized-precision peak-heights))) into most-likely

)

((argmin peak-widths)

 (precision-widths 


(format t "onset times in samples ~a~%" times-in-samples)
(plot-scale-energy+peaks-at-time scaleogram 173 (ridge-peaks analysis))
(plot-scale-energy+peaks-at-time scaleogram 173 (ridge-troughs analysis))
(plot-scale-energy+peaks-at-time scaleogram 232 extrema)
(plot-scale-energy+peaks-at-time scaleogram 646 extrema)
(plot-scale-energy+peaks-at-time scaleogram 827 extrema)

(precisions-at-time analysis 173)

(.mod (.find (.transpose maxima)) (.row-count maxima))

(setf ex-col (.column mag 130))
(setf ex-col (time (extrema-points-of-vector the-column)))
(setf ex-col (.column mag 173))
(setf ex-col (.column mag 646))
(setf ex-col (.column mag 767))
(setf ex-col (.column mag 827))


;; extrema-points-of-vector is actually pretty cheap.
(time (dotimes (i 1000) (extrema-points-of-vector the-column)))

(plot (list ex-col (.* (.column extrema 130) ex-col)) nil :styles '("lines" "impulses") :aspect-ratio 0.2)

(plot (list ex-col (.* (.column minima 130) ex-col)) nil :styles '("lines" "impulses") :aspect-ratio 0.2)

;; produce points for interpolation
(defun interpolate-around (point signal-vector)
(points-for-interpolation (.subarray signal-vector '(0 ((1- point) (1+ point)))))


;;gsl_interp * gsl_interp_alloc (const gsl_interp_type * T, size_t size)
;; This function returns a pointer to a newly allocated interpolation object of type T for size data-points.
;;int gsl_interp_init (gsl_interp * interp, const double xa[], const double ya[], size_t size)
;;void gsl_interp_free (gsl_interp * interp)


(setf res4_2-trace-rhythm (plymouth-perceptual-salience-rhythm "res4/res4_2_resp_text"
							       "res4/res4_2_pOnsets_text"
							       :weighted nil))
(setf anal (analysis-of-rhythm salience-trace-rhythm))
(setf likely-ridges (confidence-and-precision anal))
(plot-image #'magnitude-image (list likely-ridges) '((1.0 0.5) (0.0 0.3)) "")

(setf res4_3-trace-rhythm (plymouth-perceptual-salience-rhythm "res4/res4_3_resp_text"
							       "res4/res4_3_pOnsets_text"
							       :weighted nil))
(setf anal (analysis-of-rhythm res4_3-trace-rhythm))
(setf likely-ridges (confidence-and-precision anal))

(setf window-likely-ridges (window-integration likely-ridges 800))

(plot examp-col nil :styles '("impulses") :aspect-ratio 0.2)
