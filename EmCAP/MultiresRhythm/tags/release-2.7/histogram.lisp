;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for creating and retrieving a histogram of data values.
;;;;
;;;; In Common Lisp
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006
;;;;

(in-package :multires-rhythm)

(defun add-to-histogram (element-count-hash data)
  (map nil (lambda (element) (incf (gethash element element-count-hash 0))) data)
  element-count-hash)

(defun make-histogram (data)
  "Returns each unique value in data along with the count of it's occurrence in a hash-table"
  (let ((element-count-hash (make-hash-table :size (length data))))
    (add-to-histogram element-count-hash data)))

(defun print-elements-and-counts (histogram-hash-table)
  (maphash (lambda (key count) (format t "element ~a count ~a~%" key count)) histogram-hash-table))

;; (print-elements-and-counts (make-histogram (second (anthem-named 'america))))

(defun get-histogram (histogram-hash-table)
  "Returns a sorted list of the histogram keys and counts from the hash-table"
  (let ((all-keys '())
	(sorted-keys))
    (maphash #'(lambda (key value) (declare (ignore value)) (push key all-keys)) histogram-hash-table)
    (setf sorted-keys (sort all-keys #'<))
    (values sorted-keys (loop for key in sorted-keys collect (gethash key histogram-hash-table)))))

(defun get-histogram-counts (histogram-hash-table)
  (second (multiple-value-list (get-histogram histogram-hash-table))))

(defun get-histogram-elements (histogram-hash-table)
  (values (get-histogram histogram-hash-table)))

(defun plot-histogram (histogram description)
  "Plots the histogram"
  (multiple-value-bind (elements counts) (get-histogram histogram)
    (reset-plot)
    (plot-command "set title font \"Times,24\"")
    (plot-command "set xlabel offset 0,-1 font \"Times,24\"")
    (plot-command "set ylabel font \"Times,24\"")
    ;; (plot-command "set xrange [*:~d]" (1+ largest-element))
    (plot-command "set yrange [*:1.1]")
    (plot (make-narray counts) (make-narray elements)
	  :style "boxes fill solid border 9"
	  :label "Relative Frequency of Occurrence of Elementss"
	  :xlabel "Elements"
	  :ylabel "Proportion of Element Present"
	  :title (format nil "Occurrence of Elements For ~a" description)
	  :reset nil
	  :aspect-ratio 0.66)))

;;; Sum all values falling within the given bins ranges. Effectively integrate.
;;; 
(defun binned-values (times values-at-times &key (bin-size 0.025))
  "Group the values into bins, specified by bin-size. Returns
   the number of elements in each bin, the accumulated values and the boundaries."
  ;; round up to capture all times, including a perfectly dividing edge.
  (let* ((number-of-bins (1+ (floor (range times) bin-size)))
	 (last-bin (+ (.min times) (* (1- number-of-bins) bin-size))) ; for low edge of bin.
	 (bin-boundaries (.rseq (.min times) last-bin number-of-bins))
	 (accumulated-values (make-double-array number-of-bins))
	 (bin-counts (make-double-array number-of-bins)))
    (loop
       for bin-index from 0 below number-of-bins
       ;; determine which elements in times (& therefore values-at-times) are within the bin.
       for in-bin = (.and (.>= times (.aref bin-boundaries bin-index))
			  ;; catch the edge of last bin.
			  (.< times (if (= bin-index (1- number-of-bins))
					(+ last-bin bin-size) 
					(.aref bin-boundaries (1+ bin-index)))))
       for in-bin-count = (.sum in-bin)  ; number of occurances.
       do (progn
	    ;; (format t "from ~a to ~a ~a values~%" (.aref bin-boundaries (1- bin-index)) (.aref bin-boundaries bin-index) in-bin-count)
	    (setf (.aref accumulated-values bin-index) (.sum (.* in-bin values-at-times)))
	    (setf (.aref bin-counts bin-index) in-bin-count))
       finally (return (values bin-counts bin-boundaries accumulated-values)))))

(defun plot-binned-histogram (times title &key (bin-size 0.025))
  (multiple-value-bind (counts time-bins)
      (binned-values times (make-integer-array (.length times) :initial-element 1) :bin-size bin-size)
    (format t "count ~a~%time-bins ~a~%" counts time-bins)
    (window)
    (reset-plot)
    (plot-command "set title font \"Times,24\"")
    (plot-command "set xlabel font \"Times,24\"")
    (plot-command "set ylabel font \"Times,24\"")
    (plot-command "set xtics ~a out" (max bin-size 0.2))
    ;; Downsample the bin labels.
    ;; (.arefs time-bins (.* (.iseq 0 (/ (.length time-bins) 2)) 2))
    (plot counts time-bins
	  :style "boxes fill solid 1.0 border -1"
	  :xlabel title
	  :ylabel "Occurrence"
	  :label (format nil "~a Occurrence" title)
	  :aspect-ratio 0.66
	  :reset nil
	  :title (format nil "Occurrence of ~a" title)))
  (close-window))
