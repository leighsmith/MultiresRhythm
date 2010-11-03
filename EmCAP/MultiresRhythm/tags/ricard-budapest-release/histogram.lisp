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


