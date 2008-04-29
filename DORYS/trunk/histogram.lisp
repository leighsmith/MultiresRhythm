;;;; -*- Lisp -*-
;;;;
;;;; $Id:$
;;;;
;;;; Functions for creating and retrieving a histogram of data values.
;;;;
;;;; In Common Lisp
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006
;;;;

(in-package :dorys)

(defun add-to-histogram-hashtable (element-count-hash data)
  (map nil (lambda (element) (incf (gethash element element-count-hash 0))) data)
  element-count-hash)

(defun make-histogram-hashtable (data)
  "Returns each unique value in data along with the count of it's occurrence in a hash-table"
  (let ((element-count-hash (make-hash-table :size (length data))))
    (add-data-to-histogram-hashtable element-count-hash data)))

(defun display-interval-counts (histogram-hash-table)
  (maphash (lambda (key count) (format t "duration ~a count ~a~%" key count)) histogram-hash-table))

;; (display-interval-counts (make-histogram (second (anthem-named 'america))))

(defun make-histogram-of-anthem-intervals (&key (anthems *national-anthems*))
  "Returns each unique value in data along with the count of it's occurrence in a hash-table"
  (let ((element-count-hash (make-hash-table)))
    (dolist (anthem anthems)
      (map nil
	   (lambda (element) (incf (gethash element element-count-hash 0)))
	   (mapcar (lambda (x) (/ x (float (anthem-beat-duration anthem) 1d0))) (second anthem))))
    element-count-hash))

(defun get-histogram (histogram-hash-table)
  "Returns a sorted list of histogram counts from the hash-table"
  (let ((all-keys '())
	(sorted-keys))
    (maphash #'(lambda (key value) (declare (ignore value)) (push key all-keys)) histogram-hash-table)
    (setf sorted-keys (sort all-keys #'<))
    (values sorted-keys (loop for key in sorted-keys collect (gethash key histogram-hash-table)))))

(defun get-histogram-counts (histogram-hash-table)
  (maphash (lambda (key count) (format t "duration ~a count ~a~%" key count)) histogram-hash-table))

;; (display-interval-counts (make-histogram-of-anthem-intervals :anthems (anthems-of-meter "3/4")))

