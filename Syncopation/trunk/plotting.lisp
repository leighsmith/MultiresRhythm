;;;; $Id$
;;;;
;;;; Connects to gnuplot using a pipe to plot supplied lists.
;;;;
;;;; Leigh Smith <lsmith@science.uva.nl>

(in-package :syncopation)

;;; TODO needs unwind-protect?

;;; Perhaps initialise this with run-program? Makes it harder to control when
;;; gnuplot is first run.
(defvar *gnuplot-process-stream* nil)

;;; Set the gnuplot terminal. This could be X11, aqua or other more
;;; exotic output formats. The default suits MacOS X.
(defparameter *plot-terminal-type* "aqua")

;;; Can be replaced with NLISP
(defun plot-initialise ()
  (setf *gnuplot-process-stream* 
	#+sbcl (external-process-input-stream (sb-ext:run-program "/sw/bin/gnuplot" () :wait nil :input :stream :output nil))
	#+lispworks (system:run-shell-command '("/sw/bin/gnuplot") :wait nil :input :stream :output nil)))

;;; Can be replaced with NLISP
(defun plot-close ()
  (format *gnuplot-process-stream* "quit~%"))

;;; Handles vectors or lists
(defun format-for-plotting (plot-stream data-points)
  (do ((data-index 0 (1+ data-index)))
      ((>= data-index (length data-points)) nil)
    (format plot-stream "~f~%" (elt data-points data-index)))
  (format plot-stream "e~%"))

;;; Can be replaced with NLISP
(defun plot-command (command-string)
  (format *gnuplot-process-stream* "~a~%" command-string))

;;; convert to nplot:
;;; labels => legends, title the same, figure-number => window-number
(defun plot (data-points &key (ranges "") labels (title "") (figure-number 0))
  "Plots the given data points using the gnustep external process. 
Data can be a list of numbers, a vector, or a list of lists or vectors
to plot multiple data-points. Key parameters are a string label for each
signal, the title of the entire plot and which window 'figure' to plot
it in."
  (let* ((plot-stream *gnuplot-process-stream*)
	 ;; check if a list of lists or vectors is supplied vs. just a single list.
	 (data-points (if (not (listp (first data-points))) (list data-points) data-points))
	 (labels (if (not (listp labels)) (list labels) labels))
	 ;; pad the labels with nils if they are less than the number of data-points
	 (padded-labels (if (> (length data-points) (length labels))
			    (append labels (make-list (- (length data-points) (length labels))))
			    (subseq labels 0 (length data-points)))))
    (format plot-stream "set terminal ~a ~a title \"~A\"~%"
	    *plot-terminal-type* figure-number title)
    (format plot-stream "set title \"~a\"~%" title)
    ;; TODO should make the plotting style more flexible.
    (format plot-stream "set style data lines~%")
    ;; iterate over elements of data-points retrieving labels, separating by ","
    ;; TODO perhaps "unlabelled data ~:*~#[a]" need current iteration
    ;; printed so each unlabelled signal is distinguished.
    ;; (format plot-stream "plot ~a ~{\"-\" title \"~:[unlabelled data~;~:*~a~]\" with linespoints ~^, ~}~%" 
    (format plot-stream "plot ~a ~{\"-\" title \"~:[unlabelled data~;~:*~a~]\" with linespoints linewidth 2 ~^, ~}~%" 
	    ranges padded-labels)
    (loop
       for data-points in data-points
       do (format-for-plotting plot-stream data-points)
       finally (finish-output plot-stream))))

