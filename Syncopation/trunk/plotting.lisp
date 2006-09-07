;;;; $Id$
;;;;
;;;; Connects to gnuplot using a pipe to plot supplied lists.
;;;;
;;;; Leigh Smith <lsmith@science.uva.nl>

(in-package :syncopation)

;;; TODO needs unwind-protect?

;;; Perhaps initialise this with run-program? Makes it harder to control when
;;; gnuplot is first run.
(defvar *gnuplot-process* nil)

;;; Set the gnuplot terminal. This could be X11, aqua or other more
;;; exotic output formats. The default suits MacOS X.
(defparameter *plot-terminal-type* "aqua")

(defun plot-initialise ()
  (setf *gnuplot-process* 
	(run-program "/sw/bin/gnuplot" () :wait nil :input :stream :output nil)))

(defun plot-close ()
  (format (external-process-input-stream *gnuplot-process*) "quit~%"))

;;;  ((signal-external-process *gnuplot-process* 9))

;;; Handles vectors or lists
(defun format-for-plotting (plot-stream data-points)
  (do ((data-index 0 (1+ data-index)))
      ((>= data-index (length data-points)) nil)
    (format plot-stream "~f~%" (elt data-points data-index)))
  (format plot-stream "e~%"))

(defun plot-command (command-string)
  (format (external-process-input-stream *gnuplot-process*) "~a~%" command-string))

(defun plot (signals &key (ranges "") labels (title "") (figure-number 0))
  "Plots the given data points using the gnustep external process. 
Data can be a list of numbers, a vector, or a list of lists or vectors
to plot multiple signals. Key parameters are a string label for each
signal, the title of the entire plot and which window 'figure' to plot
it in."
  (let* ((plot-stream (external-process-input-stream *gnuplot-process*))
	 ;; check if a list of lists or vectors is supplied vs. just a single list.
	 (signals (if (not (listp (first signals))) (list signals) signals))
	 (labels (if (not (listp labels)) (list labels) labels))
	 ;; pad the labels with nils if they are less than the number of signals
	 (padded-labels (if (> (length signals) (length labels))
			    (append labels (make-list (- (length signals) (length labels))))
			    (subseq labels 0 (length signals)))))
    (format plot-stream "set terminal ~a ~a title \"~A\"~%"
	    *plot-terminal-type* figure-number title)
    (format plot-stream "set title \"~a\"~%" title)
    ;; TODO should make the plotting style more flexible.
    (format plot-stream "set style data lines~%")
    ;; iterate over elements of signals retrieving labels, separating by ","
    ;; TODO perhaps "unlabelled data ~:*~#[a]" need current iteration
    ;; printed so each unlabelled signal is distinguished.
    ;; (format plot-stream "plot ~a ~{\"-\" title \"~:[unlabelled data~;~:*~a~]\" with linespoints ~^, ~}~%" 
    (format plot-stream "plot ~a ~{\"-\" title \"~:[unlabelled data~;~:*~a~]\" with linespoints linewidth 2 ~^, ~}~%" 
	    ranges padded-labels)
    (loop
       for data-points in signals
       do (format-for-plotting plot-stream data-points)
       finally (finish-output plot-stream))))

