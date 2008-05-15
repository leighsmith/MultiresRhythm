;;;; -*- Lisp -*-
;;;;
;;;; $Id: metrical-expectancy.lisp 472 2008-05-13 20:23:34Z leigh $
;;;;
;;;; Routines to draw Martin trees.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2008
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

(defun diagnostic (start-point end-point)
  (format t "draw from ~a to ~a~%" start-point end-point))

(defun draw-tree (meter start-point branch-height width &key (plotter #'diagnostic))
  ;; start-point is a list arranged x, y.
  (let ((divisions (first meter))
	(next-y (+ (second start-point) branch-height)))
    (funcall plotter start-point (list (first start-point) next-y))
    (unless (null divisions) ;; at leaf, pen up.
      (dotimes (subbranch divisions)
	(let* ((branch-width (/ (coerce width 'float) divisions))
	       (next-x (+ (first start-point) (* subbranch branch-width))))
	  (draw-tree (rest meter) (list next-x next-y) branch-height branch-width :plotter plotter)
	  (unless (= (1+ subbranch) divisions)
	    (funcall plotter (list next-x next-y) (list (+ next-x branch-width) next-y))))))))

;; (draw-tree '() '(0 0) 1 16)
;; (draw-tree '(2 2 2 2) '(0 0) 1 16)

(let ((sequential-line-segments '(() ())))
  (defun build-gnuplot-segments (start-point end-point)
    "Just rearranges the line segments into two separate axes"
    ;; This can definitely be done more elegantly!!
    (push (first start-point)  (first sequential-line-segments))
    (push (first end-point)    (first sequential-line-segments))
    (push (second start-point) (second sequential-line-segments))
    (push (second end-point)   (second sequential-line-segments)))

  (defun plot-tree-old (meter)
    (setf sequential-line-segments '(() ()))
    (draw-tree meter (list 0 (1+ (length meter))) -1 (reduce #'* meter) :plotter #'build-gnuplot-segments)
    (plot (make-narray (second sequential-line-segments))
	  (make-narray (first sequential-line-segments)))))

(let ((arrow-count 0))
  (defun arrow-plots (start-point end-point)
    "Draws the line segments (gnuplot headless arrows)"
    ;; TODO could probably make the colour and width a line type so it doesn't have to be here.
    (plot-command "set arrow ~a from ~{~f~^,~} to ~{~f~^,~} nohead linewidth 4 linecolor rgb \"#7F85FF\"" 
		  (incf arrow-count) start-point end-point))

  (defun plot-martin-tree (meter maximum-width maximum-height &key (start-x 0))
    "Reset the plot and invert to plot in correct orientation"
    (let ((branch-height (/ (coerce maximum-height 'float) (1+ (length meter)))))
      (setf arrow-count 0)
      (draw-tree meter (list start-x maximum-height) (- branch-height) maximum-width :plotter #'arrow-plots)
      (dotimes (arrow arrow-count)
	(plot-command "show arrow ~a" arrow)))))

(defun test-tree (meter)
  (window)
  (reset-plot)
  (plot-martin-tree meter 1.0 0.5 :start-x 8)
  (plot (.rseq 0 1 20) nil :reset nil)
  (close-window))
