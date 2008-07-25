;;;; -*- Lisp -*-
;;;;
;;;; $Id: metrical-expectancy.lisp 506 2008-07-23 09:44:44Z leigh $
;;;;
;;;; Functions for generating kinematic ritards.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2008
;;;;

(in-package :dorys)
(use-package :nlisp)
(use-package :multires-rhythm)

(defun ritard (score-positions curvature final-tempo)
  "Calculates a kinematic ritard. Accepts normalized score positions, returns normalized tempo positions"
 (.expt (.+ 1d0 (.* (- (expt final-tempo curvature) 1.0d0) score-positions)) (/ 1.0d0 curvature)))

;;; (defmacro .last (a) `(.aref ,a (1- (.length ,a))))

;;; Removes the first interval to match the terminology used in the paper.
(defun normalise-score-positions (positions)
  "Given a set of score positions, returns normalised versions suitable for the ritardando function"
  (./ positions (.last positions)))

(defun plot-ritard (iois curvature final-tempo)
  ;; Produce score positions from the IOIs.
  (let ((score-positions (make-narray (multires-rhythm::iois-to-onsets (rest iois)))))
    (plot-command "set yrange [0.5:1.1]")
    (plot-command "set xrange [0:14]")
    (plot (ritard (normalise-score-positions score-positions) curvature final-tempo)
	  (.+ (first iois) score-positions) 
	  :style "linespoints" 
	  :reset nil)))

;; (plot-ritard '(4 4 4) 2 0.8)
;; (plot-ritard '(3 3 3 3) 2 0.8)
;; (plot-ritard '(2 2 2 2 2 2) 2 0.8)
;; (plot-ritard '(4 4 4) 2 0.6)
;; (plot-ritard '(3 3 3 3) 2 0.6)
;; (plot-ritard '(2 2 2 2 2 2) 2 0.6)

;; (plot-ritard '(1 2 1 1 1 3 2 1) 2 0.6)
;; (plot-ritard '(1 2 1 1 1 3 2 1) 1 1)
;; (plot-ritard '(2 2 2 2 2 2) 2 0.6)
