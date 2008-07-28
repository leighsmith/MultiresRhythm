;;;; -*- Lisp -*-
;;;;
;;;; $Id$
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

(defmacro .last (a)
  `(.aref ,a (1- (.length ,a))))

;;; Removes the first interval to match the terminology used in the paper.
(defun normalise-score-positions (positions)
  "Given a set of score positions, returns normalised versions suitable for the ritardando function"
  (./ positions (.last positions)))

(defun score-positions-from-iois (iois)
  (make-narray (multires-rhythm::iois-to-onsets (rest iois))))

(defun plot-ritard (iois curvature final-tempo)
  ;; Produce score positions from the IOIs.
  (let ((score-positions (score-positions-from-iois iois)))
    (reset-plot)
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
;; (plot-ritard '(2 2 2 2 2 2) 1 1)

(defun tempo-to-times (iois tempos)
  "Converts an array of IOIs (in beats) and an array of tempos (in BPMs) to times in seconds"
  (.* iois (./ 60.0d0 tempos)))

(defun ritard-iois (iois curvature final-tempo-ratio global-tempo)
  (let* ((score-positions (score-positions-from-iois iois))
	 (ritarding-tempos (ritard (normalise-score-positions score-positions) curvature final-tempo-ratio)))
    (make-narray 
     (multires-rhythm::iois-to-onsets 
      (nlisp::array-to-list (tempo-to-times (make-narray iois) (.* global-tempo ritarding-tempos)))))))

;; (multires-rhythm::rhythm-of-onsets "ritard" (ritard-iois '(2 2 2 2 2 2) 2 0.6 120))
;; (setf x (multires-rhythm::rhythm-of-onsets "ritard" (ritard-iois '(1 1 1 1 1 1 1 1 1) 2 0.6 120)))
;; (multires-rhythm::plot-rhythm x)
;; (multires-rhythm::save-rhythm x)
;; (setf a (analysis-of-rhythm x :padding #'multires-rhythm::causal-pad))
;; (multires-rhythm::plot-analysis a)
;; (plot-cwt+skeleton-of dorys::a nil dorys::x)
;; (clap-to-rhythm x)

;; (setf y (multires-rhythm::rhythm-of-onsets "ritard" (ritard-iois '(1 2 1 1 1 3 2 1) 2 0.6 120)))
;; (multires-rhythm::plot-rhythm y)
;; (setf double-claps (clap-to-rhythm y :start-from-beat 1))

(defun rhythm-with-ritard (curvature final-tempo-ratio &key (tempo 120) (ritarding-rhythm '(1 1 1 1 1 1 1 1)))
  (multires-rhythm::append-rhythm 
   (multires-rhythm::iois-to-rhythm "iso" ritarding-rhythm :tempo tempo :sample-rate 200) ; '(1 1 1 1 1 1 1 1)
   (multires-rhythm::rhythm-of-onsets "ritard" (ritard-iois ritarding-rhythm curvature final-tempo-ratio tempo))))

(defun hear-ritard (named rhythm)
  (let ((rhythm-sound (multires-rhythm::sound-of rhythm 
						 #P"/Volumes/iDisk/Research/Data/Handclap Examples/cowbell.aiff")))
    (multires-rhythm::save-to-file rhythm-sound 
				   (make-pathname :directory "/Volumes/iDisk/Research/Data/Handclap Examples/"
						  :name named 
						  :type "wav"))))

;; (dorys::hear-ritard "ritard" (rhythm-with-ritard 7 0.6))
;; (dorys::hear-ritard "ritard-rhythm" (rhythm-with-ritard 2 0.6 :ritarding-rhythm '(1 2 1 1 1 3 2 1)))
