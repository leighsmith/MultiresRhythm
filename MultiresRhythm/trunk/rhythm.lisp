;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Rhythm utilities
;;;;
;;;; Leigh Smith <lsmith@science.uva.nl>
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

(defun intervals-in-samples (intervals &key ((:tempo tempo-in-bpm) 60)
			     (ioi 1.0 interval-supplied-p)
			     (sample-rate 1000)) ; in Hz (samples per second)
  "Converts a given rhythm (in relative interval values, 1.0 = the shortest interval) to
   samples, defaulting to milliseconds for 1000Hz."
  (let* ((shortest-interval-samples (if interval-supplied-p ioi (/ (* 60 sample-rate) tempo-in-bpm))))
    (mapcar #'(lambda (interval) (truncate (* shortest-interval-samples interval))) intervals)))

(defun iois-to-onsets (iois &optional (onset 0))
  (if iois
    (cons onset (iois-to-onsets (rest iois) (+ onset (first iois))))
    (list onset)))

;(iois-to-onsets '(1 3 2)) -> (0 1 4 6)

;; TODO generate vector as well as list
(defun onsets-to-grid (onsets)
  (loop with rhythm-grid = (make-list (1+ (first (last onsets))) :initial-element 0)
        for onset in onsets
        do (setf (elt rhythm-grid onset) 1)
        finally (return rhythm-grid)))

;(onsets-to-grid '(0 1 4 6)) -> (1 1 0 0 1 0 1)

(defun grid-to-onsets (grid)
  "From a binary grid returns onset locations of non-zero values"
  (loop
     for beat in grid
     counting beat into ioi
     when (not (zerop beat))
     collect ioi))

(defun onsets-to-iois (onsets)
  "From a set of onset times, returns the interonset intervals, the first derivative thereof"
  (if (null (second onsets))
      nil
      (cons (- (second onsets) (first onsets)) (onsets-to-iois (rest onsets)))))

;; TODO add an IOI parameter
(defun rhythmic-grid-to-signal (grid &key (tempo 80) (shortest-ioi 1.0 ioi-supplied-p) (sample-rate 200))
  "Returns rhythmic grid as a nlisp array."
  (nlisp::list-to-array (butlast 
			(onsets-to-grid
			 (iois-to-onsets 
			  (intervals-in-samples 
			   ;; Append an impulse to ensure trailing silences are counted.
			   (onsets-to-iois (grid-to-onsets (append grid '(1))))
			   :tempo tempo 
			   :sample-rate sample-rate))))))

(defun repeat-rhythm (rhythm repeat)
  (if (> repeat 0)
      (append rhythm (repeat-rhythm rhythm (1- repeat)))))

