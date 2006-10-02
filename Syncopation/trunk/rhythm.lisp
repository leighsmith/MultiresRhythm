;;;; $Id$
;;;;
;;;; Rhythm utilities
;;;;
;;;; Leigh Smith <lsmith@science.uva.nl>
;;;;

(in-package :syncopation)

(defun intervals-in-ms (intervals &key ((:tempo tempo-in-bpm) 60)
			(ioi 1.0 interval-supplied-p))
  "Converts a given rhythm (in relative interval values, 1.0 = the shortest interval) to milliseconds."
  (let* ((shortest-interval-milliseconds (/ 60000 tempo-in-bpm)))
    (mapcar #'(lambda (x) (truncate (* shortest-interval-milliseconds x))) intervals)))

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

(defun repeat-rhythm (rhythm-to-repeat repeat)
  (if (> repeat 0)
      (append rhythm-to-repeat (repeat-rhythm rhythm-to-repeat (1- repeat)))))

