;;;; $Id:$
;;;;
;;;; Rhythm utilities
;;;;
;;;; Leigh Smith <lsmith@science.uva.nl>
;;;;

(defun intervals-in-ms (intervals &key ((:tempo tempo-in-bpm) 60)
			(ioi 1.0 interval-supplied-p))
  "Converts a given rhythm (in relative interval values, 1.0 = the shortest interval) to milliseconds."
  (let* ((shortest-interval-milliseconds (/ 60000 tempo-in-bpm)))
    (mapcar #'(lambda (x) (truncate (* shortest-interval-milliseconds x))) intervals)))

(defun iois-to-onsets (iois &optional (onset 0))
  (if iois
    (cons onset (iois-to-onsets (rest iois) (+ onset (first iois))))
    (list onset)))

;(iois-to-onsets '(1 3 2)) -> (0 1 4 6)

;; TODO generate vector as well as list
(defun onsets-to-grid (onsets)
  (loop with rhythm = (make-list (1+ (first (last onsets))) :initial-element 0)
        for onset in onsets
        do (setf (elt rhythm onset) 1)
        finally (return rhythm)))

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
