;;; $Id$
;;;
;;; Taken from Handel '93
;;; First 13 patterns of table 1.
;;; Each 1 represents a tone, 0 represents a rest, the smallest IOI is 200ms.
;;;
(in-package :syncopation)

(defparameter *handel-patterns*
  '((1 0 0 1 0 0 1 0 1 0 0 1 0 0 0 0)
    (1 0 0 1 0 0 1 0 0 1 0 1 0 0 0 0)
    (1 0 1 0 0 0 1 0 0 1 0 1 0 0 0 0)
    (1 0 1 0 0 0 1 0 1 0 0 1 0 0 0 0)
    (1 0 1 0 0 1 0 1 0 0 0 1 0 0 0 0)
    (1 0 0 1 0 1 0 1 0 0 0 1 0 0 0 0) 
    (1 0 1 0 0 1 0 0 0 1 0 1 0 0 0 0)
    (1 0 0 1 0 1 0 0 1 0 0 1 0 0 0 0)
    (1 0 1 0 1 0 0 1 0 0 0 1 0 0 0 0)
    (1 0 0 1 0 1 0 0 0 1 0 1 0 0 0 0)
    (1 0 1 0 0 1 0 0 1 0 0 1 0 0 0 0)
    (1 0 1 0 1 0 0 0 1 0 0 1 0 0 0 0)
    (1 0 0 1 0 0 0 1 0 1 0 1 0 0 0 0)))

;;; TODO 
;;; Reduce each pattern to 12 beats in order to test against triplet meters. Drop the last
;;; 4 beats but then re-instate the terminating beat, leaving 13 beats.

;; Handel's non-metrical example on p372
;; (multires-rhythm::tactus-for-rhythm-grid "handel-nonmetrical-1" '(1 0 1 0 0 0 1 0 0 1 0 1 0 0 0 0))
;;vs. 
;; (multires-rhythm::tactus-for-rhythm-grid "handel-nonmetrical-2" '(1 0 1 0 0 1 0 0 0 1 0 1 0 0 0 0))

;; 
(defun tactus-for-handel-pattern (pattern-number)
  (multires-rhythm::tactus-for-rhythm-grid (format nil "handel-pattern-~d" pattern-number)
					   (repeat-rhythm (nth pattern-number *handel-patterns*) 2)))

;; Must do two repetitions.
; (multires-pattern-complexity "handel-pattern" *handel-patterns*)

;;;; From Handel '98
;;;; Rhythms which were hard to discriminate when presented with a metrical co-occuring pulse:
;Metronome '(1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0)
;First     '(1 0 1 0 0 0 1 0 0 1 0 0 1 0 0 0)
;Second    '(1 0 1 0 0 0 1 0 0 0 1 0 1 0 0 0)

