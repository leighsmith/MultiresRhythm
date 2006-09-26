;;; $Id: Shmulevitch.lisp 4703 2006-02-09 23:04:31Z leigh $
;;;
;;; Taken from Handel '93
;;; First 13 patterns of table 1.
;;; Each 1 represents a tone, 0 represents a rest, the smallest IOI is 200ms.
;;;
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
    (1 0 0 1 0 0 0 1 0 1 0 1 0 0 0 0))

;;; TODO 
;;; Reduce each pattern to 12 beats in order to test against triplet meters. Drop the last
;;; 4 beats but then re-instate the terminating beat, leaving 13 beats.

;; Handel's non-metrical example on p372
;; (test-tactus-for-rhythm "handel-nonmetrical-1" '(1 0 1 0 0 0 1 0 0 1 0 1 0 0 0 0))
;;vs. 
;; (test-tactus-for-rhythm "handel-nonmetrical-2" '(1 0 1 0 0 1 0 0 0 1 0 1 0 0 0 0))
