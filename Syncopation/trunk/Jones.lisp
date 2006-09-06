;;;;
;;;; $Id:$
;;;;
;;;; Examples from Jones & Boltz 1989

;;; J&B timing patterns of the 5 variations of a folk tune shown in their figure 6.
(defconstant *JB-ex6*
  ;; J&B example A
  '(:h          (4 2 2 2 1 1 2 2 4 2 2 2 2 2 2 4 2 2 2 2 1 1 1 1 4 2 2 1 1 1 1 2 2 4)
    ;; example B
    :nh-early   (4 2 2 2 1 1 2 2 4 2 2 2 2 2 2 4 1 1 1 1 1 1 1 1 4 2 2 2 2 2 2 2 2 4)
    ;; example C
    :nh-late    (4 2 2 2 1 1 2 2 4 2 2 2 2 2 2 4 2 2 2 2 2 2 2 2 4 1 1 1 1 1 1 1 1 4)
    ;; example D
    :nh-early-h (4 2 1 1 1 1 2 2 2 4 2 2 2 2 2 2 4 1 1 2 1 1 2 2 2 4 2 2 2 1 1 2 2 4)
    ;; example E
    :nh-late-h  (4 2 2 2 2 2 2 4 2 2 2 2 2 2 4 2 1 1 2 2 1 1 2 4 2 1 1 1 1 1 1 2 2 4)))

; (play-rhythm (getf *jb-ex6* :h) :ioi 200)
; (play-rhythm (getf *jb-ex6* :nh-early) :ioi 200)
; (play-rhythm (getf *jb-ex6* :nh-late) :ioi 200)
; (play-rhythm (getf *jb-ex6* :nh-early-h) :ioi 200)
; (play-rhythm (getf *jb-ex6* :nh-late-h) :ioi 200)

;;; Grouping examples, listeners should attempt to tap to these.
(defconstant *group-by-2* '(1 2 1 2 1 2))
(defconstant *group-by-3* '(1 1 2 1 1 2 1 1 2))
(defconstant *group-by-4* '(1 1 1 2 1 1 1 2 1 1 1 2))
(defconstant *group-by-5* '(1 1 1 1 2 1 1 1 1 2 1 1 1 1 2))

; (play-rhythm (repeat-rhythm *group-by-5* 4) :ioi 200)

;;; Irregular 4 interval (5 beat) sequences used by Drake & Botte 1993
;;; In milliseconds.
(defconstant *db-ex2*
  '(:g1a (546 465 534 455)
    :g1b (470 542 460 528)
    :g2a (506 430 495 569)
    :g2b (585 498 423 494)
    :g2c (505 581 494 420)
    :g2d (435 500 575 490)
    :g3a (400 460 529 611)
    :g3b (628 534 453 385)))
