(use-package :nlisp)
(setf c (.concatenate (.transpose (.iseq2 0 5)) (.transpose (.iseq2 0 5)))) ; a transition is sampled mid-point
(image (.* c 1d0) nil nil :reset nil)


(setf a (.transpose (.iseq2 0 5)))
(setf b (.transpose (.iseq2 0 5)))
(setf d (.- 5 b))
(setf e (.- 5 b))
(setf f (.concatenate d e)) ; either direction
(image (.* f 1d0) nil nil :reset nil)
(setf g (.concatenate a e)) ; but a peak is properly sampled.
(image (.* g 1d0) nil nil)
(reset-plot)
(plot-command "set cbrange [0:20]")
(nlisp::palette "model RGB")
(nlisp::palette "model HSV")
(nlisp::palette "defined ( 0 0 0 1, 1 1 0 0, 2 0 0 0 )")
(nlisp:palette-defined '((0 "#FFFFFF") (1 "#000000")))
(nlisp::palette "model RGB maxcolors 5")
(nlisp::palette "defined (0 0 0.5 0, 1 0 1 0, 1 0 1 1, 4 0 0.5 0.5, 4 1 0 0, 5 0.5 0 0, 5 1 0 0 )")
(nlisp:palette-defined '((0 "#FFFFFF") (0.5 "#000000") (1 "#FFFFFF")))

(setf d (make-fixnum-array '(6 6) :initial-element 1))
(setf e (make-fixnum-array '(6 6) :initial-element 5))
(setf f (.concatenate d e d)) ; either direction


;; Just gnuplot alone
;; set pm3d corners2color
;; set view map
;; splot "test.dat"
;; isolines could be the issue.
