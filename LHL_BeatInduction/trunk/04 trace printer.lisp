;****************************************************************************************
; pretty printing the computation trace
(in-package :shoe)

(defun  print-process-traces (model patterns &rest args)
  (format t "~%(with ~A)" args)
  (loop for (pattern name) in patterns
        do (format t "~2%Example ~A:~%" name)
        do (apply #'print-process-trace model pattern args)))

(defun print-process-trace (model pattern &rest args &key 
                                  tap-trace
                                  (stream t) 
                                  &allow-other-keys)
  (let* ((trace (apply model pattern :trace #'now-rule-t1-beat :allow-other-keys t args)))     
    (print-grid stream pattern "" 27)
    (print-traces trace pattern 27 t stream)
    (when tap-trace
      (loop for tap in (if (listp tap-trace) tap-trace (list tap-trace))
            do (let* ((taps (funcall tap trace (apply #'+ pattern)))
                      (tap-pattern (differentiate (cons 0 taps))))
                 (terpri stream)
                 (print-grid stream tap-pattern (format nil "~a" tap) 27 1 "." "|" "|"))))
    (values)))

(defun print-traces (traces rhythm collumn &optional (fill-in-beat t) (stream t))
  (loop with length = (apply #'+ rhythm)
        for trace in traces
        do (destructuring-bind (now rule t1 beat 
                                    &aux 
                                    (t3 (+ t1 (* 2 beat))))
                      trace
             (print-trace now rule t1 beat t3 
                          length collumn fill-in-beat stream t t))))

(defun print-state (state)
    (print-grid   t (pattern state) "" 27 1 "|" "|" "|" "." (now state))
    (print-trace (now state)(rule state)(t1 state)(beat state)(t3 state)(now state)
                   27 t t t nil))

(defun print-state-debug (state)
  (when (rule state)
    (print-grid   t (pattern state) "" 27 1 "|" "|" "|" "." (now state))
    (print-trace (now state)(rule state)(t1 state)(beat state)(t3 state)(now state)
                 27 t t t nil)))

(defun print-state-full (state)
  (when (rule state)
    (Format t "~2%")
    (print-state-debug state))) 



#|
(defun print-state-interactive-debug (state)
  (if (rule state)
    (progn (print-trace (now state)(rule state)(t1 state)(beat state)(t3 state)(now state)
                        27 t t t nil)
           (print-grid  t (pattern state) "" 27 1 "|" "|" "|" "." (now state)))
    (progn (when (eql (now state) 1)
             (print-grid  t (pattern state) "" 27 1 "|" "|" "|" "." 0)
             (control-key-wait))
           (format t (if (note-on-now? state) "|" "."))))
  (control-key-wait))

(defun control-key-wait ()
  (loop while (not (control-key-p)))
  (loop while (control-key-p)))

|#

(defun print-trace (now rule t1 beat t3
                        length collumn fill-in-beat stream number-times? print-*?)
  (when print-*? 
    (print-label "" (+ collumn now) stream)
        (format stream "*"))
  (format stream "~%~2A " now)
  (print-label (string rule) (- collumn 15) stream nil)
  (when beat 
    (format stream "=> [~2A ~2A]  " t1 beat)
    (loop for time from 1 to 3
          as size = (1+ t1) then beat
          do (print-section size (if number-times? (format nil "~A" time) "|") stream))
    (loop for i from 1 
          while (<= i (- length t3))
          when (and (zerop (mod i beat)) fill-in-beat)
          do (format stream "|")
          else do (format stream "."))))

(defun print-section (length delimiter stream)
  (loop repeat (1- length) do (format stream "."))
  (format stream delimiter))

(defun print-grid (stream l
                          &optional (label "") (collumn 0) (grid 1) 
                          (first-bar "|")(bar "|")(last-bar "|")(space ".")
                          (length (apply #'+ l)))
  (print-label label collumn stream)
  (format stream first-bar)
  (when l
    (loop repeat (1- (/ (first l) grid)) do (format stream space))
    (loop for x in (rest l) 
          do (format stream bar)
          do (loop repeat (1- (/ x grid)) do (format stream space)))
    (format stream "~A" last-bar))
  (loop repeat (- length (apply #'+ l)) do (format stream space)))

(defun print-label (label collumn stream &optional (fresh t))
  (when fresh (format stream "~%"))
  (format stream "~A" label)
  (loop repeat (- collumn (length label)) do (format stream " ")))

#|

(print-process-trace #'(lambda (x) (LHL82 x :trace #'now-rule-t1-beat)) 
             '(4 4 4 4 8 8 4 4 4 4 16))
(print-process-trace #'(lambda (x) (LHL82 x :trace #'now-rule-t1-beat)) 
             '(1 2 9 1 2 9 1 2 4 2 3 1 2))
(print-process-trace #'(lambda (x) (lee85 x :trace #'now-rule-t1-beat)) 
             '(1 2 9 1 2 9 1 2 4 2 3 1 2))
|#
