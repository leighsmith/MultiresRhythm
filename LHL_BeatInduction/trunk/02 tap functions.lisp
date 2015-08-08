;****************************************************************************************
; trace to tap functions (for foot-tapper and print-trace)

#|
(print-process-trace #'lhl82 '(1 2 9 1 2 9 1 2 4 2 3 1 2) :max-beat 30 :tap-trace 
                     '(t3s t2s tns passed-t3s passed-tns last-beat confirmed-beat filled-in-t3s))

(print-process-trace #'lhl82 '(6 2 4 4 4 4 2 2 4) :max-beat 30 :tap-trace 
                     '(t3s t2s tns passed-t3s passed-tns last-beat confirmed-beat filled-in-t3s))
|#

(defun t3s (trace &rest ignore)
  (remove-duplicates
   (loop for (now rule t1 beat) in trace 
         collect (+ t1 (* 2 beat)))))

(defun t2s (trace &rest ignore)
  (remove-duplicates 
   (loop for (now rule t1 beat) in trace 
         collect (+ t1 beat))))

(defun tns (trace &rest ignore)
  (sort (remove-duplicates 
         (append (t2s trace)(t3s trace)))
        #'<))

(defun passed-t3s (trace &optional ignore)
  (loop for (now rule t1 beat) in trace 
        for (next-now next-rule next-t1 next-beat) in (rest trace)
        as t3 = (+ t1 (* 2 beat))
        when (<= t3 next-now)
        collect t3))

(defun passed-tns (trace &optional duration &rest ignore)
  (loop with nexts = (append (loop for (now rule t1 beat) in (rest trace)
                                          unless (eql rule 'confirm) collect now)
                                    (list duration))
        for (now rule t1 beat) in trace 
        for next in nexts
        append (loop for tn from (+ t1 (* 2 beat)) by beat
                     while (<= tn next) 
                     collect tn)))

(defun last-beat (trace duration &optional (more 2))
  (when trace
    (destructuring-bind (now rule t1 beat) (first (last trace))
      (make-beat (+ t1 (* 3 beat)) beat duration more))))

(defun confirmed-beat (trace duration &optional (more 2))
  (when (and trace (eql (second (first (last trace))) 'confirm))
    (last-beat trace duration more)))

(defun make-beat (start beat duration more)
  (if (>= start duration)
    (loop repeat more
          for time from start by beat
          collect time)
    (cons start (make-beat (+ start beat) beat duration more))))

(defun filled-in-t3s (trace &optional duration &rest ignore)
  (fill-in-beats duration (t3s trace duration)))

(defun fill-in-beats (duration onsets &aux (iois (differentiate onsets)))
  (let* ((filled-in (cons (first iois)
                          (loop for ioi in iois
                                for next in (rest iois)
                                when (and (< ioi next) (zerop (mod next ioi)))
                                append (make-list (/ next ioi) :initial-element ioi)
                                else append (list next))))
         (end-time (first (last onsets)))
         (last-ioi (first (last iois)))
         (rest-duration (- duration end-time))
         (tail-iois (when (< 0 rest-duration)
                      (make-list (ceiling (/ rest-duration last-ioi)) :initial-element last-ioi))))
    (rest (integrate (append filled-in tail-iois)))))