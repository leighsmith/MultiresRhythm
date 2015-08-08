(defun agreed-fraction 
       (&key n 
             models
             file 
             pre 
             args
             (same-beat #'same-beat?)
             min 
             max 
             step 
             output 
             (time-format "~A")
             (incremental t))
  (terpri)
  (pprint `(agreed-fraction :n ,n
                :models ',models
                :file ,file
                :pre ,pre
                :args ',args
                :same-beat ,same-beat
                :min ,min
                :max ,max
                :step ,step 
                :output ,output
                :time-format ,time-format))
  (let ((eq-names (destructuring-bind (modela modelb modelc) models
                    (mapcar #'intern (list (format nil "~A=~A=~A" modela modelb modelc)
                                           (format nil "~A=~A" modela modelb)
                                           (format nil "~A=~A" modelb modelc)
                                           (format nil "~A=~A" modela modelc)
                                           (format nil "UNEQUAL")
                                           )))))
    (with-open-file (stream file)
      (monte-carlo
       :n n 
       :result #'(lambda(pattern &rest args)
                   (loop for model in models collect (apply model pattern args)))
       :input stream
       :pre-process pre
       :args args
       :key #'(lambda (pattern input traces)
                (declare (ignore pattern input))
                (collect-grids min max step traces eq-names same-beat))
       :multiple t
       :test #'equal
       ; voor printer evt butlast eq-names neme om unequal niet te printen
       :printer (when output #'(lambda(result size)
                                 ;(maphash #'(lambda(index entry)(when (eql (first index) 90)(print (list index entry)))) result)
                                 (if (typep output '(or string pathname))
                                   (with-open-file (out output :direction :output :if-exists :supersede)
                                     (print-agreed-fraction result eq-names min max step size out time-format 'time-in-grid-units incremental))
                                   (print-agreed-fraction result eq-names min max step size t time-format 'time-in-grid-units incremental))))))))

(defun print-agreed-fraction (results eq-names min max step n stream time-format post &optional (incremental t))
    (loop initially (format t "~%; n = ~A~%" n)
          initially (loop initially (format stream "~A" post #\tab)
                          for eq-name in eq-names do (format stream "~A~A" #\tab eq-name))
          for duration from min to max by step
          as agrees-raw = (loop for eq-name in eq-names 
                            collect (gethash (list duration eq-name) results 0))
          as agrees = (if incremental (loop for agree in agrees-raw
                                            sum agree into score
                                            collect score)
                          agrees-raw)
          do (format stream "~%")
          do (format stream time-format duration)
          do (loop for agree in agrees do (format stream "~A~3$" #\tab (/ agree n)))
          finally (progn
                     (format stream "~%n")
                     (loop repeat (length agrees) do (format stream "~A~3$" #\tab n)))
          finally (progn
                     (format stream "~%95%confidence")
                     (loop with confidence = (worst-case-confidence n .95)
                           repeat (length agrees)
                           do (format stream "~A~3$" #\tab confidence)))))

(defun collect-grids (min max step traces eq-names same-beat)
  (when (<= min max)
    (cons (list min (agree-beat (loop for trace in traces 
                                             collect (list (third (first trace))
                                                           (fourth (first trace))))
                                 eq-names
                                 same-beat))
          (collect-grids (+ min step) max step (loop for trace in traces
                                                      when (and (second trace)(>= (+ min step) (first (second trace))))
                                                      collect (rest trace)
                                                      else collect trace)
                         eq-names same-beat))))

(defun agree-beat (results eq-names same-beat)
  (destructuring-bind ((t1a beata)(t1b beatb)(t1c beatc)) results
    (let* ((eqab (funcall same-beat t1a beata t1b beatb))
           (eqbc (funcall same-beat t1c beatc t1b beatb))
           (eqac (funcall same-beat t1c beatc t1a beata))
           (eqabc (and eqab eqbc)))
      (loop for eq in (list eqabc eqab eqbc eqac t) ; t eruit
            for name in eq-names
            when eq do (return name)))))

(defun agree-beat (results eq-names same-beat) ;new
  (destructuring-bind ((t1a beata)(t1b beatb)(t1c beatc)) results
    (let* ((eqab (and beata beatb (funcall same-beat t1a beata t1b beatb)))
           (eqbc (and beatc beatb (funcall same-beat t1c beatc t1b beatb)))
           (eqac (and beata beatc (funcall same-beat t1c beatc t1a beata)))
           (eqabc (and eqab eqbc)))
      (loop for eq in (list eqabc eqab eqbc eqac t) ; t eruit
            for name in eq-names
            when eq do (return name)))))

(defun same-beat? (starta durationa startb durationb)
  (or (and (null durationa) (null durationb))
      (let ((min (min durationa durationb)))
        (and (eql (mod starta min)
                  (mod startb min))
             (multiple? min (max durationa durationb))))))

(defun multiple? (a b)
  (zerop (mod b a)))

;(same-beat? 1 2 3 4)
;(same-beat? 0 8 0 4)
;(same-beat? 0 12 0 4)
;(same-beat? 7 16 3 4)
;(same-beat? '(0 12) '(0 3))
;(same-beat? '(1 12) '(1 3))
;(same-beat? '(0 4) '(1 8))
;(same-beat? '(1 8) '(0 4))
;(same-beat? '(0 2) '(4 2))
;(same-beat? '(0 2) '(4 8))
;(same-beat? '(1 2) '(4 8))  

(defun make-agreed-fraction-over-data-sets (&key n (min 0) (max 100) (step 5))
  (let ((out "analyses;results:AGREEMENT:"))
    (with-open-file (stream (concatenate 'string out "script") :direction :output :if-exists :supersede)
      (loop initially (let ((time (multiple-value-list (decode-universal-time (get-universal-time)))))
                        (format stream ";;; ~A/~A/~A ~A:~A:~A~%"
                                (fourth time)(fifth time)(sixth time)(third time)(second time)(first time)))
            with random-files = (directory "analyses;data:random:*.data") 
            with pulse-files = (directory "analyses;data:pulse:*.data")
            with metric-files = (directory "analyses;data:metric:*.data")
            with P&K-files = (directory "analyses;data:P&K:*.data")
            with anthem-files = (directory "analyses;data:National Anthems:*.data")
            with musical-files = (append anthem-files P&K-files)
            with pres = (append (make-list (+ (length random-files) (length metric-files) (length pulse-files)) 
                                           :initial-element nil)
                                (make-list (length musical-files) :initial-element 'second))
            with args = (append (make-list (+ (length random-files) (length metric-files) (length pulse-files)) 
                                           :initial-element nil)
                                (make-list (length musical-files) 
                                           :initial-element 
                                           'factor-from-score))
            with files = (append random-files pulse-files metric-files musical-files)
            for file in files
            for pre in pres
            for arg in args
            do (pprint `(agreed-FRACTION   
                         :n ,n
                         :file ,(unexpand-logical-directory file '("analyses;"))
                         :output ,(merge-pathnames out (merge-pathnames ".tbl" file))
                         :models '(LHL82 L85 LH94) ;'(LH94 LHL82 longnote-model)
                         :pre ',pre
                         :args ',arg
                         :MIN ,min
                         :MAX ,max
                         :STEP ,step)
                       stream)
            do (terpri stream)
            finally (pprint `(collect-tables ,(merge-pathnames "*.tbl" out)) stream)))))
  
;(make-agreed-fraction-over-data-sets :n 1000)


  


