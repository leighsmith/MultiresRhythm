;;; LMS
(in-package :shoe)

(defun correct-fraction (&key n model (models (list model)) file pre correct min max step output (time-format "~A"))
  (terpri)
  (pprint `(correct-fraction :file ,file  :output ,output :n ,n :models ',models  :pre ',pre :correct ',correct :min ,min :max ,max :step ,step))
  (with-open-file (stream file)
    (monte-carlo 
     :n n 
     :result #'(lambda(pattern)(loop for model in models collect (funcall model pattern)))
     :input stream
     :pre-process pre
     :key #'(lambda (pattern input traces)
              (loop for trace in traces
                    for model in models
                    append 
                    (destructuring-bind (now rule t1 beat) (first (last trace))
                      (when (and (eql rule 'confirm) ; from confirm-time to max
                                 (funcall correct (used-part-of-pattern pattern now) t1 beat))
                        (loop with index = (list model 'confirmed)
                              for time from (* (ceiling (/ now step)) step) below max by step ; ? to max by step
                              collect (list index time))))
                    append 
                    (loop for (now1 rule1 t11 beat1) in trace ;now1 and now2 can be equal
                          for (now2 rule2 t12 beat2) 
                          in (append (rest trace) 
                                     (list (cons max (rest (first (last trace))))))
                          append
                          (loop for time from (* (ceiling (/ now1 step)) step)  ;VAN/TOT INTERVALLEN <-> step grootte
                                below (* (floor (/ now2 step)) step) ;to ?
                                by step ; below floor is niet goed voor eind
                                as sub = (used-part-of-pattern pattern now1)
                                when (funcall correct sub t11 beat1) ; correct arg now ipv sub pattroon uitrekenen.
                                collect (list model time)
                                ;else do (pprint (list 'sub sub 't11 t11 'beat1 beat1 'pattern pattern 'now1 now1 trace))
                                ))))
     :multiple t
     :test #'equal
     :printer (when output #'(lambda(result size)
                               (if (typep output '(or string pathname))
                                 (with-open-file (out output :direction :output :if-exists :supersede)
                                   (print-correct-fraction result models min max step size out time-format 'time-in-grid-units))
                                 (print-correct-fraction result models min max step size t time-format 'time-in-grid-units)))))))


(defun used-part-of-pattern (pattern now)
  (loop for item in pattern 
        sum item into end 
        while (<= end now)
        collect item))




(defmacro make-correct-fun-from-file (name file &optional allow-multiples (allow-sub-beats t))
  (unless (probe-file file)
    (error "Tree file not found ~A" file))
  `(let ((tree (with-open-file (stream ,file) (read stream))))
     (make-correct-fun-from-tree ,name tree
                                 ,allow-multiples
                                 ,allow-sub-beats)))

(defmacro make-correct-fun-from-tree (name tree max &optional allow-multiples (allow-sub-beats t))
  `(defun ,name (pattern t1 beat &key (parameter-factor 1) &allow-other-keys)
     (and 
      beat 
      (new-correct-tree-beat? ,tree 
                                (loop for x in pattern collect (/ (* x parameter-factor) 16))
                                (/ (* (mod t1 beat) parameter-factor) 16)
                                (/ (* beat parameter-factor) 16)
                                ,max
                                ,allow-multiples
                                ,allow-sub-beats))))


(defmacro MAKE-correct-level-fun-FROM-FILE (name file max &optional multiples)
  (unless (probe-file file)
    (error "Tree file not found ~A" file))
  `(let ((tree (with-open-file (stream ,file) (read stream))))
     (MAKE-correct-level-fun-FROM-tree ,name tree ,max ,multiples)))


(defmacro MAKE-correct-level-fun-FROM-tree (name tree max &optional multiples)
  `(defun ,name (pattern t1 beat &key (parameter-factor 1) &allow-other-keys)
     (and beat 
          (new-level-tree-beat? ,tree 
                                (loop for x in pattern collect (/ (* x parameter-factor) 16))
                                (/ (* (mod t1 beat) parameter-factor) 16)
                                (/ (* beat parameter-factor) 16)
                                ,max
                                ,multiples))))

;; (defun make-level-from-file (file max &optional multiples)
;;   (unless (probe-file file)
;;     (error "Tree file not found ~A" file))
;;   (let ((tree (with-open-file (stream file) (read stream))))
;;     #'(lambda (pattern t1 beat)
;;         (and pattern 
;;              beat 
;;              (new-level-tree-beat? tree pattern (mod t1 beat) beat max
;;                                multiples)))))

(defun make-correct-from-file (file &optional allow-multiples (allow-sub-beats t))
  (unless (probe-file file)
    (error "Tree file not found ~A" file))
  (let ((tree (with-open-file (stream file) (read stream))))
    #'(lambda (pattern t1 beat)
        (format t "pattern ~a t1 ~a beat ~a~%" pattern t1 beat)
        (and ;pattern 
             beat 
             (correct-tree-beat? tree (or pattern '(1)) (mod t1 beat) beat 
                                 allow-multiples allow-sub-beats)))))


;;; correct-tree-beat? check on script file. .LTF
;;; new-correct-

(defun correct-metric (pattern t1 beat)
  (and pattern beat (check-metric-t1-beat pattern t1 beat '(12 16 18 24) '(2 3))))

(defun correct-strict-pulse (pattern t1 beat)
  (and pattern beat (correct-strict-pulse-aux beat pattern (mod t1 beat))))

(defun correct-strict-pulse-aux (period pattern &optional (phase 0))
  (cond ((null pattern) t)
        ((zerop phase) (correct-strict-pulse-aux period pattern period))
        ((> (first pattern) phase) nil)
        (t (correct-strict-pulse-aux period (rest pattern)(- phase (first pattern))))))

(defun print-correct-fraction (results models min max step n stream time-format post)
    (loop initially (format t "~%; n = ~A~%" n)
          initially (loop ; initially (format stream "~A~Afirst-correct" post #\tab)
                      initially (format stream "~A~A" post #\tab)
                          for model in models do (format stream "~A~A-correct~A~A-confirmed-correct" #\tab model #\tab model))
          for duration from min to max by step
          as corrects = (loop for model in models collect (gethash (list model duration) results 0))
          as confirmed-corrects = (loop for model in models collect (gethash (list (list model 'confirmed) duration) results 0))
          do (format stream "~%")
          do (format stream time-format duration)
          do (loop for correct in corrects
                   for confirmed-correct in confirmed-corrects
                   do (format stream "~A~3$~A~3$" #\tab (/ correct n) #\tab (/ confirmed-correct n)))
          finally (progn
                     (format stream "~%>~%n")
                     (loop repeat (* 2 (length models))
                           do (format stream "~A~A" #\tab n)))
          finally (progn
                     (format stream "~%95%confidence")
                     (loop with confidence = (worst-case-confidence n .95)
                           repeat (* 2 (length models))
                           do (format stream "~A~3$" #\tab confidence)))))

#|

(CORRECT-fraction  
  :N
  40
  :FILE
  "analyses;data:National Anthems:anthems.data"
  :OUTPUT
  t
  :MODELs '(LH94)
  :PRE
  'SECOND
  :CORRECT
  (MAKE-correct-FROM-FILE "analyses;data:National Anthems:anthems.tree")
  :MIN
  0
  :MAX
  100
  :STEP
  5)

(CORRECT-fraction  
  :N
  40
  :FILE
  "analyses;data:metric:metric-2.data"
  :OUTPUT
  t
  :MODELs '(LH94)
  :PRE nil
  :CORRECT 'correct-metric
  :MIN
  0
  :MAX
  100
  :STEP
  5)

|#

#| can go if make-correct-fraction-over-data-sets is tested
(defun correct-fraction-over-data-sets (&key n min max step)
  ;no random files
  (loop with time = (get-universal-time)
        with pulse-files = (directory "analyses;data:pulse:*.data")
        with metric-files = (directory "analyses;data:metric:*.data")
        with P&K-files = (directory "analyses;data:P&K:*.data")
        with P&K-tree-files = (directory "analyses;data:P&K:*.tree")
        with anthem-files = (directory "analyses;data:National Anthems:*.data")
        with anthem-tree-files = (directory "analyses;data:National Anthems:*.tree")
        with musical-files = (append anthem-files P&K-files)
        with musical-tree-files = (append anthem-tree-files P&K-tree-files)
        with pres = (append (make-list (length musical-files) :initial-element #'second)
                            (make-list (length pulse-files) :initial-element nil)
                            (make-list (length metric-files) :initial-element nil))
        with corrects = (append (mapcar #'make-correct-from-file musical-tree-files)
                                (make-list (length pulse-files) :initial-element #'correct-strict-pulse)
                                (make-list (length metric-files) :initial-element #'correct-metric))
        with files = (append musical-files
                             pulse-files 
                             metric-files)
        with out = "analyses;results:CORRECT:"
        for file in files
        for pre in pres
        for correct in corrects
        do (correct-FRACTION
            :n n
            :file file
            :output (merge-pathnames out (merge-pathnames ".tbl" file))
            :models '(LH94 LHL82 L85)
            :pre pre
            :correct correct
            :MIN min
            :MAX max
            :STEP step)
        finally (collect-tables (merge-pathnames "*.tbl" out))
        finally (format t "~%; correct-fraction-over-data-sets took ")
        finally (let ((time (multiple-value-list (decode-universal-time (- (get-universal-time) time) 0))))
                  (format t "~A:~A:~A" (third time)(second time)(first time)))))

;(correct-fraction-over-data-sets :min 0 :max 100 :step 5 :n 10)
|#

(defun make-correct-fraction-over-data-sets (&key n (min 0)(max 100)(step 5))
  ;no random files
  (let ((out "analyses;results:CORRECT:"))
    (with-open-file (stream (concatenate 'string out "script") :direction :output :if-exists :supersede)
      (loop initially (let ((time (multiple-value-list (decode-universal-time (get-universal-time)))))
                        (format stream ";;; ~A/~A/~A ~A:~A:~A~%"
                                (fourth time)(fifth time)(sixth time)(third time)(second time)(first time)))
            with pulse-files = (directory "analyses;data:pulse:*.data")
            with metric-files = (directory "analyses;data:metric:*.data")
            with P&K-files = (directory "analyses;data:P&K:*.data")
            with P&K-tree-files = (directory "analyses;data:P&K:*.tree")
            with anthem-files = (directory "analyses;data:National Anthems:*.data")
            with anthem-tree-files = (directory "analyses;data:National Anthems:*.tree")
            with musical-files = (append anthem-files P&K-files)
            with musical-tree-files = (append anthem-tree-files P&K-tree-files)
            with pres = (append (make-list (length musical-files) :initial-element 'second)
                                (make-list (length pulse-files) :initial-element nil)
                                (make-list (length metric-files) :initial-element nil))
            with corrects = (append (mapcar #'(lambda(x) (list 'make-correct-from-file 
                                                               (unexpand-logical-directory x '("analyses;"))))
                                            musical-tree-files)
                                    (make-list (length pulse-files) :initial-element 'correct-strict-pulse)
                                    (make-list (length metric-files) :initial-element 'correct-metric))
            with files = (append musical-files
                                 pulse-files 
                                 metric-files)
            for file in files
            for pre in pres
            for correct in corrects
            do (pprint `(correct-FRACTION
                         :n ,n
                         :file ,(unexpand-logical-directory file '("analyses;"))
                         :output ,(merge-pathnames out (merge-pathnames ".tbl" file))
                         :models '(LH94 LHL82 L85)
                         :pre ',pre
                         :correct ,(if (listp correct) correct (list 'quote correct))
                         :MIN ,min
                         :MAX ,max
                         :STEP ,step)
                       stream)
            do (terpri stream)
            finally (pprint `(collect-tables ,(merge-pathnames "*.tbl" out)) stream)))))


;(make-correct-fraction-over-data-sets :n 1000)



