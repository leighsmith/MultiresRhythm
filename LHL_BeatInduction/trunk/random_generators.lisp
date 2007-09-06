; generator-creators for random patterns
(in-package :shoe)

;**************************************************************************

(defvar *input-set-stream*)
(setf *input-set-stream* nil) ;name vergelkijken

(defun pattern-from-file (&key file (key #'identity) &allow-other-keys)
  (unless *input-set-stream* (setf *input-set-stream* (open file)))
  (let ((data (read *input-set-stream* nil :EOF)))
    (if (eql data :EOF) 
      (progn (close *input-set-stream*)
             (setf *input-set-stream* nil)
             data)
      (funcall key data))))

(defun pattern-from-file-with-answer (&key file (max-beat 50) &allow-other-keys)
  (unless *input-set-stream* (setf *input-set-stream* (open file)))
  (let ((data (read *input-set-stream* nil :EOF)))
    (if (eql data :EOF) 
      (progn (close *input-set-stream*)
             (setf *input-set-stream* nil)
             data)
      (destructuring-bind ((country time-sign ignore dum1 duration dum2 start) pattern)
                          data
        (values pattern (fitting-beats time-sign duration start max-beat))))))

(defvar *input-set*)
(setf *input-set* nil)

(defun pattern-with-certain-duration-from-file (&key file duration min max step)
  (unless (equalp file (rest *input-set*))
    (setf *input-set* (cons (read-patterns-from-file file) file)))
  (loop with patterns = (first *input-set*)
        as random-duration = (or duration (random-duration min max step))
        as random-pattern = (elt patterns (random (length patterns)))
        as sub-pattern = (head-of (second random-pattern) random-duration)
        ;do (format t "~% min ~A max ~A duration ~A" min max duration)
        when (and sub-pattern (> random-duration 0))
        do (return sub-pattern)))

(defun head-of (pattern duration)
  (loop for ioi in pattern
        as time = ioi then (+ time ioi)
        collect ioi into result
        when (= time duration) do (return result)
        when (> time duration) do (return)))

;(head-of '(1 2 1 2 2 1 1 2 2) 5)

(defun read-patterns-from-file (file)
  (with-open-file (stream file :direction :input)
    (loop as data = (read stream nil :EOF)
          if (eql data :EOF) 
          do (return result)
          else collect data into result)))

;(pattern-with-certain-duration-from-file :file "MC analyses;data sets:National Anthems:anthem rhythms" :duration 6)

;**************************************************************************

(defun fitting-beats (time-signature bar-duration start-at 
                                     &key (max-beat 60) (multiples nil)
                                     (allow-sub-beats t))
  ;strictly fitting beats
  (collect-beats (fitting-beat-durations time-signature bar-duration allow-sub-beats) 
                 bar-duration start-at max-beat :multiples multiples))

(defun fitting-beat-durations (time-signature bar-duration &optional (allow-sub-beats t))
  (let ((divs (time-sig-divs time-signature)))
    (when (null divs)(warn "unkown time-signature ~A" time-signature))
    (metre-to-durations divs bar-duration allow-sub-beats)))

(defun time-sig-divs (time-signature)
  (or (rest (assoc time-signature '(("3/4" 3 2)
                                ("3/2" 3 2)
                                ("6/8" 2 3)
                                ("2/4" 2 2)
                                ("4/4" 2 2)
                                ("2/2" 2 2)
                                ("9/8" 3 3)
                                ("12/8" 3 2))
                   :test #'string=))
      (error "unkown time-signature ~A" time-signature)))
  

;(fitting-beats "4/4" 48 36)
;(fitting-beat-durations "4/4" 48)

(defun collect-beats (beat-durations bar-duration start-at max-beat &key (multiples t))
  (append 
   (when multiples
     (loop with durations = (multiple-beat-durations bar-duration max-beat)
           for duration in durations
           append (loop for start from start-at below duration by bar-duration
                        collect (list (mod (- duration start) duration) duration))))
   (loop for beat-duration in beat-durations
         as start = (mod start-at beat-duration)
         collect (list (mod (- beat-duration start) beat-duration) beat-duration))))

(defun metre-to-durations (divs bar-duration &optional (allow-sub-beats t))
  (if allow-sub-beats 
    (list* bar-duration 1
           (loop with rest = (loop repeat (ceiling (log (/ bar-duration (apply #'* divs)) 2)) collect 2)
                 for div in (append divs rest)
                 as duration = (/ bar-duration div) then (/ duration div)
                 while (and (> duration 1) (integerp duration))
                 collect duration))
    (list bar-duration (/ bar-duration (first divs)))))
            

;(METRE-TO-DURATIONs '(2 2) 48)
;(log (/ 48 4) 2)

;(METRE-TO-DURATIONS '(2 3) 12) 

;(metre-to-durations '(2 3) 48)
;(metre-to-durations '(2 2) 36)

(defun multiple-beat-durations (d max)
  (loop for mult from 2
        as new = (* d mult)
        while (<= new max)
        collect new))



#|

;(fitting-beats "6/8" 12 0) =>
((0 24) (12 24)
 (0 48) (12 48) (24 48) (36 48)
 (0 36) (12 36) (24 36) 
 (0 12) (0 6) (0 2))

;(fitting-beats "6/8" 12 9) =>
((15 24) (3 24) 
 (39 48) (27 48) (15 48) (3 48) 
 (27 36) (15 36) (3 36) 
 (3 12) (3 6) (1 2))

(fitting-beats "2/4" 36 0 100) =>
((0 72) (36 72) (0 36) (0 18) (0 9))
|#

;**************************************************************************

(defun metre-marks (metre)
  (cond ((string= metre "2/4") '(2 2 2 2 ))
        ((string= metre "4/4") '(2 2 2 2))
        ((string= metre "3/4") '(3 2 2 2))
        ((string= metre "6/8") '(2 3 2 2))))

;**************************************************************************

#|
(defun pattern-with-certain-number-of-intervals (&key number intervals)
  (loop repeat number collect (pick-random intervals)))

(defun number-of-patterns-with-certain-number-of-intervals (&key number intervals)
  (expt (length intervals) number))
|#

;**************************************************************************

(defun pattern-with-certain-duration (&key duration min max step)
  (let ((dur (or duration (random-duration min max step))))
    (when (> dur 0)
      (let ((min (expt 2 dur)))
        (bits-to-intervals (+ min 1 (ash (random (/ min 2)) 1)))))))

(defun number-of-patterns-with-certain-duration (&key duration &allow-other-keys)
  (expt 2 duration))

;**************************************************************************

(defun pattern-with-certain-duration-of-intervals (&key duration  min max step intervals)
  (let ((dur (or duration (random-duration min max step))))
    (when (> dur 0)
      (loop thereis (allowed-intervals (pattern-with-certain-duration :duration dur) intervals)))))
    
(defun number-of-patterns-with-certain-duration-of-intervals (&key &allow-other-keys) 
  '?)

(defun allowed-intervals (pattern intervals)
  (when (loop for ioi in pattern always (member ioi intervals)) pattern))

;**************************************************************************
; generator-creators for metrical patterns

(defun metric-pattern-with-certain-duration 
       (&key duration min max step (phase nil) (sizes '(16 18 24)) (divs '(2 3))) 
  (let ((dur (or duration (random-duration min max step))))
    (let* ((bar (pick-random sizes))
           (meter (a-metre bar divs))
           (position (if phase phase (random bar))))
      (pattern-from-grammar-long-segment position dur bar meter))))

#|
(loop for metre in (all-metres)
      collect (metre-to-durations metre 48))

|#
;(metric-pattern-with-certain-duration 40)
;(check-any-metre (metric-pattern-with-certain-duration :duration 40))

;**************************************************************************

(defun pattern-from-metre (&key bar-length metre (start-phase 0) (duration bar-length))
  (pattern-from-grammar-long-segment start-phase duration bar-length metre))

(defun number-of-patterns-from-metre (&key bar-length metre (start-phase 0) (duration bar-length))
  (number-of-pattern-from-grammar-long-segment start-phase duration bar-length metre))

;(pattern-from-metre :bar-length 12 :metre '(2 3 2) :duration 12)
;;(check-any-metre (pattern-from-metre :bar-length 24 :metre '(2 3 2 2) :duration 24))

;**************************************************************************
;**************************************************************************

(defun pattern-from-grammar-interval-count-segment (phase interval-count size divs)
  (loop with count = interval-count
        while (> count 0)
        as pattern = (pattern-from-grammar-segment phase size size divs)
        then (pattern-from-grammar size divs)
        as length = (length pattern)
        when (<= length count)
        append pattern
        and do (decf count length)
        else append (subseq pattern 0 (1- count))
        and do (setf count 0)))

(defun pattern-from-grammar-long-segment (phase duration size divs)
  (let* ((head (- size phase))
         (only-head (<= duration head))
         (head-end-phase (if only-head (+ phase duration) size))
         (rest (unless only-head (- duration head)))
         (bars (when rest (not-zero (floor (/ rest size)))))
         (tail (when rest (not-zero (mod rest size)))))
    (append (pattern-from-grammar-segment phase head-end-phase size divs)
            (when bars (loop repeat bars append
                             (pattern-from-grammar size divs)))
            (when tail (pattern-from-grammar-segment 0 tail size divs)))))

(defun number-of-pattern-from-grammar-long-segment (phase duration size divs)
  (let* ((head (- size phase))
         (only-head (<= duration head))
         (head-end-phase (if only-head (+ phase duration) size))
         (rest (unless only-head (- duration head)))
         (bars (when rest (not-zero (floor (/ rest size)))))
         (tail (when rest (not-zero (mod rest size)))))
    (* (number-of-patterns-from-grammar-segment phase head-end-phase size divs)
       (if bars (expt (number-of-patterns-from-grammar size divs) bars) 1)
       (if tail (number-of-patterns-from-grammar-segment 0 tail size divs) 1))))

;**************************************************************************

(defun pattern-from-grammar-segment (start-phase end-phase size divs)
  (cond ((and (<= start-phase 0)
              (>= end-phase size))
         (pattern-from-grammar size divs))
        ((or (>= start-phase size)
             (<= end-phase 0))
         nil)
        ((null divs) (error "impossible phase"))
        (t (loop repeat (first divs)
                 for time from 0 by (/ size (first divs))
                 append (pattern-from-grammar-segment (- start-phase time)
                                                      (- end-phase time)
                                                      (/ size (first divs)) 
                                                      (rest divs))))))

(defun number-of-patterns-from-grammar-segment (start-phase end-phase size divs)
  (cond ((and (<= start-phase 0)
              (>= end-phase size))
         (number-of-patterns-from-grammar size divs))
        ((or (>= start-phase size)
             (<= end-phase 0))
         1)
        ((null divs) 0)
        (t (apply #'* (loop repeat (first divs)
                            for time from 0 by (/ size (first divs))
                            collect (number-of-patterns-from-grammar-segment (- start-phase time)
                                                                             (- end-phase time)
                                                                             (/ size (first divs)) 
                                                                             (rest divs)))))))

;**************************************************************************

(defun random-duration (min max step)
  (+ min (* step (random (1+ (/ (- max min) step))))))

;**************************************************************************

(defun all-levels (&optional (sizes '(16 18 24)))
  (sort (remove-duplicates (mapcan #'levels sizes))
        #'<))

(defun levels (size)
  (when (integerp size)
    (cons size 
          (remove-duplicates
           (append (levels (/ size 3))
                   (levels (/ size 2)))))))

;(all-levels) -> (1 2 3 4 6 8 9 12 16 18 24)

(defun sole-divisors (beat &optional ignore)
  (cond ((= beat 1) t)
        ((zerop (mod beat 2))
         (sole-divisors (/ beat 2)))
        ((zerop (mod beat 3))
         (sole-divisors (/ beat 3)))
        (t nil)))

(defun part-of (pattern now)
  (cond ((= now 0) nil)
        ((null pattern) nil)
        ((< now (first pattern)) nil)
        (t (cons (first pattern)
                 (part-of (rest pattern) (- now (first pattern)))))))

;**************************************************************************

(defun pattern-from-grammar (size divs &optional (n (number-of-patterns-from-grammar size divs)))
  (cond ((null divs) (list size))
        ((zerop (random n))
         (list size))
        (t (loop repeat (first divs)
                 append (pattern-from-grammar (/ size (first divs)) 
                                              (rest divs) 
                                              (round (expt (1- n) (/ (first divs)))))))))

(defun number-of-patterns-from-grammar (size divs)
  (if (null divs) 1
      (1+ (expt (number-of-patterns-from-grammar (/ size (first divs)) (rest divs))
                (first divs)))))

;**************************************************************************
;**************************************************************************

(defun all-metres (&optional (sizes '(16 18 24)) (divs '(2 3)))
  (loop for size in sizes append (metres size divs)))

(defun metres (size &optional (divs '(2 3)))
  (if (= size 1) 
    '(())
    (loop for div in divs when (zerop (mod size div)) 
          append (loop for metre in (metres (/ size div) divs)
                       collect (cons div metre)))))

(defun beat-fitting-metres (size divs beat)
  (cond ((= beat size) ;metres below beat-level
         (metres size divs))
        ((> beat size) nil)
        (t (loop for div in divs when (zerop (mod size div)) 
                 append (loop for metre in (beat-fitting-metres (/ size div) divs beat)
                              collect (cons div metre))))))

#|
(defun beat-fitting-metres (size divs beat) ;new
  (cond ((= beat size) ;no metres below beat-level
         (list nil))
        ((> beat size) nil)
        (t (loop for div in divs when (zerop (mod size div)) 
                 append (loop for metre in (beat-fitting-metres (/ size div) divs beat)
                              collect (cons div metre))))))
|#

;(beat-fitting-metres 12 '(2 3) 6)
;(trace beat-fitting-metres metres)
;(beat-fitting-metres 12 '(2 3) 3)
;(beat-fitting-metres 12 '(2 3) 2)
;(metres 4 '(2 3))

(defun a-metre (size &optional (divs '(2 3)))
  (when (> size 1)
    (let ((div ( pick-random (loop for div in divs when (zerop (mod size div)) collect div))))
      (cons div (a-metre (/ size div) divs)))))

;**************************************************************************
;**************************************************************************
; utils

(defun not-zero (x)(unless (zerop x) x))

(defun bits-to-intervals (n)
  (loop  with prev = 0
         for count from 1
         for mask = (expt 2 (1- (floor (log n 2)))) then (ash mask -1)
         while (> mask 0)
         unless (zerop (logand mask n))
         collect (- count prev)
         and do (setf prev count)))  
          
;(bits-to-intervals #b101100010101)

(defun pick-random (list)
  (nth (random (length list)) list))

;**************************************************************************
;**************************************************************************

(defun metric? (pattern size metre &optional metre-above (positions '(0)))
  (cond ((null pattern)) ; (zerop (first positions))) ;t)
        ((= (first pattern) size)	; interval matches maximum.
         (metric? (rest pattern)	; skip the first interval of the pattern.
                size metre metre-above 
                (next-position positions (reverse metre-above)))) ; TODO?
        ((and (< (first pattern) size)
              (<= (first pattern) (/ size (first metre))))
         (metric? pattern 		; use the same pattern.
                (/ size (first metre)) 	; reduce size by the highest metrical subdivision.
                (rest metre)	      ; reduce the metre, removing the highest subdivision
                (append metre-above  (list (first metre))) ; save the subdivision in metre-above
                (cons 0 positions)))			   ; mark that we start at phase 0 on this metre.
        ((and (> (first pattern) size)
              (zerop (first positions))
              metre-above)
         (metric? pattern 
                (* size (first (last metre-above))) 
                (append (last metre-above) metre)
                (butlast metre-above)
                (rest positions)))
        ((and (> (first pattern) size)
              (zerop (first positions))
              (null metre-above)
              (zerop (mod (first pattern) size)))
         (metric? (rest pattern) size metre metre-above))
        (t nil)))

(defun next-position (positions metre)
  (when positions
    (let ((new (mod (1+ (first positions)) 
               (or (first metre) 1))))
      (if (zerop new)
        (cons new (next-position (rest positions) (rest metre)))
        (cons new (rest positions))))))
         
;(metric? '(4 1 1 2 1 1 2  1 1 2 4 4 4 4 4) 12 '(3 2 2)) => T
;(metric? '(1 1 2 1 1  2 1 1 2  2 2 2  2) 12 '(2 3 2)) => T
;(metric? '(2 2 2  2 1 1 2  2 2 2  6  1) 12 '(2 3 2)) =>
;(metric? '(3 3 3  1 1 1 3 1 1 1  3 3 3  9  1) 18 '(2 3 3)) => T
;(metric? '(3 3 3 1 1 1 3 3  3 3 3 9  6 6 3 3) 18 '(3 2 3)) => NIL
;(metric? '(6 6 6  6 6 3 3) 18 '(3 2 3)) => T

;(metric? '(2 2 2 2 1 1 2 2 24 2 2 2 6) 12 '(2 3 2))
;(metric? '(2 2 2 2 1 1 2 2 24 2 2 2 6) 12 '(3 2 2))
;(metric? '(2 2 2 2 1 1 2 2 24 2 2 2 6) 12 '(2 2 3))
;(metric? '(16 2 1 1 4) 16 '(2 2 2 2) NIL '(0))
;(metric? nil 16 '(2 2 2 2) NIL '(0))

(defun position-to-state (position size metre &optional metre-above (positions '(0)))
  (cond ((null (integerp position)) nil)
        ((zerop position) (list size metre metre-above positions))
        ;; ((zerop position) (list size (append metre (reverse metre-above)) nil positions))
        ((= position size)
         (list size metre metre-above  
               (cons (mod (1+ (first positions)) (first (last metre-above)))
                     (rest positions))))
        ((< position size)
         (position-to-state 
          (mod position (/ size (first metre))) ; Find location of position within the first subdivided size.
          (/ size (first metre)) 
          (rest metre)
          (append metre-above  (list (first metre)))
          (cons (floor (/ position (/ size (first metre))))  positions)))
        (t nil)))

;(<= (first pattern) (/ size (first metre)))
;(POSITION-TO-STATE  2 3 '(2 2) '(2 2) '(0 0 0))
;(position-to-state 1 12 '(2 3 2))
;(check-t1-beat? 6 6 12 '(2 3 2) 0)
;(check-t1-beat? 5 6 12 '(2 3 2) 1)

;(check-position '(8) 12 '(2 2 2 2) 2)
;(check-position '(4) 12 '(2 2 2 3) 2)
;(untrace position-to-state CHECK)

; (position-to-state 0 16 '(2 2 2 2)) => (16 (2 2 2 2) NIL (0))
; (position-to-state 1 16 '(2 2 2 2)) => (1 NIL (2 2 2 2) (1 0 0 0 0))
; (position-to-state 2 16 '(2 2 2 2)) => (2 (2) (2 2 2) (1 0 0 0))
; (position-to-state 3 16 '(2 2 2 2)) => (1 NIL (2 2 2 2) (1 1 0 0 0))
; (position-to-state 4 16 '(2 2 2 2)) => (4 (2 2) (2 2) (1 0 0))
; (position-to-state 5 16 '(2 2 2 2)) => (1 NIL (2 2 2 2) (1 0 1 0 0))

;(Check-position '(2 2 2 2 1 1 2 2 24 2 2 2 6) 12 '(2 3 2) 1)

(defun all-states (size metre)
  (loop for position from 0 below size 
        collect (cons position (position-to-state position size metre))))

;(all-states 12 '(2 3 2))

(defun check-any-phase (pattern bar-size metre 
                                &optional (states (all-states bar-size metre)))
  (loop for (position size metre metre-above positions) in states
        when (metric? pattern size metre metre-above positions)
        collect position))

;(check-any-phase '(2 2 2 2 1 1 2 2 24 2 2 2 6) 12 '(2 3 2))
;(check-any-phase '(2 2 2 2 1 1 2 2 6 2 2 2 6) 12 '(3 2 2))
;(check-any-phase '(2 2 2 2 1 1 2 2 6 2 2 2 6) 12 '(2 2 3))

(defun check-any-metre (pattern  
                        &optional
                        (sizes '(24 18 16)) (divs '(2 3)))
  (loop for size in sizes
        append (loop for metre in (metres size divs)
                     as result = (check-any-phase pattern size metre)
                     when result collect (cons metre result))))
         
;(check-any-metre '(2 2 1))
;(check-any-metre '(2 2 2 2 1 1 2 2 24 2 2 2 6))
;(check-any-metre '(2 2 2 2 1 1 2 2 24 2 2 2 2 2 2))
;(check-any-metre '(2 2 2 2 1 1 2 2 2 2 2 2 2 2))
;(check-any-metre '(2 2 2 2 1 1 2 2 6 2 2 2))

(defun check-any-metre-certain-phase (pattern  
                                      &optional
                                      (phase 0)
                                      (sizes '(24 18 16)) (divs '(2 3)))
  (loop for size in sizes
        append (loop for metre in (metres size divs)
                     as result = (check-certain-phase pattern size metre phase)
                     when result collect metre)))

(defun check-certain-phase (pattern bar-size meter phase)
  (destructuring-bind (size metre metre-above positions)
                      (position-to-state (mod phase bar-size) bar-size meter)
    (metric? pattern size metre metre-above positions)))


;(check-any-metre '(2 2 2 2 1 1 2 2 6))
;(check-any-metre-certain-phase '(2 2 2 2 1 1 2 2 6) 22)
;(check-any-metre-certain-phase '(9 3 1 1 1 3 1 1 1 1 1 1) 9)


;(check-t1-beat? 11 24 12 '(2 3 2) 1)
;(check-t1-beat-any-metre? '(2 2 4 1 1 2 2 2 2 2 2 2 2) 2 2 12 '(2 3))

(defun t1-beat-any-metre? (pattern t1 beat size divs 
                                         &optional 
                                         (posibilities (loop for metre in (metres size divs) 
                                                             collect (cons metre
                                                                           (loop for position below size
                                                                                 collect position)))))
 (let* ((result (loop for (metre . positions) in posibilities
                      as r =
                      (loop for position in positions
                            when (check-position pattern size metre position)
                            collect position)
                      when r collect (cons metre r)))
        (found (loop for (metre . positions) in result
                     thereis (loop for position in positions 
                                   thereis (check-t1-beat? t1 beat size metre position)))))
   (cons found result)))

;(check-t1-beat-any-metre? '(2 2 4 1 1 2 2 12 2 2 2 2) 4 4 12 '(2 3))
; return correct t1 beat and list of metre positions that fit pattern

(defun check-t1-beat-any-metre? (pattern t1 beat size divs 
                                         &optional 
                                         (posibilities (loop for metre in (metres size divs) 
                                                             collect (cons metre
                                                                           (loop for position below size
                                                                                 collect position)))))
  (loop for (metre . positions) in posibilities
        thereis
        (loop for position in positions
              thereis (and (check-t1-beat? t1 beat size metre position)
                           (check-position pattern size metre position)))))


#|
(defun check-metric-t1-beat (pattern t1 beat sizes divs)
  (loop for size in sizes
        thereis (loop for metre in (beat-fitting-metres size divs beat)
                      thereis (loop for position below size
                                    when
                                    (and (check-position nil size metre (mod (+ t1 position) size)) 
                                         ;(check-t1-beat? t1 beat size metre position)
                                         (check-position pattern size metre position))
                                    do (print (list t1 beat size metre position))
                                    and do (return t)))))
|#

(defun check-metric-t1-beat (pattern t1 beat sizes divs)
  "Check that a rhythmic pattern can accomodate a beat period (beat) starting at t1.
   Acceptable lengths of rhythm are in sizes and meters described in terms of divisions as divs"
  (loop for size in sizes
        thereis (loop for metre in (beat-fitting-metres size divs beat)
                      thereis (loop for position below size
                                    thereis
                                    (and (check-position nil size metre (mod (+ t1 position) size)) 
                                         ;(check-t1-beat? t1 beat size metre position)
                                         (check-position pattern size metre position)
					 (progn (format t "size ~a metre ~a position ~a~%" size metre position) t))))))


; (check-metric-t1-beat '(4 1 1 2 1 1 2  1 1 2 4 4 4 4 4) 0 4 '(8 12 16 18 24) '(2 3)) => T

(defun check-t1-beat? (t1 beat size metre metre-start)
  (check-position (list beat) size metre (mod (+ t1 metre-start) size)))

(defun check-position (pattern size metre position)
  (let ((state (position-to-state position size metre)))
    (when state
      (destructuring-bind (size metre metre-above positions) state
        (metric? pattern size metre metre-above positions)))))

;(check-metric-t1-beat '(2 2 4) 4 4 '(4 8) '(2 3))
;(check-metric-t1-beat '(2 2 4) 3 4 '(4 8) '(2 3))
;(check-metric-t1-beat '(3 1 2 1 1 1 2 1) 0 3 '(12 16 18 24) '(2 3))
;(check-metric-t1-beat '(3 1 1 1 1 1 1 3) 0 3 '(12 16 18 24) '(2 3))
;(check-metric-t1-beat '(3 1 2 1 1 1 1 2) 0 4 '(12 16 18 24) '(2 3))
;(apply #'+ '(3 1 2 1 1 1 2 1))
;(beat-fitting-metres 12 '(2 3) 3)
;(trace metric?)

(defun level-metric-t1-beat (pattern t1 beat sizes divs max max-beat multiples)
  (loop for size in sizes
        append (loop for metre in (beat-fitting-metres size divs beat)
                      append (loop for position below size
                                    when (and (check-position nil size metre (mod (+ t1 position) size)) 
                                              ;(check-t1-beat? t1 beat size metre position)
                                              (check-position pattern size metre position))
                                    collect (beat-level beat size (- max))))))

;**************************************************************************
;**************************************************************************
;strict pulse patterns

(defun strict-pulse-pattern-certain-duration (&key duration min max step pulses intervals phase)
  (let* ((pulse (elt pulses (random (length pulses))))
         (p (if phase (mod phase pulse) (random pulse))))
    (append (pattern-with-certain-duration-of-intervals :duration p :intervals intervals) ;phase part of pulse
            (loop for beat from pulse to (- duration p) by pulse
                  append (pattern-with-certain-duration-of-intervals :duration pulse :intervals intervals)) ;n pulses
            (pattern-with-certain-duration-of-intervals :duration (mod (- duration p) pulse) :intervals intervals))))

;(strict-pulse-pattern-certain-duration :duration 16 :pulses '(2 3 4 5 6 7 8) :intervals '(4 3 2 1))
;(strict-pulse-pattern-certain-duration :duration 10 :pulses '(2 4 6) :intervals '(1 3))

#|
(loop (let ((pulse (strict-pulse-pattern-certain-duration :duration 16 :pulses '(2 3 4 5 6 7 8) :intervals '(4 3 2 1))))
  (print (list pulse (correct-strict-pulse *beat* pulse *phase*)))))
|#


#|
(defun factor-from-score (score)
  (when score
    (destructuring-bind ((country time-signature name &key bar-duration start-at) pattern)
                        score
      (let* ((number-of-quarter-notes (rest (assoc time-signature '(("3/4" . 3)
                                                                    ("3/2" . 6)
                                                                    ("6/8" . 3)
                                                                    ("2/4" . 2)
                                                                    ("4/4" . 4)
                                                                    ("2/2" . 4)
                                                                    ("9/8" . 4.5)
                                                                    ("12/8" . 6))
                                                   :test #'string=)))
             (quarter-duration (/ bar-duration number-of-quarter-notes)))
        (/ 4.0 quarter-duration)))))
|#

(defun factor-from-score (score)
  (when score
    (destructuring-bind ((country time-signature name &key bar-duration start-at) pattern)
                        score
      (* 16 
         (/ (read-from-string time-signature)
            bar-duration)))))

(defun factor-from-score-arg (score)
  (list :parameter-factor (factor-from-score score)))

#|
(factor-from-score
 '((LIBERIA "4/4" ("L'inno Nazionale") :BAR-DURATION 8 :START-AT 6)
   (2 3 1 2 2 6 2 3 1 2 2 6 2 3 1 2 2 3 1 2 2 4 4 6 2 2 2 2 2 2 2 2 2 2 2 2 2 38
    2 2 2 2 2)))
-> 2

(factor-from-score
'((MOROCCO "2/4" ("Hymne Cherifien") :BAR-DURATION 8 :START-AT 0)
 (3 1 2 1 1 8 3 1 2 1 1 8 3 1 2 2 1 1 1 1 4 1 1 1 1 12 3 1 2 1 1 8 3 1 3 1 8 3
  1 2 1 1 8 2 1 1 2 1 9 2 1 1 4 2 1 1 2 2 1 1 1 1 10 2 3 1 2 2 1 1 1 1 1 1 1 1
  1 1 6 3 1 2 1 1 8 3 1 3 1 8 3 1 2 1 1 8 2 1 1 2 1 9 8)))
-> 1
|#
