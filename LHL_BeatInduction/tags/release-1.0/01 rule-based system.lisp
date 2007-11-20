;************************************************************************************
; rule-based beat-induction models
; shared part
;************************************************************************************
; state
(in-package :shoe)

(defclass state ()
  ((input :accessor input :initform (make-input) :initarg :input)
   (beat :accessor beat :initform nil :initarg :beat)
   (t1 :accessor t1 :initform 0 :initarg :t1)
   (rule :accessor rule :initform nil :initarg :rule)
   (now :accessor now :initform -1 :initarg :now)
   (prev :accessor prev :initform nil :initarg :prev)
   (rules :accessor rules :initform nil :initarg :rules)))

;;; Method declarations

(defgeneric apply-rule (state rules &rest args)
   (:documentation 
"should be supplied by each state class can dispatch to one of:
apply-one-rule-at-most, apply-each-rule-in-order-once,
apply-rule-round-robin-until-no-matches or apply-rule-from-start-until-no-matches"))

(defgeneric beat-phase (state))

(defgeneric duration-of-note-on-t (state time))

(defgeneric make-next-state (state))

(defgeneric note-on-t? (state time))

(defgeneric offset-of-note-on-t (state time))

(defgeneric minimal-duration-of-note-on-t (state time))

(defgeneric now-rule-t1-beat (state))

(defgeneric onset-of-note-ending-on-t (state time))

;;; definitions

(defun make-input ()
  (let ((vector (make-array 16 :adjustable t :fill-pointer 0)))
    ;(vector-push-extend t vector)
    vector))

(defmethod pattern ((state state))
  (loop as prev = 0 then time
        as time = (offset-of-note-on-t state 0) then (offset-of-note-on-t state time)
        while time
        collect (- time prev)))

(defmethod make-next-state ((state state))
  (make-instance (class-of state)
    :input (input state)
    :beat (beat state)
    :t1 (t1 state)
    :now (now state)
    :prev state
    :rules (rules state)))
  
(defun act-state (state action args debug)
  (apply action state args)
  (setf (rule state) action)
  (when debug (funcall debug state))
  (make-next-state state))

(defun test-state (state test args)
  (apply test state args))

;;;utils


(defun integrate (intervals &optional (time 0))
  (if intervals
    (cons time (integrate (rest intervals) (+ (first intervals) time)))
    (list time)))

(defun differentiate (times)
  (when times (loop for time in times
                    as next in (rest times)
                    collect (- next time))))

;************************************************************************************

(defmethod t2 ((state state)) (when (beat state) (+ (beat state) (t1 state))))
(defmethod t3 ((state state)) (when (beat state) (+ (* 2 (beat state)) (t1 state))))

(defmethod (setf t2) (new-t2 (state state))
  (setf (beat state)(- new-t2 (t1 state))))

(defmethod double-beat ((state state))
  (setf (beat state) (* 2 (beat state))))

(defmethod beat-phase ((state state))
  (mod (t1 state)(beat state)))

(defmethod note-on-t? ((state state) time) 
  (and (<= time (now state))
       (aref (input state) time)
       time))

(defmethod note-on-now? ((state state)) (note-on-t? state (now state)))
(defmethod note-on-t2? ((state state)) (note-on-t? state (t2 state)))
(defmethod note-on-t3? ((state state)) (note-on-t? state (t3 state)))

(defmethod duration-of-note-on-t ((state state) time) 
  (when (note-on-t? state time)
    (let ((next (offset-of-note-on-t state time)))
      (when next (- next time)))))

(defmethod offset-of-note-on-t ((state state) time)
  (loop for next from (1+ time)
        while (<= next (now state))
        when (note-on-t? state next)
        do (return next)))

(defmethod minimal-duration-of-note-on-t ((state state) time)
  (or (duration-of-note-on-t state time) 
      (1+ (- (now state) time))))

(defmethod minimal-duration-of-note-on-t2 ((state state))
  (minimal-duration-of-note-on-t state (t2 state)))

(defmethod duration-of-note-on-t1 (state)(duration-of-note-on-t state (t1 state)))
(defmethod duration-of-note-on-t2 (state)(duration-of-note-on-t state (t2 state)))
(defmethod duration-of-note-on-t3 (state)(duration-of-note-on-t state (t3 state)))

(defmethod onset-of-note-ending-on-t ((state state) time)
  (when (note-on-t? state time)
    (loop for prev downfrom (1- time)
          while (>= prev 0)
          when (note-on-t? state prev)
          do (return prev))))

(defmethod duration-of-note-ending-on-t ((state state) time) 
  (when (note-on-t? state time)
    (let ((prev (onset-of-note-ending-on-t state time)))
      (when prev (- time prev)))))

(defun onset-of-long-note-between (state from to minimum &optional (test #'>))
  (loop with time 
        with max-duration = minimum
        as onset = from then (offset-of-note-on-t state onset)
        while (and onset (< onset to))
        as duration = (minimal-duration-of-note-on-t state onset) 
        when (funcall test duration max-duration)
        do (setf time onset max-duration duration)
        finally (return time)))

;************************************************************************************
; control structure for rule base

(defun make-input-stream (onsets)
  #'(lambda(now)
      (cond ((null onsets) :halt)
            ((= now (first onsets))
             (pop onsets) t)
            (t nil))))

(defun get-input (stream now)
  (funcall stream now))

(defun run (class data &key
                  sets
                  debug
                  (trace #'rule-t1-beat)
                  args
                  munch-all-input)
  (let* ((debug-fun (if (eql debug t) 
                      #'(lambda (state) (print (now-rule-t1-beat state)))
                      debug))
         (initial-state (make-instance class :rules sets))
         (input (make-input-stream (integrate data)))
         (final-state (apply-rule-sets input
                                       initial-state
                                       debug-fun
                                       args
                                       munch-all-input)))
    (if trace 
      (collect-trace (prev final-state) trace) 
      final-state)))

(defun apply-rule-sets (onsets state &optional debug args munch-all-input)
  (loop as (new stop) = (list state nil) 
        then (apply-rule-set-step onsets new debug args munch-all-input)
        while (not stop)
        finally (return new)))

(defun apply-rule-set-step (onsets state debug args munch-all-input)
  (let ((stop (apply-tick onsets state debug)))
    (if stop 
      (list state t)
      (list (apply-rule-sets-step state debug args) 
            (and  (null (rules state)) (not munch-all-input))))))

(defun apply-tick (onsets state debug)
  (incf (now state))
  (let ((in (get-input onsets (now state))))
    (unless (eql in :halt)
      (vector-push-extend in (input state))
      (when debug (funcall debug state)))
    (eql in :halt)))

(defun apply-rule-sets-step (state debug args)
  (loop as status = state then new
        as rules = (butlast (first (rules status)))
        as stop = (first (last (first (rules status))))
        as new = (apply-rule status rules debug args)
        when (and (prev new) (eql (rule (prev new)) stop))
        do (pop (rules new))
        else do (return new)))

#|
(setf lst (lhl82 '(2 1 1 2 ) :max-beat 30 :debug #'print-state-debug 
                 :munch-all-input nil :trace nil))

(defun print-state-debug (state)
  (print-grid   t (pattern state) "" 27 1 "!" "!" "!" "." (now state))
  (print-trace (now state)(rule state)(t1 state)(beat state)(t3 state)(now state)
               27 t t t nil)
  (print (rules state)))

(rules (prev lst))
(rules lst)
|#

;************************************************************************************
; strategy

(defun apply-one-rule-at-most (state rules debug args)
  (cond ((null rules) state)
        ((test-state state (first (first rules)) args)
         (act-state state (second (first rules)) args debug))
        (t (apply-one-rule-at-most state (rest rules) debug args))))

(defun apply-each-rule-in-order-once (state rules debug args)
  (cond ((null rules) state)
        ((test-state state (first (first rules)) args)
         (let ((new (act-state state (second (first rules)) args debug)))
           (apply-each-rule-in-order-once new (rest rules) debug args)))
        (t (apply-each-rule-in-order-once state (rest rules) debug args))))

(defun apply-rule-round-robin-until-no-matches (state rules debug args)
  (let ((new (apply-each-rule-in-order-once state rules debug args)))
    (if (eql new state)
      state
      (apply-rule-round-robin-until-no-matches new rules debug args))))

(defun apply-rule-from-start-until-no-matches (state rules debug args)
  (let ((new (apply-one-rule-at-most state rules debug args)))
    (if (eql new state)
      state
      (apply-rule-from-start-until-no-matches new rules debug args))))

;************************************************************************************

(defun collect-trace (state trace &optional result)
  (if state
    (collect-trace (prev state) trace (cons (funcall trace state) result))
    result))

;************************************************************************************
; trace data collectors

(defmethod re-create ((state state)) ; for debugging
  `(make-instance 'state 
     :now ,(now state)
     :rule ',(rule state) 
     :t1 ,(t1 state) 
     :beat ,(beat state)
     :input ,(input state)))

(defmethod now-rule-t1-beat-input ((state state))
  (list (now state) (rule state) (t1 state) (beat state)(input state)))

(defmethod now-rule-t1-beat ((state state))
  (list (now state) (rule state) (t1 state) (beat state)))

(defmethod now-rule-phase-beat ((state state))
  (list (now state) (rule state) (beat-phase state) (beat state)))

(defmethod rule-t1-beat ((state state))
  (list (rule state) (t1 state) (beat state)))

;*******************************************************************************

#|
(defun integrate (iois)
  (loop for ioi in iois
        as onset = (first iois) then (+ onset ioi)
        collect onset))

(defun differentiate (l) 
  (loop for element in (cons 0 l)
        for next in l 
        collect (- next element)))

|#
  
