;;; Longuet-Higgins' beat induction algorithm
;;;   according to
;;;   Longuet-Higgins, H.C. & C.S. Lee (1982).
;;;   Perception of musical rhythms, Perception. 11,115-128 
(in-package :shoe)

(defclass LHL-82-state (state)())

;;; method declarations

(defgeneric long-note-at-t2? (LHL-82-state))

(defgeneric longer-note-after-t2-than-on-t2? (LHL-82-state))

(defgeneric longest-note-at-t2? (LHL-82-state))

(defgeneric may-grow-beat? (LHL-82-state &key max-beat &allow-other-keys) )

(defgeneric near-beginning? (LHL-82-state))

;;; definitions

(defun LHL82 (data &key 
                   munch-all-input
                   debug
                   (max-beat 30)
                   (trace #'now-rule-t1-beat))
  (run 'LHL-82-state
       data
       :munch-all-input munch-all-input
       :trace trace 
       :debug debug 
       :sets '(((may-initialize?  initialize) initialize)
                ((may-stretch?  stretch)
                 (may-update?   update)
                 (may-longnote? longnote)
                 (may-conflate? conflate)
                 (may-confirm? confirm)
                 confirm))
       :args (list :max-beat max-beat)))

(defmethod apply-rule ((state LHL-82-state) rules &rest args)
  (apply #'apply-one-rule-at-most state rules args))

;(lhl82 '(2 1 1 2 4 2 4) :max-beat 30 :debug #'print-state-interactive-debug)
;(lhl82 '(2 1 1 2 4 2 4) :max-beat 30 :debug #'print-state-debug)
;(lhl82 '(2 1 1 2 4 2 4))
;; D&H99 example:
;(lhl82 '(3 1 6 2 3 1 6 2 3 1 6) :max-beat 30 :debug #'print-state-debug)

;****************************************************************************************

(defmethod may-initialize? ((state LHL-82-state) &rest ignore)
  (declare (ignore ignore))
  (and (null (beat state))
       (> (now state) 0)
       (note-on-t? state (now state))))

(defmethod initialize ((state LHL-82-state) &rest ignore)
  (declare (ignore ignore))
  (setf (beat state)
        (duration-of-note-on-t state 0)))

;****************************************************************************************
; conflate rule

(defmethod may-grow-beat? ((state LHL-82-state) &key max-beat &allow-other-keys) 
  (<= (* 2 (beat state)) max-beat))

(defmethod may-conflate? ((state LHL-82-state) &rest args &key max-beat &allow-other-keys) 
  (declare (ignore max-beat))
  (and (note-on-t3? state) 
       (apply #'may-grow-beat? state args)))

(defmethod conflate ((state LHL-82-state) &rest ignore)
  (declare (ignore ignore))
  (double-beat state))

;****************************************************************************************
; incremental real-time stretch rule

(defmethod longer-note-after-t2-than-on-t2? ((state LHL-82-state))
  (let ((duration (duration-of-note-on-t2 state)))
    (when duration
      (onset-of-long-note-between state 
                                  (offset-of-note-on-t state (t2 state))
                                  (now state)
                                  duration))))

(defmethod stretch ((state LHL-82-state) &key max-beat &allow-other-keys)
  (declare (ignore max-beat))
  (setf (t2 state) (longer-note-after-t2-than-on-t2? state)))

(defmethod may-stretch? ((state LHL-82-state) &rest args)
  (and (apply #'may-grow-beat? state args)
       (longer-note-after-t2-than-on-t2? state)))

;****************************************************************************************
; update rule

(defmethod near-beginning? ((state LHL-82-state)) t) ; no formalization given ??

(defmethod long-note-at-t2? ((state LHL-82-state))
  (>= (minimal-duration-of-note-on-t2 state) 
      (beat state)))

(defmethod longest-note-at-t2? ((state LHL-82-state))
  (let ((duration (minimal-duration-of-note-on-t2 state)))
    (not (onset-of-long-note-between state 
                                     0
                                     (t2 state)
                                     duration
                                     #'>=))))

(defmethod update ((state LHL-82-state) &key max-beat &allow-other-keys)
  (declare (ignore max-beat))
  (setf (t1 state) (t2 state)))

(defmethod may-update? ((state LHL-82-state) &key max-beat &allow-other-keys) 
  (declare (ignore max-beat))
  (and (near-beginning? state)
       (note-on-t2? state)
       (longest-note-at-t2? state)
       (long-note-at-t2? state)))

;****************************************************************************************
; longnote rule

(defmethod may-longnote? ((state LHL-82-state) &key max-beat &allow-other-keys)
  (declare (ignore max-beat))
  (and (duration-of-note-on-t1 state)
       (> (duration-of-note-on-t1 state)
          (* 2 (beat state)))))

(defmethod longnote ((state LHL-82-state) &key max-beat &allow-other-keys)
  (declare (ignore max-beat))
  (setf (t2 state) (offset-of-note-on-t state (t1 state))))

;****************************************************************************************
; confirm rule

(defmethod may-confirm? ((state LHL-82-state) &key max-beat &allow-other-keys)
  (declare (ignore max-beat))
  (and (>= (now state) (t3 state))
       (note-on-now? state)))

(defmethod confirm ((state LHL-82-state) &rest ignore)
  (declare (ignore ignore)))

;****************************************************************************************
; tapper

;(foot-tapper #'lhl82 '(1 2 9 1 2 9 1 2 4 2 3 1 2) :max-beat 30 :tap-function)
;(foot-tapper #'lhl82 #'lhl82'(4 4 4 4 8 8 4 4 4 4 16) :unit 4 :tempo 120)
;(foot-tapper #'lhl82 '(2 1 1 2 4 2) :unit 4 :tempo 120 :incremental nil :b-advance 300 :tap-function #'derive-t3s)
;(foot-tapper #'lhl82 '(2 1 1 2 4 2) :unit 4 :tempo 120 :incremental nil :b-advance 300 :tap-function #'derive-t2s :max-beat 2)

;****************************************************************************************
; print-elaborate-traces
;(print-process-traces #'lhl82 (lhl82-examples) :max-beat 30 :tap-trace 'passed-tns)
;(print-process-traces #'lhl82 (povels-problematic-examples) :max-beat 30) 
;(print-process-trace #'lhl82 '(1 2 9 1 2 9 1 2 4 2 3 1 2) :max-beat 30) ;ex. B
;(print-process-trace #'lhl82 '(1 2 9 1 2 9 1 2 4 2 3 1 2) :max-beat 30 :tap-trace 'passed-tns)

;;(loop for (example i) in (lhl82-examples) as result = (lhl82 example :max-beat 30)
;;      do (print (list (passed-tns result (apply #'+ example)) i)))

;(print-process-trace #'lhl82 '(8 4 4 6 2 8 6 2 8 8 4 4 6 2 8 6 2 8 12 4 16) :max-beat 30 :tap-trace #'passed-tns)
;=>
#|
; *=now, rule on next line fires
; action => new beat [start, duration]
; 1=t1, 2=t2, 3=t3, |=beat
                        !!.!........!!.!........!!.!...!.!..!!.!
                         *(=1)
INITIALIZE =>    [0 1]  123|||||||||||||||||||||||||||||||||||||
                          *(=2)
UPDATE =>        [1 1]  .123||||||||||||||||||||||||||||||||||||
                           *(=3)
CONFLATE =>      [1 2]  .1.2.3.|.|.|.|.|.|.|.|.|.|.|.|.|.|.|.|.|
                             *(=5)
UPDATE =>        [3 2]  ...1.2.3.|.|.|.|.|.|.|.|.|.|.|.|.|.|.|.|
                               *(=7)
LONGNOTE =>      [3 9]  ...1........2........3........|........|
                                             *(=21)
STRETCH =>      [3 12]  ...1...........2...........3...........|
                                                   *(=27)
CONFLATE =>     [3 24]  ...1.......................2.......................3

tap trace:              ..||.|.|.............|.....|.......................|
|#

;****************************************************************************************
;examples

(defun povels-problematic-examples ()
  '(((2 1 2 4 3 1 2 1 2 4 3 1 2 1 2 4 3 1) "povel 1")
    ((3 1 2 3 3 3 1 2 3 3 3 1 2 3 3) "povel 2")
    ((2 2 3 1 2 2 4 2 2 3 1 2 2 4 2 2 3 1 2 2 4) "povel 3")
    ((2 5 1 1 1 2 1 1 2 2 5 1 1 1 2 1 1 2 2 5 1 1 1 2 1 1 2) "povel 4, p 123")
    ((1 3 2 1 1 4 1 3 2 1 1 4 1 3 2 1 1 4) "povel 5")
    ((1 2 1 2 2 4 4 1 2 1 2 2 4 4 1 2 1 2 2 4 4) "povel 6")))

(defun lhl82-examples ()
  '(((2 1 1 2 4 2 4) "1, Cliche")
    ((8 2 2 8 2 2 4 2 4 2 6 6 8 2 2 8 2 2 2 2 2 2 2 2 12) "2, Beethoven piano sonate op 109")
    ((2 2 4 2 2 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 4) "3,4 Beethoven dance (ex. Simon & Sumner, 1968)")
    ((1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1) "5,6 Bach, WTC, book 1, Fuge 2")
    ((4 4 4 4 4 4 4) "7, isochronous")
    ((4 2 4 2 2 2 2 4) "8A, Pop goes the weasel")
    ((6 2 4 4 4 4 2 2 4) "8B, Deutchland uber alles")
    ((4 6 2 4 4) "9, Auld lang syne")
    ((4 4 4 4 8 8 4 4 4 4 16) "12A, Onward christian soldiers")
    ((4 4 4 6 2 4 4) "12B, God save the queen") 
    ((2 2 4 2 2 4) "13")
    ((1 1 2 2 4 2 2 4) "14") 
    ((4 6 2 2 2 2 2) "15")
    ((2 5 1 1 1 2 1 1 2) "16") 
    ((2 1 1 2 2 2 2 4 2 1 1 2 2 2 2 2) "17")
    ((8 4 4 6 2 8 6 2 8 8 4 4 6 2 8 6 2 8 12 4 16) "19, Schubert C-major symphony, first mov.")
    ((4 2 4 2 2 2 2 6 4 2 4 2 6 6) "20")
    ((4 2 4 2 2 2 2 2 4 2 4 2 6) "21")
    ((1 2 9 1 2 9 1 2 4 2 3 1 2) "B")
    ((2 4 2 2 4 2 1 1 4 2 2 4 2 1 1 4 2 2 6) "D")
    ((3 1 6 2 3 1 6 2 3 1 6) "G") 
    ((4 4 1 1 1 1 2 2 4) "I")
    ((4 4 2 2 6 2 4) "J")
    ((4 4 2 2 3 1 2 2 2 2) "K")
    ((4 4 2 2 3 1 4 4 8) "L")
    ((4 4 2 2 4 4 2 2 4) "N")))
#|
#|
(make-diagnose 'LHL82-diagnose "LHL 82" 'LHL82 (LHL82-examples) 
               :trace 'now-rule-t1-beat :max-beat 30 :debug t)
|#

(DEFUN LHL82-DIAGNOSE ()
  (CHECK "1, Cliche"
         '(2 1 1 2 4 2 4)
         (LHL82 '(2 1 1 2 4 2 4) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((2 INITIALIZE 0 2) (4 CONFLATE 0 4) (8 STRETCH 0 6) (12 CONFLATE 0 12)))
  (CHECK "2, Beethoven piano sonate op 109"
         '(8 2 2 8 2 2 4 2 4 2 6 6 8 2 2 8 2 2 2 2 2 2 2 2 12)
         (LHL82 '(8 2 2 8 2 2 4 2 4 2 6 6 8 2 2 8 2 2 2 2 2 2 2 2 12)
                :TRACE
                'NOW-RULE-T1-BEAT
                :MAX-BEAT
                30)
         '((8 INITIALIZE 0 8) (14 STRETCH 0 12) (24 CONFLATE 0 24) (48 CONFIRM 0 24)))
  (CHECK "3,4 Beethoven dance (ex. Simon & Sumner, 1968)"
         '(2 2 4 2 2 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 4)
         (LHL82 '(2 2 4 2 2 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 4) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((2 INITIALIZE 0 2) (4 CONFLATE 0 4) (7 UPDATE 4 4) (12 CONFLATE 4 8) (20 CONFLATE 4 16)
           (36 CONFIRM 4 16)))
  (CHECK "5,6 Bach, WTC, book 1, Fuge 2"
         '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1)
         (LHL82 '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((1 INITIALIZE 0 1) (2 CONFLATE 0 2) (3 UPDATE 2 2) (6 CONFLATE 2 4) (10 CONFLATE 2 8)
           (18 CONFLATE 2 16)))
  (CHECK "7, isochronous"
         '(4 4 4 4 4 4 4)
         (LHL82 '(4 4 4 4 4 4 4) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (16 CONFLATE 0 16)))
  (CHECK "8A, Pop goes the weasel"
         '(4 2 4 2 2 2 2 4)
         (LHL82 '(4 2 4 2 2 2 2 4) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((4 INITIALIZE 0 4) (8 STRETCH 0 6) (12 CONFLATE 0 12) (20 STRETCH 0 18)))
  (CHECK "8B, Deutchland uber alles"
         '(6 2 4 4 4 4 2 2 4)
         (LHL82 '(6 2 4 4 4 4 2 2 4) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((6 INITIALIZE 0 6) (10 STRETCH 0 8) (16 CONFLATE 0 16) (32 CONFIRM 0 16)))
  (CHECK "9, Auld lang syne"
         '(4 6 2 4 4)
         (LHL82 '(4 6 2 4 4) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((4 INITIALIZE 0 4) (8 UPDATE 4 4) (12 CONFLATE 4 8) (20 CONFLATE 4 16)))
  (CHECK "12A, Onward christian soldiers"
         '(4 4 4 4 8 8 4 4 4 4 16)
         (LHL82 '(4 4 4 4 8 8 4 4 4 4 16) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (16 CONFLATE 0 16) (32 CONFIRM 0 16)))
  (CHECK "12B, God save the queen"
         '(4 4 4 6 2 4 4)
         (LHL82 '(4 4 4 6 2 4 4) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (16 STRETCH 0 12) (24 CONFLATE 0 24)))
  (CHECK "13"
         '(2 2 4 2 2 4)
         (LHL82 '(2 2 4 2 2 4) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((2 INITIALIZE 0 2) (4 CONFLATE 0 4) (7 UPDATE 4 4) (12 CONFLATE 4 8)))
  (CHECK "14"
         '(1 1 2 2 4 2 2 4)
         (LHL82 '(1 1 2 2 4 2 2 4) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((1 INITIALIZE 0 1) (2 CONFLATE 0 2) (3 UPDATE 2 2) (6 CONFLATE 2 4) (9 UPDATE 6 4)
           (14 CONFLATE 6 8)))
  (CHECK "15"
         '(4 6 2 2 2 2 2)
         (LHL82 '(4 6 2 2 2 2 2) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((4 INITIALIZE 0 4) (8 UPDATE 4 4) (12 CONFLATE 4 8) (20 CONFLATE 4 16)))
  (CHECK "16"
         '(2 5 1 1 1 2 1 1 2)
         (LHL82 '(2 5 1 1 1 2 1 1 2) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((2 INITIALIZE 0 2) (4 UPDATE 2 2) (7 LONGNOTE 2 5) (11 STRETCH 2 8)))
  (CHECK "17"
         '(2 1 1 2 2 2 2 4 2 1 1 2 2 2 2 2)
         (LHL82 '(2 1 1 2 2 2 2 4 2 1 1 2 2 2 2 2) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((2 INITIALIZE 0 2) (4 CONFLATE 0 4) (8 CONFLATE 0 8) (14 STRETCH 0 12)
           (24 CONFLATE 0 24)))
  (CHECK "19, Schubert C-major symphony, first mov."
         '(8 4 4 6 2 8 6 2 8 8 4 4 6 2 8 6 2 8 12 4 16)
         (LHL82 '(8 4 4 6 2 8 6 2 8 8 4 4 6 2 8 6 2 8 12 4 16) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((8 INITIALIZE 0 8) (16 CONFLATE 0 16) (32 CONFIRM 0 16)))
  (CHECK "20"
         '(4 2 4 2 2 2 2 6 4 2 4 2 6 6)
         (LHL82 '(4 2 4 2 2 2 2 6 4 2 4 2 6 6) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((4 INITIALIZE 0 4) (8 STRETCH 0 6) (12 CONFLATE 0 12) (20 STRETCH 0 18)
           (36 CONFIRM 0 18)))
  (CHECK "21"
         '(4 2 4 2 2 2 2 2 4 2 4 2 6)
         (LHL82 '(4 2 4 2 2 2 2 2 4 2 4 2 6) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((4 INITIALIZE 0 4) (8 STRETCH 0 6) (12 CONFLATE 0 12) (22 STRETCH 0 20)))
  (CHECK "B"
         '(1 2 9 1 2 9 1 2 4 2 3 1 2)
         (LHL82 '(1 2 9 1 2 9 1 2 4 2 3 1 2) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((1 INITIALIZE 0 1) (2 UPDATE 1 1) (3 CONFLATE 1 2) (5 UPDATE 3 2) (12 LONGNOTE 3 9)
           (14 STRETCH 3 10) (17 STRETCH 3 12) (27 CONFLATE 3 24)))
  (CHECK "D"
         '(2 4 2 2 4 2 1 1 4 2 2 4 2 1 1 4 2 2 6)
         (LHL82 '(2 4 2 2 4 2 1 1 4 2 2 4 2 1 1 4 2 2 6) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((2 INITIALIZE 0 2) (4 UPDATE 2 2) (6 CONFLATE 2 4) (10 CONFLATE 2 8) (18 CONFLATE 2 16)
           (34 CONFIRM 2 16)))
  (CHECK "G"
         '(3 1 6 2 3 1 6 2 3 1 6)
         (LHL82 '(3 1 6 2 3 1 6 2 3 1 6) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((3 INITIALIZE 0 3) (5 STRETCH 0 4) (7 UPDATE 4 4) (12 CONFLATE 4 8) (19 STRETCH 4 12)
           (28 CONFLATE 4 24)))
  (CHECK "I"
         '(4 4 1 1 1 1 2 2 4)
         (LHL82 '(4 4 1 1 1 1 2 2 4) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (13 STRETCH 0 12) (18 STRETCH 0 16)))
  (CHECK "J"
         '(4 4 2 2 6 2 4)
         (LHL82 '(4 4 2 2 6 2 4) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (14 STRETCH 0 12) (24 CONFLATE 0 24)))
  (CHECK "K"
         '(4 4 2 2 3 1 2 2 2 2)
         (LHL82 '(4 4 2 2 3 1 2 2 2 2) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (14 STRETCH 0 12) (24 CONFLATE 0 24)))
  (CHECK "L"
         '(4 4 2 2 3 1 4 4 8)
         (LHL82 '(4 4 2 2 3 1 4 4 8) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (14 STRETCH 0 12) (19 STRETCH 0 16)
           (32 CONFIRM 0 16)))
  (CHECK "N"
         '(4 4 2 2 4 4 2 2 4)
         (LHL82 '(4 4 2 2 4 4 2 2 4) :TRACE 'NOW-RULE-T1-BEAT :MAX-BEAT 30)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (14 STRETCH 0 12) (24 CONFLATE 0 24))))

;(INSTALL-DIAGNOSE-SUB-MENU "LHL 82" #'LHL82-DIAGNOSE)

(defun check (label rhythm expr-result expected-result)
  (format t "~a ~a = ~a~%" label rhythm
	  (equalp expr-result expected-result)))

|#
