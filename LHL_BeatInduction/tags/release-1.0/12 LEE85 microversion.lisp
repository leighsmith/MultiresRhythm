;;; C.S. Lee's 1985 paper interpreted as a beat induction algorithm
;;;   according to 
;;;   Lee, C. S. (1985) The rhythmic interpretation of 
;;;   simple musical sequences: towards a perceptual 
;;;   model. In R. West, P. Howell, & I. Cross, Musical 
;;;   Structure and Cognition. 53-69. London: Academic Press.


;****************************************************************************************
; toplevel

(defclass LEE-85-state (LHL-82-old-state)())

(defmethod t4 ((state LEE-85-state))
  (+ (t1 state) (* 3 (beat state))))

(defmethod note-on-t4? ((state LEE-85-state))
  (note-on-t? state (t4 state)))

(defun LEE-85 (data &key debug (trace #'now-rule-t1-beat))
  (run 'LEE-85-state
       data
       :trace trace 
       :debug debug 
       :sets '(((may-initialize?  initialize) initialize)
               ((may-update?      update)
                (may-stretch?     stretch)
                (may-shift?       shift) nil))))

;(LEE-85 '(3 1 6 2 2 2 6 2 2 2)) ;ex. 49
;(LEE-85 '(4 6 2 4 4))           ;ex. 5
;(LEE-85 '(4 4 8 4 4 8 4 4 8))   ;ex. 25
;(LEE-85 '(4 4 4 4 4 4 4 8 8 4)) ; ex. 50

;(print-LEE-85 '(4 4 4 4 4 4 4 8 8 4))
;****************************************************************************************
;;; Rule 0 (p. 64) 'INITIALIZE'
;;; start by placing t1 on the first note and t2 on the second
;;; (and calculate t3 accordingly)

; as lhl85

;****************************************************************************************
;;; Rule 1 (p. 64) 'stretch'
;;; if there is a note between t2 and t3 that is longer than the note on t2
;;; move t2 to this note (and t3 accordingly)

(defmethod may-stretch? ((state LEE-85-state) &rest ignore)
  (and (>= (now state) (t3 state))
       (longer-note-after-t2-than-on-t2? state)))

;****************************************************************************************
;;; Rule 2 (p. 64) 'update'
;;; if there is a note that outlasts the current beat (in interval t2,t3)
;;; move t1 to that note and t2 to the next note occuring on a beat (t4, t5, ...)
;;; (and t3 accordingly)

(defmethod may-update? ((state LEE-85-state) &rest ignore)
  (and (or (note-on-t3? state)
           (note-on-t4? state))
       (duration-of-note-on-t2 state)
       (> (duration-of-note-on-t2 state) (beat state))))

(defmethod update ((state LEE-85-state) &rest ignore)
  (let ((new-t2 (or (note-on-t3? state)
                    (note-on-t4? state))))
    (setf (t1 state) (t2 state))
    (setf (t2 state) new-t2)))

;****************************************************************************************
;;; Rule 3 (p. 64+66) 'shift' (named in letter CONFIRM)
;;; if rule 1 and rule 2 do not apply keep processing sequence

(defmethod may-shift? ((state LEE-85-state) &rest ignore)
  (or (>= (now state) (t4 state))
      (and (>= (now state) (t3 state))
           (or (null (note-on-t2? state)) ; no update
               (and (duration-of-note-on-t2 state)
                    (<= (duration-of-note-on-t2 state) (beat state)))))))

(defmethod shift ((state LEE-85-state) &rest ignore)
  (setf (t1 state) (t2 state)))

;****************************************************************************************
; tapper

;(foot-tapper #'LEE-85 '(1 2 9 1 2 9 1 2 4 2 3 1 2))

;****************************************************************************************
; print-elaborate-trace

;(print-process-trace #'LEE-85 '(12 12 4 4 4 4 4 4))
;(print-process-trace #'LEE-85 '(1 2 4 2 4 2 4))           ; ex. letter A 
;(print-process-trace #'LEE-85 '(3 1 6 2 2 2 6 2 2 2))     ; ex. letter B
;(print-process-trace #'LEE-85 '(2 2 2 6 2 2 2 6 2 2 2 6)) ; ex. letter C
;(print-process-trace #'LEE-85 '(3 1 6 2 2 2 6 2 2 2))

;=>
#|
; *=now, rule on next line fires
; action => new beat [start, duration]
; 1=t1, 2=t2, 3=t3, |=beat

                        3 1 6 2 2 2 6 2 2 2 ;
                        !..!!.....!.!.!.!.....!.!.!.!
                           *(=3)
 0-INITIALIZE => [0 3]  1..2..3..|..|..|..|..|..|..|.
                              *(=6)
 stretch => [0 4]     1...2...3...|...|...|...|...|
                                    *(=12)
 update => [4 8]      ....1.......2.......3.......|
                                            *(=20)
 stretch => [4 12]    ....1...........2...........3

|#

;(print-process-traces #'LEE-85 (lhl82-examples))
;(print-process-trace #'LEE-85 (first (nth 8 (lhl82-examples))))

;****************************************************************************************
; examples

(defun LEE-85-examples ()
  '(((3 1 6 2 2 2 6 2 2 2) "44-49")
    ((12 12 4 4 4 4 4 4) "9")
    ((8 4 4 8 4 4 8 4 4 8) "19")
    ((6 2 2 2 2 2 2 6 2 2 2 2 2 2 6) "20")
    ((8 6 2 8 4 2 2 8 2 2 2 2 6) "21")
    ((6 6 2 2 2 6 2 2 2 2 2 2 6) "26")
    ((4 6 2 4 4) "5")
    ((4 4 8 4 4 8 4 4 8) "25")
    ((4 4 4 4 4 4 4 8 8 4) "50")
    ((4 6 2 2 2 2 2 6 2 2 2) "42")
    ((1 2 9 1 2 9 1 2 4 2 3 1 2) "LHL example B, not mentioned in LEE-85")
    ;add rest
    ))

;****************************************************************************************
; diagnose

;(make-diagnose 'LEE-85-diagnose- "LEE 85" 'LEE-85 (LEE-85-examples) :trace 'now-rule-t1-beat)

#|

(defun LEE-85-diagnose ()
  (CHECK "44-49"
        '(3 1 6 2 2 2 6 2 2 2)
        (LEE-85 '(3 1 6 2 2 2 6 2 2 2))
        '((3 INITIALIZE 0 3) (6 STRETCH 0 4) (12 update 4 8) (20 STRETCH 4 12)(28 shift 16 12)))
 (CHECK "9"
        '(12 12 4 4 4 4 4 4)
        (LEE-85 '(12 12 4 4 4 4 4 4))
        '((12 INITIALIZE 0 12) (24 shift 12 12) (36 shift 24 12)(48 shift 36 12)))
 (CHECK "19"
        '(8 4 4 8 4 4 8 4 4 8)
        (LEE-85 '(8 4 4 8 4 4 8 4 4 8))
        '((8 INITIALIZE 0 8) (16 shift 8 8) (24 shift 16 8) (32 shift 24 8)
          (40 shift 32 8) (48 shift 40 8)(56 shift 48 8)))
 (CHECK "20"
        '(6 2 2 2 2 2 2 6 2 2 2 2 2 2 6)
        (LEE-85 '(6 2 2 2 2 2 2 6 2 2 2 2 2 2 6))
        '((6 INITIALIZE 0 6) (12 shift 6 6) (18 shift 12 6) (24 shift 18 6)
          (30 shift 24 6) (36 shift 30 6)(42 shift 36 6)))
 (CHECK "21"
        '(8 6 2 8 4 2 2 8 2 2 2 2 6)
        (LEE-85 '(8 6 2 8 4 2 2 8 2 2 2 2 6))
        '((8 INITIALIZE 0 8) (16 shift 8 8) (24 shift 16 8) (32 shift 24 8)
          (40 shift 32 8) (48 shift 40 8)))
 (CHECK "26"
        '(6 6 2 2 2 6 2 2 2 2 2 2 6)
        (LEE-85 '(6 6 2 2 2 6 2 2 2 2 2 2 6))
        '((6 INITIALIZE 0 6) (12 shift 6 6) (18 shift 12 6) (24 shift 18 6)
          (30 shift 24 6) (36 shift 30 6)(42 shift 36 6)))
 (CHECK "5" '(4 6 2 4 4) (LEE-85 '(4 6 2 4 4)) '((4 INITIALIZE 0 4) (12 update 4 8)(20 shift 12 8)))
 (CHECK "25"
        '(4 4 8 4 4 8 4 4 8)
        (LEE-85 '(4 4 8 4 4 8 4 4 8))
        '((4 INITIALIZE 0 4) (8 shift 4 4) (16 update 8 8) (24 shift 16 8)
          (32 shift 24 8) (40 shift 32 8)(48 shift 40 8)))
 (CHECK "50"
        '(4 4 4 4 4 4 4 8 8 4)
        (LEE-85 '(4 4 4 4 4 4 4 8 8 4))
        '((4 INITIALIZE 0 4) (8 shift 4 4) (12 shift 8 4) (16 shift 12 4)
          (20 shift 16 4) (24 shift 20 4) (28 shift 24 4) (36 update 28 8)
          (44 shift 36 8)))
 (CHECK "42"
        '(4 6 2 2 2 2 2 6 2 2 2)
        (LEE-85 '(4 6 2 2 2 2 2 6 2 2 2))
        '((4 INITIALIZE 0 4) (12 update 4 8) (20 shift 12 8) (28 shift 20 8)))
 (CHECK "LHL example B, not mentioned in LEE-85"
        '(1 2 9 1 2 9 1 2 4 2 3 1 2)
        (LEE-85 '(1 2 9 1 2 9 1 2 4 2 3 1 2))
        '((1 INITIALIZE 0 1) (3 update 1 2) (13 update 3 9) (21 STRETCH 3 12)
          (27 shift 15 12))))

(INSTALL-DIAGNOSE-SUB-MENU "LEE 85" #'LEE-85-DIAGNOSE)



|#