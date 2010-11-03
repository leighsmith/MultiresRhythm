
(defclass LH-shoe-state (LHL-82-state)())

(defmethod t0 ((state LH-shoe-state))
  (and (note-on-now? state)
       (onset-of-note-ending-on-t state (now state))))

(defmethod d0 ((state LH-shoe-state))
  (and (note-on-now? state)
       (duration-of-note-ending-on-t state (now state))))

(defun LH-shoe (data &key 
                   debug
                   (min-beat 8)
                   (maximum-updatable-beat 4)
                   (trace #'now-rule-t1-beat))
  (run 'LH-shoe-state
       data
       :trace trace 
       :debug debug 
       :sets '(((may-initialize?  initialize) initialize)
                ((may-stretch?  stretch)
                 (may-update?   update)
                 (may-conflate? conflate)
                 (may-confirm? confirm)
                 confirm))
       :args (list :min-beat min-beat :maximum-updatable-beat maximum-updatable-beat)))

(defmethod apply-rule ((state LH-shoe-state) rules &rest args)
  (apply #'apply-each-rule-in-order-once state rules args))

;;;**************************************************************************************
  
(defmethod may-stretch? ((state LH-shoe-state) &rest ignore)
  (and (note-on-now? state)
       (note-on-t2? state)
       (< (t2 state) (t0 state) (t3 state))
       (> (d0 state)
          (duration-of-note-on-t2 state))))

(defmethod stretch ((state LH-shoe-state) &rest ignore)
  (setf (t2 state) (t0 state)))

;;;**************************************************************************************

(defmethod may-update? ((state LH-shoe-state) &key maximum-updatable-beat &allow-other-keys)
  (and (duration-of-note-on-t2 state)
       (> (duration-of-note-on-t2 state) (duration-of-note-on-t1 state))
       (<= (beat state) maximum-updatable-beat)))

(defmethod update ((state LH-shoe-state) &rest ignore)
  (setf (t1 state) (t2 state)
        (t2 state) (now state)))

;;;**************************************************************************************

(defmethod may-conflate? :around ((state LH-shoe-state) &rest args)
  (and (= (now state) (t3 state))
       (call-next-method)))

(defmethod may-grow-beat? ((state LH-shoe-state) &key min-beat &allow-other-keys) 
  (< (beat state) min-beat))

;;;**************************************************************************************

(defmethod may-confirm? ((state LH-shoe-state) &key min-beat &allow-other-keys)
  (and (= (now state) (t3 state))
       (note-on-t3? state)
       (>= (beat state) min-beat)))

;;;**************************************************************************************

(defun LH-SHOE-examples ()
  ;rhythm/beat (start duration)/new name and name in LH&L82 (if present)
  '(((4 2) nil "p0")
    ((4 2 2 4) nil "p1")
    ((4 2 3 3) nil "p2")
    ((4 2 4 2) nil "p3")
    ((2 1 1 2 4 2) nil "p4")
    ((2 1 1 2 4 2 4) nil "L1; 1, cliche")
    ((8 2 2 8 2 2 4 2 4 2 6 6 8 2 2 8 2 2 2 2 2 2 2 2 12) (24 12) "L2; 2, Beethoven piano sonate op 109")
    ((2 2 8 2 2 8 2 2 2 2 2 2 2 2 2 2 2 2 2 2 8) (28 12) "L3; 3,4 Beethoven dance (ex. Simon & Sumner, 1968")
    ((1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1) (18 8) "L5; 5,6 Bach, WTC, book 1, Fuge 2")
    ((4 4 4 4 4 4 4 4 4 4) (16 8) "L7; 7,isochronous")
    ((4 2 4 2 2 2 2 6) nil "L8a; 8A, pop goes the weasel")
    ((6 2 4 2 2 2 2 6) (16 8) "L8b; 8B, deutchland uber alles")
    ((4 6 2 4 4 6 2 4 4 6 2 4 4 8) (20 8) "L9; 9, Auld lang syne [extended]")
    ((3 3 6 3 3 6 3 3 6) nil "L11")
    ((4 4 4 4 8 8 4 4 4 4 16) (16 8) "L12a; 12A, onward christian soldiers")
    ((4 4 4 6 2 4 4 4 4 6 2 4 4 4 4 12) (24 12) "L12b; 12B, god save the queen [extended]")
    ((2 2 4 2 2 4) nil "L13; 13 [doubled duration]")
    ((1 1 2 2 4 2 2 4) nil "L14; 14")
    ((4 6 2 2 2 2 2) nil "L15; 15 [doubled duration]")
    ((2 5 1 1 1 2 1 1 2) nil "L16; 16")
    ((2 1 1 2 2 2 2 4 2 1 1 2 2 2 2 2) (24 12) "L17; 17")
    ((8 4 4 6 2 8 6 2 8 8 4 4 6 2 8 6 2 8 6 2 8 12 4 16) (16 8) "L19; 19, Schubert C-major symphony, first mov.")
    ((4 2 4 2 2 2 2 6 4 2 4 2 6 6) (36 18) "L20; 20 [doubled duration]")
    ((4 2 4 2 2 2 2 2 4 2 4 2 6) nil "L21; 21 [doubled duration]")
    ((1 2 9 1 2 9 1 2 4 2 3 1 2 1 3 2 3 1 2 9 1 2 9 1 2 4 2 2 2 2 6) (27 12) "B; B [extended]")
    ((2 4 2 2 4 2 1 1 4 2 2 4 2 1 1 4 2 2 6) (18 8) "D; D")
    ((1 3 1 4 4 4 4 6 2 3 1 3 1 4 8 3 1 6) (21 8) "E")
    (() nil "F[no pattern]")
    ((3 1 6 2 3 1 6 2 3 1 6 2 2 2 6) (28 12) "G; G [extended]")
    ((4 8 8 2 2 2 2 4 4 12 2 2 12) (20 8) "H")
    ((4 4 1 1 1 1 2 2 4) nil "I; I")
    ((4 4 2 2 6 2 4) nil "J; J [doubled duration]")
    ((4 4 2 2 3 1 2 2 2 2) nil "K; K")
    ((4 4 2 2 3 1 4 4 8) nil "L; L")
    ((4 6 2 2 2 2 2 4 4 4 2 2 4 4 3 1 2 2 8 4) (36 16) "M")
    ((4 4 2 2 4 4 2 2 4) (24 12) "N; N [doubled duration]")))

;;;**************************************************************************************


#|
(make-diagnose 'LH-SHOE-diagnose "LH shoe" 'LH-shoe (LH-shoe-examples) 
               :trace 'now-rule-t1-beat :MIN-BEAT 8 :maximum-updatable-beat 4)

(DEFUN LH-SHOE-DIAGNOSE ()
  (CHECK "p0" '(4 2) (LH-SHOE '(4 2) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4) '((4 INITIALIZE 0 4)))
  (CHECK "p1"
         '(4 2 2 4)
         (LH-SHOE '(4 2 2 4) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8)))
  (CHECK "p2"
         '(4 2 3 3)
         (LH-SHOE '(4 2 3 3) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (9 STRETCH 0 6) (12 CONFLATE 0 12)))
  (CHECK "p3"
         '(4 2 4 2)
         (LH-SHOE '(4 2 4 2) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (10 STRETCH 0 6) (12 CONFLATE 0 12)))
  (CHECK "p4"
         '(2 1 1 2 4 2)
         (LH-SHOE '(2 1 1 2 4 2) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((2 INITIALIZE 0 2) (4 CONFLATE 0 4) (10 STRETCH 0 6) (12 CONFLATE 0 12)))
  (CHECK "L1; 1, cliche"
         '(2 1 1 2 4 2 4)
         (LH-SHOE '(2 1 1 2 4 2 4) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((2 INITIALIZE 0 2) (4 CONFLATE 0 4) (10 STRETCH 0 6) (12 CONFLATE 0 12)))
  (CHECK "L2; 2, Beethoven piano sonate op 109"
         '(8 2 2 8 2 2 4 2 4 2 6 6 8 2 2 8 2 2 2 2 2 2 2 2 12)
         (LH-SHOE '(8 2 2 8 2 2 4 2 4 2 6 6 8 2 2 8 2 2 2 2 2 2 2 2 12)
                  :MIN-BEAT
                  8
                  :MAXIMUM-UPDATABLE-BEAT
                  4)
         '((8 INITIALIZE 0 8) (20 STRETCH 0 12) (24 CONFIRM 0 12)))
  (CHECK "L3; 3,4 Beethoven dance (ex. Simon & Sumner, 1968"
         '(2 2 8 2 2 8 2 2 2 2 2 2 2 2 2 2 2 2 2 2 8)
         (LH-SHOE '(2 2 8 2 2 8 2 2 2 2 2 2 2 2 2 2 2 2 2 2 8)
                  :MIN-BEAT
                  8
                  :MAXIMUM-UPDATABLE-BEAT
                  4)
         '((2 INITIALIZE 0 2) (4 CONFLATE 0 4) (12 UPDATE 4 8) (24 STRETCH 4 12)
           (28 CONFIRM 4 12)))
  (CHECK "L5; 5,6 Bach, WTC, book 1, Fuge 2"
         '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1)
         (LH-SHOE '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1)
                  :MIN-BEAT
                  8
                  :MAXIMUM-UPDATABLE-BEAT
                  4)
         '((1 INITIALIZE 0 1) (2 CONFLATE 0 2) (4 UPDATE 2 2) (6 CONFLATE 2 4) (10 CONFLATE 2 8)
           (18 CONFIRM 2 8)))
  (CHECK "L7; 7,isochronous"
         '(4 4 4 4 4 4 4 4 4 4)
         (LH-SHOE '(4 4 4 4 4 4 4 4 4 4) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (16 CONFIRM 0 8)))
  (CHECK "L8a; 8A, pop goes the weasel"
         '(4 2 4 2 2 2 2 6)
         (LH-SHOE '(4 2 4 2 2 2 2 6) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (10 STRETCH 0 6) (12 CONFLATE 0 12) (24 STRETCH 0 18)))
  (CHECK "L8b; 8B, deutchland uber alles"
         '(6 2 4 2 2 2 2 6)
         (LH-SHOE '(6 2 4 2 2 2 2 6) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((6 INITIALIZE 0 6) (12 STRETCH 0 8) (16 CONFIRM 0 8)))
  (CHECK "L9; 9, Auld lang syne [extended]"
         '(4 6 2 4 4 6 2 4 4 6 2 4 4 8)
         (LH-SHOE '(4 6 2 4 4 6 2 4 4 6 2 4 4 8) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (10 UPDATE 4 6) (16 STRETCH 4 8) (20 CONFIRM 4 8)))
  (CHECK "L11"
         '(3 3 6 3 3 6 3 3 6)
         (LH-SHOE '(3 3 6 3 3 6 3 3 6) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((3 INITIALIZE 0 3) (6 CONFLATE 0 6) (12 CONFLATE 0 12) (24 STRETCH 0 18)
           (36 CONFIRM 0 18)))
  (CHECK "L12a; 12A, onward christian soldiers"
         '(4 4 4 4 8 8 4 4 4 4 16)
         (LH-SHOE '(4 4 4 4 8 8 4 4 4 4 16) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (16 CONFIRM 0 8)))
  (CHECK "L12b; 12B, god save the queen [extended]"
         '(4 4 4 6 2 4 4 4 4 6 2 4 4 4 4 12)
         (LH-SHOE '(4 4 4 6 2 4 4 4 4 6 2 4 4 4 4 12) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (18 STRETCH 0 12) (24 CONFIRM 0 12)))
  (CHECK "L13; 13 [doubled duration]"
         '(2 2 4 2 2 4)
         (LH-SHOE '(2 2 4 2 2 4) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((2 INITIALIZE 0 2) (4 CONFLATE 0 4) (8 UPDATE 4 4) (12 CONFLATE 4 8)))
  (CHECK "L14; 14"
         '(1 1 2 2 4 2 2 4)
         (LH-SHOE '(1 1 2 2 4 2 2 4) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((1 INITIALIZE 0 1) (2 CONFLATE 0 2) (4 UPDATE 2 2) (6 CONFLATE 2 4) (10 UPDATE 6 4)
           (14 CONFLATE 6 8)))
  (CHECK "L15; 15 [doubled duration]"
         '(4 6 2 2 2 2 2)
         (LH-SHOE '(4 6 2 2 2 2 2) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (10 UPDATE 4 6) (16 CONFLATE 4 12)))
  (CHECK "L16; 16"
         '(2 5 1 1 1 2 1 1 2)
         (LH-SHOE '(2 5 1 1 1 2 1 1 2) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((2 INITIALIZE 0 2) (7 UPDATE 2 5) (12 STRETCH 2 8)))
  (CHECK "L17; 17"
         '(2 1 1 2 2 2 2 4 2 1 1 2 2 2 2 2)
         (LH-SHOE '(2 1 1 2 2 2 2 4 2 1 1 2 2 2 2 2) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((2 INITIALIZE 0 2) (4 CONFLATE 0 4) (8 CONFLATE 0 8) (16 STRETCH 0 12)
           (24 CONFIRM 0 12)))
  (CHECK "L19; 19, Schubert C-major symphony, first mov."
         '(8 4 4 6 2 8 6 2 8 8 4 4 6 2 8 6 2 8 6 2 8 12 4 16)
         (LH-SHOE '(8 4 4 6 2 8 6 2 8 8 4 4 6 2 8 6 2 8 6 2 8 12 4 16)
                  :MIN-BEAT
                  8
                  :MAXIMUM-UPDATABLE-BEAT
                  4)
         '((8 INITIALIZE 0 8) (16 CONFIRM 0 8)))
  (CHECK "L20; 20 [doubled duration]"
         '(4 2 4 2 2 2 2 6 4 2 4 2 6 6)
         (LH-SHOE '(4 2 4 2 2 2 2 6 4 2 4 2 6 6) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (10 STRETCH 0 6) (12 CONFLATE 0 12) (24 STRETCH 0 18)
           (36 CONFIRM 0 18)))
  (CHECK "L21; 21 [doubled duration]"
         '(4 2 4 2 2 2 2 2 4 2 4 2 6)
         (LH-SHOE '(4 2 4 2 2 2 2 2 4 2 4 2 6) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (10 STRETCH 0 6) (12 CONFLATE 0 12) (24 STRETCH 0 20)
           (38 STRETCH 0 32)))
  (CHECK "B; B [extended]"
         '(1 2 9 1 2 9 1 2 4 2 3 1 2 1 3 2 3 1 2 9 1 2 9 1 2 4 2 2 2 2 6)
         (LH-SHOE '(1 2 9 1 2 9 1 2 4 2 3 1 2 1 3 2 3 1 2 9 1 2 9 1 2 4 2 2 2 2 6)
                  :MIN-BEAT
                  8
                  :MAXIMUM-UPDATABLE-BEAT
                  4)
         '((1 INITIALIZE 0 1) (3 UPDATE 1 2) (12 UPDATE 3 9) (15 STRETCH 3 10)
           (24 STRETCH 3 12) (27 CONFIRM 3 12)))
  (CHECK "D; D"
         '(2 4 2 2 4 2 1 1 4 2 2 4 2 1 1 4 2 2 6)
         (LH-SHOE '(2 4 2 2 4 2 1 1 4 2 2 4 2 1 1 4 2 2 6) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((2 INITIALIZE 0 2) (6 UPDATE 2 4) (10 CONFLATE 2 8) (18 CONFIRM 2 8)))
  (CHECK "E"
         '(1 3 1 4 4 4 4 6 2 3 1 3 1 4 8 3 1 6)
         (LH-SHOE '(1 3 1 4 4 4 4 6 2 3 1 3 1 4 8 3 1 6) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((1 INITIALIZE 0 1) (4 UPDATE 1 3) (9 STRETCH 1 4) (9 UPDATE 5 4)
           (13 CONFLATE 5 8) (21 CONFIRM 5 8)))
  (CHECK "F[no pattern]" 'NIL (LH-SHOE 'NIL :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4) 'NIL)
  (CHECK "G; G [extended]"
         '(3 1 6 2 3 1 6 2 3 1 6 2 2 2 6)
         (LH-SHOE '(3 1 6 2 3 1 6 2 3 1 6 2 2 2 6) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((3 INITIALIZE 0 3) (10 STRETCH 0 4) (10 UPDATE 4 6) (15 STRETCH 4 8)
           (22 STRETCH 4 12) (28 CONFIRM 4 12)))
  (CHECK "H"
         '(4 8 8 2 2 2 2 4 4 12 2 2 12)
         (LH-SHOE '(4 8 8 2 2 2 2 4 4 12 2 2 12) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (12 UPDATE 4 8) (20 CONFIRM 4 8)))
  (CHECK "I; I"
         '(4 4 1 1 1 1 2 2 4)
         (LH-SHOE '(4 4 1 1 1 1 2 2 4) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (14 STRETCH 0 12) (20 STRETCH 0 16)))
  (CHECK "J; J [doubled duration]"
         '(4 4 2 2 6 2 4)
         (LH-SHOE '(4 4 2 2 6 2 4) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (18 STRETCH 0 12) (24 CONFIRM 0 12)))
  (CHECK "K; K"
         '(4 4 2 2 3 1 2 2 2 2)
         (LH-SHOE '(4 4 2 2 3 1 2 2 2 2) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (15 STRETCH 0 12) (24 CONFIRM 0 12)))
  (CHECK "L; L"
         '(4 4 2 2 3 1 4 4 8)
         (LH-SHOE '(4 4 2 2 3 1 4 4 8) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (15 STRETCH 0 12) (20 STRETCH 0 16)
           (32 STRETCH 0 24)))
  (CHECK "M"
         '(4 6 2 2 2 2 2 4 4 4 2 2 4 4 3 1 2 2 8 4)
         (LH-SHOE '(4 6 2 2 2 2 2 4 4 4 2 2 4 4 3 1 2 2 8 4)
                  :MIN-BEAT
                  8
                  :MAXIMUM-UPDATABLE-BEAT
                  4)
         '((4 INITIALIZE 0 4) (10 UPDATE 4 6) (16 CONFLATE 4 12) (24 STRETCH 4 16)
           (36 CONFIRM 4 16)))
  (CHECK "N; N [doubled duration]"
         '(4 4 2 2 4 4 2 2 4)
         (LH-SHOE '(4 4 2 2 4 4 2 2 4) :MIN-BEAT 8 :MAXIMUM-UPDATABLE-BEAT 4)
         '((4 INITIALIZE 0 4) (8 CONFLATE 0 8) (16 STRETCH 0 12) (24 CONFIRM 0 12))))

(INSTALL-DIAGNOSE-SUB-MENU "LH shoe" #'LH-shoe-diagnose)




|#