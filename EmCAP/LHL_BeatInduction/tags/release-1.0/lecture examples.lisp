;;; LECTURE EXAMPLES, 28 april 1994 CCRMA
;;; LECTURE EXAMPLES,  5 may   1994 CNMAT
;;; LECTURE EXAMPLES,  9 may   1994 APPLE ATG, Cupertino
;;; LECTURE EXAMPLES, 12 may   1994 CCRMA, Affiliate days


(setf *shoe-advance* .510)
(initialize-TG100-for-shoe-demo 0 -1 1 -1 2 1)

;;; ***********************************************************************************
;;; fast process, but sometimes impossible

(play-grid-pattern '(2 1 1 2 1 1 2 2 2 1 1 2 1 1 2 1 1 1 1 2) :tempo 60)
(play-grid-pattern '(2 1 3 2 1 3 1 1 1 3 2 1 3 2 1 3 3) :tempo 60)
(play-grid-pattern '(2 4 4 3 3 4 4 2 3 3 2 4 4 3 3 4 4 2 3 3 2 4 4 3 3 4 4 2 3 3) :tempo 100)

(play-grid-pattern '(2 1 3 2 1 3 1 1 1 3 2 1 3 2 1 3 3) :tempo 90)

;;; ***********************************************************************************
;;; global tempo

; 6/8 in two
(play-grid-pattern '(4 2 4 2 2 2 2 4 2 4 2 4 2 2 2 2) :tempo 130)
; 6/8 in three
(play-grid-pattern '(4 2 4 2 2 2 2 4 2 4 2 4 2 2 2 2) :tempo 60)

;;; ***********************************************************************************
;;; persistence/syncopation

(play-grid-pattern '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1) :tempo 90)

;;; ***********************************************************************************
;;; gradual tempo change (rubato)

(play-pattern '(0.241 0.242 0.485 0.490 0.495 0.251 0.254 0.516 0.531 0.550 
                0.283 0.288 0.599 0.634 0.332 0.344 1.526 0.431 0.448 0.888 
                0.853 0.798 0.376 0.358 0.662 0.591 0.525 0.253 0.253 0.506 
                0.506 0.253 0.253 1.011 0.253 0.253))

;;; ***********************************************************************************
;;; sudden tempo change

(play-pattern '(0.253 0.253 0.507 0.507 0.508 0.254 0.254 0.508 0.509 0.509 0.255 0.255 
                0.510 0.379 0.190 0.190 0.759 0.190 0.190 0.379 0.379 0.379 0.190 0.190 
                0.379 0.379 0.379 0.190 0.190 0.379 0.379 0.190 0.190 0.759 0.190 0.190))

;;; ***********************************************************************************
;;; ***********************************************************************************
;;; Rule-based theories 
;;; example G

(play-grid-pattern  '(3 1 6 2 3 1 6 2 3 1) :tempo 100)

;;; ***********************************************************************************
;;; Interprete as model for shoe tapping
;;; l5: WTC book I, Fugue 2

(foot-tapper #'lhl82 '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 
                       1 1 1 1 1 1 1 1 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1)
             :max-beat 12
             :tap-function 'confirmed-beat
             :tempo 60
             :shoe t :beat t)

(foot-tapper #'lee-85 '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 
                1 1 1 1 1 1 1 1 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1)
             :tap-function 't3s
             :tempo 60
             :shoe t :beat t)

(foot-tapper #'lh-shoe '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 
                         1 1 1 1 1 1 1 1 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1)
             :tap-function 'confirmed-beat
             :tempo 60
             :shoe t :beat t)

;;all : t3s t2s tns passed-t3s passed-tns last-beat confirmed-beat filled-in-t3s


;;; **********************************************************************
;;; END   ****************************************************************
;;; **********************************************************************


;;; ***********************************************************************************
;;; their interpretation as computational model

;;; ***********************************************************************************
;;; their interpretation as processmodel

;; illustrate incremental process:
(progn (lhl82 '(3 1 6 2 3 1 6 2 3 1) :max-beat 12 :debug #'print-state-interactive-debug)
       nil)

(print-process-trace #'lhl82 '(3 1 6 2 3 1 6 2 3 1))
(print-process-trace #'lee-85 '(3 1 6 2 3 1 6 2 3 1))
(print-process-trace #'lh-shoe '(3 1 6 2 3 1 6 2 3 1))



;;; ***********************************************************************************
;;; ***********************************************************************************
;;; ***********************************************************************************
;;; LECTURE EXAMPLES, 8 maart 1994 NICI

;;; ***********************************************************************************
;;; isochroon:

(play-grid-pattern '(2 2 2 2 2 2 2 2 2 2 2 2) :tempo 60)

;;; ***********************************************************************************
;;; snel proces:

(play-grid-pattern '(2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 ) :tempo 60)
(play-grid-pattern '(3 2 1 3 2 1 3 2 1 3 2 1 3 2 1) :tempo 60)
(play-grid-pattern '(3 1 6 2 3 1 6 2 3 1 4 4 3 1 6 2 3 1 6 2 3 1 6 2 3 1 4 4 4) :tempo 120)

;;; ***********************************************************************************
;;; soms geen beat-induktie ??

(play-grid-pattern '(2 4 4 3 3 4 4 2 3 3) :tempo 120)

;;; ***********************************************************************************
;;; persistentie/syncopatie

(play-grid-pattern '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1) :tempo 45)

;;; ***********************************************************************************
;;; global tempo

(play-grid-pattern '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4) :tempo 80)

;;; ***********************************************************************************
;;; tempo verloop (rubato)

(play-grid-pattern '(0.225 0.226 0.453 0.457 0.462 0.234 0.237 0.482 0.496 0.513
                0.264 0.269 0.559 0.592 0.31 0.321 1.424 0.402 0.418 0.829 
                0.796 0.745 0.351 0.334 0.618 0.552 0.49 
                0.236 0.236 0.472 0.472 0.236 0.236 0.944 0.236 0.236) 
              :unit .8 :tempo 70)

;;; ***********************************************************************************
;;; tempo sprong

(play-grid-pattern '(0.236 0.236 0.473 0.473 0.474 0.237 0.237 0.474 0.475 
                0.475 0.238 0.238 0.476 0.354 0.177 0.177 0.708 0.177 
                0.177 0.354 0.354 0.354 0.177 0.177 0.354 0.354 0.354 
                0.177 0.177 0.354 0.354 0.177 0.177 0.708 0.177 0.177)
              :unit .8 :tempo 70)

;;; ***********************************************************************************
;;; pattern for uninterpreted trace (G)

(play-grid-pattern  '(3 1 6 2 3 1 6 2 3 1) :tempo 120)

;;; ***********************************************************************************
;;; l5: WTC book I, Fugue 2
;;; short

(setf *beat* t)

(LHL82-tapper '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 )
              :unit 4 :tempo 80 :shoe nil)

(LEE85-tapper '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 )
              :unit 4 :tempo 80 :shoe nil)

(LH-SHOE-tapper '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1)
                :unit 4 :tempo 80 :shoe nil)


;;; ***********************************************************************************
;;; long

(setf *beat* nil)

(LHL82-tapper '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 
                1 1 1 1 1 1 1 1 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1)
              :unit 4 :tempo 80)
;=>
; warning:  beat on 2 predicted too late (skipped)
;    (predicted 188ms in advance, 300ms needed)


(LEE85-tapper '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 
                1 1 1 1 1 1 1 1 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1)
              :unit 4 :tempo 80)
;=>
; warning:  beat on 2 predicted too late (skipped)
;    (predicted 188ms in advance, 300ms needed)
; warning:  beat on 3 predicted too late (skipped)
;    (predicted 188ms in advance, 300ms needed)


(LH-SHOE-tapper '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1
                  1 1 1 1 1 1 1 1 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1)
                :unit 4 :tempo 80)
;=>
; warning:  beat on 2 predicted too late (skipped)
;    (predicted 188ms in advance, 300ms needed)



;;; ***********************************************************************************
;;; rest

(LHL82-tapper '(2 1 1 2 4 2)
              :unit 4 :tempo 80 :shoe nil :beat nil)

(LEE85-tapper '(2 1 1 2 4 2)
              :unit 4 :tempo 80 :shoe nil :beat nil)

(LH-SHOE-tapper '(2 1 1 2 4 2)
                :unit 4 :tempo 80)



(LHL82-tapper '(2 1 1 2 4 2) 
              :unit 4 :tempo 120 :incremental nil :trace-to-tap-fun #'t2s :max-beat 2)
(LHL82-print '(2 1 1 2 4 2) :trace-to-tap-fun #'derive-t3s)
(LHL82-print '(2 1 1 2 4 2) :trace-to-tap-fun #'derive-t2s)
(LHL82-print '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1))
(LHL82-print '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1) :trace-to-tap-fun #'filled-in-t3s)


;
;(LHL82-print '(3 1 2 2 1 3))
;(LHL82-tapper '(3 1 2 2 1 3) :unit 4 :tempo 80)


(play-grid-pattern  '(3 1 6 2 3 1 6 2 3 1 4 4 4) :tempo 120)
(LHL82-tapper '(3 1 6 2 3 1 6 2 3 1 4 4 4)
              :unit 4 :tempo 120 :beat t :shoe nil
              :trace-to-tap-fun #'filled-in-t3s) ;ERROR??

(LHL82-tapper '(2 6 2 2 2 6 2 2 2 6)
              :unit 4 :tempo 120) :beat nil :shoe nil)

;extended WTCI, F2
(play-grid-pattern '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 
                1 1 1 1 1 1 1 1 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1)
              :unit 4 :tempo 80)

(LHL82-tapper '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 
                1 1 1 1 1 1 1 1 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1)
              :unit 4 :tempo 80 :beat t)

(LEE85-tapper '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 
                1 1 1 1 1 1 1 1 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1)
              :unit 4 :tempo 80 :beat t)

(LH-SHOE-tapper '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 
                1 1 1 1 1 1 1 1 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1)
              :unit 4 :tempo 80 :beat t)
|#
;(LEE85-tapper '(3 1 2 2 1 3 3 1 2 2 1 3) :unit 4 :tempo 60 :beat t)

;(LHL82-tapper   '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1) :unit 4 :tempo 100) ; :trace-to-tap-fun #'t3s)
;(LEE85-tapper   '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1) :unit 4 :tempo 100) ; :trace-to-tap-fun #'t3s)
;(LH-SHOE-tapper '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1) :unit 4 :tempo 100) ; :trace-to-tap-fun #'t3s)





#|
(LHL82 '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1))
(SHOE (butlast '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1)))
(LEE85 '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1))
(LHL82-print '(2 1 1 2 4 2 ))
(LH-SHOE-print  '( 4 2 2  4 8 4)) ;'(2 1 1 2 4 2 ))
(LHL82-print '(8 4 4 6 2 8 8 4 4 8 8  8 4 4 8 4 4  8 6 2 16))
(LH-SHOE-print '(8 4 4 6 2 8 8 4 4 8 8  8 4 4 8 4 4  8 6 2 16))
(LH-SHOE-print '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1))
(LEE85-print '(4 4 4 2 2 2 2 8 4 2 2))
(LH-SHOE-print '(4 4 4 2 2 2 2 8 4 2 2))
(LHL82-print '(4 4 4 2 2 2 2 8 4 2 2))



(LHL82-tapper '(2 1 1 2 4 2 4)   
              :unit 4 :tempo 60 :incremental nil :b-repeat 7 :b-advance 0)

(LH-SHOE-tapper '(12 3 3 3 3 4 4 4 6 6 6)
                :unit 4 :tempo 200 :incremental t :b-repeat 3 :b-advance 0)

(LH-SHOE-tapper '(3 3 3 3 4 4 4 6 6 6)
                :unit 4 :tempo 200 :incremental t :b-repeat 3 :b-advance 0)
(LH-SHOE-print '(3 3 3 3 4 4 4 6 6 6))



(LH-SHOE-tapper '(6 6 4 4 4 3 3 3 3 6 6 6)
                :unit 4 :tempo 200 :incremental t :b-repeat 3 :b-advance 0)

(LH-SHOE-tapper '(4 4 4 12 6 6 3 3 6 6 6)
                :unit 4 :tempo 230 :incremental t :b-repeat 3 :b-advance 0)
(LH-SHOE-print '(4 4 4 3 3 3 3 6 6 6))


(LEE85-tapper '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 1)   
              :unit 4 :tempo 120 :incremental t :b-repeat 7 :b-advance 0)

((4 2) nil "p0")
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
((4 4 2 2 4 4 2 2 4) (24 12) "N; N [doubled duration]"))

