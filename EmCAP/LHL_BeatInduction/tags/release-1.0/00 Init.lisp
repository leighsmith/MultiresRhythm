;; to startup beat-induction models
;; load this first:

; (load "Applications/LispWorks-MIDI/00 portmidi-install.lisp")

; then evaluate whole:

(progn
;; load facility

(defun smart-load (files)
  (let ((dir (pathname-directory *load-pathname*) ))
    (loop for file in files
          as lisp-file =  (make-pathname :directory dir :name  (pathname-name file) :type "lisp")
          as fasl-file =  (make-pathname :directory dir :name  (pathname-name file) :type "xfasl")
          do (compile-file-if-needed lisp-file :output-file fasl-file :load t)
          ;do (format t "~%; Loading  ~S" fasl-file)
          )))

;; BI code
(smart-load '("00 shoe-graphics"
              "01 rule-based system"
              "02 tap functions" 
              "03 foot tapper"
              "04 trace printer" 
              "10 LHL82 microversion" 
              "11 LH shoe micro version" 
              "12 LEE85 microversion"))

;; SETUP graphics
;; graphics window

  (defvar *shoe-window*)
  (setf *shoe-window* (capi:display (make-instance 'virtual-shoe)))

;; images

;(defun shoe-up-in-window (&optional (w  *shoe-window*))
;    (test-image-functions-change-image w #P"/Users/Honing/Sites/Website backup/images/shoes/shoe 4.gif"))
;(defun shoe-down-in-window (&optional (w  *shoe-window*))
;  (test-image-functions-change-image w #P"/Users/Honing/Sites/Website backup/images/shoes/shoe 1.gif"))

(defun shoe-up1-in-window (&optional (w  *shoe-window*))
  (test-image-functions-change-image w #P"/Users/Honing/Desktop/LISP Code/BI micro versions/shoe-images/shoe-1.gif"))
(defun shoe-up2-in-window (&optional (w  *shoe-window*))
  (test-image-functions-change-image w #P"/Users/Honing/Desktop/LISP Code/BI micro versions/shoe-images/shoe-2.gif"))
(defun shoe-up3-in-window (&optional (w  *shoe-window*))
  (test-image-functions-change-image w #P"/Users/Honing/Desktop/LISP Code/BI micro versions/shoe-images/shoe-3.gif"))
(defun shoe-down-in-window (&optional (w  *shoe-window*))
  (test-image-functions-change-image w #P"/Users/Honing/Desktop/LISP Code/BI micro versions/shoe-images/shoe-4.gif"))

(shoe-down-in-window) ;shoudl be after method on window

;; main function

(defun visualize-grid-shoe (rhythm beats &optional (delay 1) (touch-time .10))
  ;(format t "~%;visualize-grid-shoe beats: ~{~3$ ~}~%" beats)
  (shoe-up1-in-window)
  (loop for beat in beats
        do (mp:schedule-timer-relative-milliseconds (mp:make-timer 'shoe-down-in-window) 
                                                    (* 1000 (+ delay beat)))
        do (mp:schedule-timer-relative-milliseconds (mp:make-timer 'shoe-up3-in-window) 
                                                    (* 1000 (+ delay beat (* 1 touch-time))))
        do (mp:schedule-timer-relative-milliseconds (mp:make-timer 'shoe-up2-in-window)
                                                    (* 1000 (+ delay beat (* 2 touch-time))))
        do (mp:schedule-timer-relative-milliseconds (mp:make-timer 'shoe-up1-in-window) 
                                                    (* 1000 (+ delay beat (* 3 touch-time))))
        ))
)






;;; **********************************
;;; Music & Cognition Class 28.02.2007

;; Rhythm only:
(foot-tapper #'lhl82  '(3 1 6 2 3 1 6 2 3 1 4 4 )
               :unit 4 :tempo 100 :beat nil)

;; Models presented in class:
(play-grid-shoe (integrate '(3 1 6 2 3 1 6 2 3 1 4 4 )) 
                '(8 12 16 20 24) ; 12beat-grid-onsets: to decide upon in class
                :unit 4 :tempo 100 :beat t )

(play-grid-shoe (integrate '(3 1 6 2 3 1 6 2 3 1 4 4 )) 
                '(10 14) ; beat-grid-onsets: to decide upon in class
                :unit 4 :tempo 100 :beat t )

;; Longuet-Higgins & Lee (1982):
(foot-tapper #'lhl82 '(3 1 6 2 3 1 6 2 3 1 4 4 )
               :unit 4 :tempo 100 :beat t :debug #'print-state-debug)

;; Lee (1985):
(foot-tapper #'lee-85  '(3 1 6 2 3 1 6 2 3 1 4 4 )
               :unit 4 :tempo 100 :beat t :debug #'print-state-debug)

;; Longuet-Higgins (1994):
(foot-tapper #'lh-shoe  '(3 1 6 2 3 1 6 2 3 1 4 4 )
               :unit 4 :tempo 100 :beat t :debug #'print-state-debug)

;;; **********************************












;*******************************************

;CONFLATE
;(foot-tapper #'lhl82 '(2 2 2 2 2 2 2 2 2 2 2 2 2 2) :unit 4 :tempo 120 :debug #'print-state-debug )

;TESTS
;(foot-tapper #'lhl82 '(2 1 1 2 4 2) :unit 4 :tempo 100 :debug #'print-state-debug :tap-function 't3s :beat nil)
;(foot-tapper #'lhl82 '(2 1 1 2 4 2) :unit 4 :tempo 100 :debug #'print-state-debug :tap-function 'filled-in-t3s)
;(foot-tapper #'lee-85 '(2 1 1 2 4 2) :unit 4 :tempo 100 :debug #'print-state-debug )

#| Examples on beat induction:

(foot-tapper #'lhl82  '(3 1 6 2 3 1 6 2 3 1 4 4 3 1 6 2 3 1 6 2 3 1 6 2 3 1 4 4 4)
               :unit 4 :tempo 100 :beat nil)









(progn
  (foot-tapper #'lhl82  '(4 4 4 6 2 4 4 4 4 6 2 4 4 4 4 ) 
               :unit 4 :tempo 100 :beat nil)
  ;(format t "~2%Virtual Tapping Shoe (http://www.hum.uva.nl/mmm/fun.html) ")
  (format t "~2%Playing rhythm  (God Save the Queen) ...."))

(progn
  (format t "~2%Playing rhythm plus beat predicted by ~%Longuet-Higgins 1982 model (tapping on expected beat) ...")
  (foot-tapper #'lhl82 '(4 4 4 6 2 4 4 4 4 6 2 4 4 4 4 ) 
               :unit 4 :tempo 100 ))

(progn
  (format t "~2%Playing rhythm plus beat predicted by ~%Lee 1985 model  (tapping on expected beats) ...")
  (foot-tapper #'lee-85 '(4 4 4 6 2 4 4 4 4 6 2 4 4 4 4 ) 
               :unit 4 :tempo 100  ))

(progn
  (format t "~2%Playing rhythm plus beat predicted by ~%Longuet-Higgins 1994 model  (tapping on expected beats) ...")
  (foot-tapper #'lh-shoe   '(4 4 4 6 2 4 4 4 4 6 2 4 4 4 4 ) 
               :unit 4 :tempo 100  ))



(format t "~2%*******************************************")
|#


#|
;; CCRMA EXAMPLES

(foot-tapper #'lhl82 '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 
                         1 1 1 1 1 1 1 1 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1)
             :unit 4 :tempo 60 :debug #'print-state-debug :beat nil)
(foot-tapper #'lhl82 '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 
                         1 1 1 1 1 1 1 1 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1)
             :unit 4 :tempo 60 :debug #'print-state-debug )
(foot-tapper #'lee-85 '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 
                          1 1 1 1 1 1 1 1 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1)
             :unit 4 :tempo 60 :debug #'print-state-debug )
(foot-tapper #'lh-shoe '(1 1 2 2 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1 
                           1 1 1 1 1 1 1 1 2 1 1 2 2 2 1 1 2 2 1 1 4 1 1)
             :unit 4 :tempo 60 :debug #'print-state-debug)
;;

(foot-tapper #'lhl82  '(3 1 6 2 3 1 6 2 3 1 4 4 3 1 6 2 3 1 6 2 3 1 6 2 3 1 4 4 4)
             :unit 8 :tempo 60 :debug #'print-state-debug :tap-function 't3s :b-key 45 :beat nil)
(foot-tapper #'lhl82  '(3 1 6 2 3 1 6 2 3 1 4 4 3 1 6 2 3 1 6 2 3 1 6 2 3 1 4 4 4)
              :unit 8 :tempo 60 :debug #'print-state-debug :tap-function 't3s :b-key 45)
(foot-tapper #'lee-85  '(3 1 6 2 3 1 6 2 3 1 4 4 3 1 6 2 3 1 6 2 3 1 6 2 3 1 4 4 4)
             :unit 8 :tempo 60 :debug #'print-state-debug :tap-function 't3s :b-key 45)





;(lhl82 '(2 1 1 2 4 2 4) :max-beat 30 :debug #'print-state-debug)

;(print-process-traces #'lhl82 (lhl82-examples) :max-beat 30)
;(print-process-trace #'lhl82 '(2 1 1 2 4 2 4))
;(print-process-trace #'lee-85 '(2 1 1 2 4 2 4))
;(print-process-trace #'lh-shoe '(2 1 1 2 4 2 4))

;(foot-tapper #'lhl82 '(2 1 2) :unit 4 :tempo 60)
;(foot-tapper #'lhl82 '(2 2 2 2 ) :unit 4 :tempo 120 :b-key 56)
;(foot-tapper #'lhl82 '(2 1 1 2 4 2) :unit 4 :tempo 120 :debug #'print-state-debug :tap-function 't3s)
;(foot-tapper #'lhl82 '(2 1 1 2 4 2) :unit 4 :tempo 120 :debug #'print-state-full :tap-function 't3s)

|#
