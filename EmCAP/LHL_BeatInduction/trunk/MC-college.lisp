;; Code for generating worksheets for M&C course 2007
;; Needs BI models
;; and Todd, Cousins, Lee data



;; load this first:

; (load "Applications/LispWorks-MIDI/00 portmidi-install.lisp")

 (play-rhythm '(1 1 2 1 3 1) :ioi 250)

;(play-rhythm '(4 4 6 2  4 4 6 2 ) :ioi 125)     ;hes
;(play-rhythm '(2 6 4 4  2 6 4 4 ) :ioi 125)  ;ant
;(play-rhythm '(2 4 2 4 4  2 4 2 4 4 ) :ioi 125) ;sync


;(play-rhythm '(3 3 4 3 3  3 3 4 3 3) :ioi 125)  ;bossa

;;; Syncopation

(defun examples ()
  '((hesitation (4 4 6 2))
    (anticipation (2 6 4 4))
    (syncopation (2 4 2 4 4))
    (classical-1 (2 1 1 2 1 1 2 1 1 4))
    (classical-2 (3 1 3 1 3 1 4))
    (shiko (4 2 4 2 4))
    (son (3 3 4 2 4))
    (rumba (3 4 3 2 4))
    (soukous (3 3 4 1 3 2))
    (gahu (3 3 4 4 2))
    (bossa-nova (3 3 4 3 3))))


(defun header (stream &optional (title "title"))
  (format stream "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 TRANSITIONAL//EN\"> ~
        <html> ~
	<head> ~
	<title>~A (c)2007, www.hum.uva.nl/mmmm</title> ~
        <META NAME=\"ROBOTS\" CONTENT=\"NOINDEX, NOFOLLOW\">
	</head> ~
	<body><font face=\"Courier\" Size=+1>" title))

(defun tail (stream)
  (format stream "</font> ~
	</body> ~
        </html>"))

;; generate html file (assuming 'SYN-EX' examples are there):

(with-open-file (stream #P"/Users/Honing/Desktop/syncopation-worksheet.html"
                        :direction :output :if-exists :supersede)
  (loop initially (header stream "Syncopation worksheet")
        for  (name rhythm) in (examples)
        for i from 0
        do (format stream "~% ~A: " i)
        do (print-grid stream rhythm)
        do (format stream "<br> <OBJECT CLASSID=\"clsid:02BF25D5-8C17-4B23-BC80-D3488ABDDC6B\" ~
WIDTH=160 HEIGHT=136 ~
<PARAM name=\"SRC\" VALUE=\"http:/www.hum.uva.nl/mmm/drafts/MC/SYN-EX-~A.mid\"> ~
<PARAM name=\"AUTOPLAY\" VALUE=\"false\"> ~
<PARAM name=\"CONTROLLER\" VALUE=\"true\"> ~
 <PARAM name=\"KIOSKMODE\" VALUE=\"true\"> ~
<EMBED SRC=\"http:/www.hum.uva.nl/mmm/drafts/MC/SYN-EX-~A.mid\" ~
PLUGINSPAGE=\"http://www.apple.com/quicktime/download/\" ~
WIDTH=160 HEIGHT=136 ~
KIOSKMODE=\"true\" type=\"audio/mp4\" ~
CONTROLLER=true LOOP=false AUTOPLAY=false> ~
</EMBED> </OBJECT> <br>"  i i)
        finally (tail stream)))

;-------------------------------------------------------------

                    

;;; Beat induction

(defun examples ()
  '(((2 2 2 2 2 2 2 2) (16 8) "7,isochronous")
    ((4 2 4 2 2 2 2 2) nil "Stretch")
    ((4 6 2 2 2 2 2 2) nil "Update")
    ((2 1 1 2 4 2 4 4 4) nil "Rhythmic cliche")
    ((2 4 4 3 3 4 4 2 3 3 2 2 2) nil "Syncopated")
    ((2 4 3 4 2 6 3 2 2 4) nil "Random")
    ((3 1 6 2 3 1 6 2 3 1 4 4 4) nil nil)
    ((2 2 4 4 4 2 2 4 4 4 2 2 4 4 2 2 8 2 2 2) (36 16) "5,6 Bach, WTC, book 1, Fuge 2")
    ((4 4 4 6 2 4 4 4 4 6 2 4 4 4 4 12) (24 12) "England")
    ((4 4 4 2 2 2 2 4 2 2 4 ) nil "Netherlands")
    ((4 4 4 2 2 2 2 2 2 2 2 2 2 4 4) nil " Spain")
    ))

(loop for  (rhythm beat name) in (examples)
      as i from 0
      do (print-grid t rhythm (format nil "~%~A: " i)))


(loop for  (rhythm beat name) in (examples)
      as i from 0
      as output = (cddr (first (last  (lhl82 rhythm))))
      do (format t "~%~A [~A ~A]" i (first output) ( second output)))

(with-open-file (stream #P"/Users/Honing/Desktop/BI-worksheet.html"
                        :direction :output :if-exists :supersede)
  (loop for  (rhythm beat name) in (examples)
        for i from 0
        do (format stream "~% ~A: " i)
        do (print-grid stream rhythm)
        do (format stream "<br> <OBJECT CLASSID=\"clsid:02BF25D5-8C17-4B23-BC80-D3488ABDDC6B\" ~
WIDTH=160 HEIGHT=136 ~
<PARAM name=\"SRC\" VALUE=\"http:/www.hum.uva.nl/mmm/drafts/MC/EX-~A.mid\"> ~
<PARAM name=\"AUTOPLAY\" VALUE=\"false\"> ~
<PARAM name=\"CONTROLLER\" VALUE=\"true\"> ~
 <PARAM name=\"KIOSKMODE\" VALUE=\"true\"> ~
<EMBED SRC=\"http:/www.hum.uva.nl/mmm/drafts/MC/EX-~A.mid\" ~
PLUGINSPAGE=\"http://www.apple.com/quicktime/download/\" ~
WIDTH=160 HEIGHT=136 ~
KIOSKMODE=\"true\" type=\"audio/mp4\" ~
CONTROLLER=true LOOP=false AUTOPLAY=false> ~
</EMBED> </OBJECT> <br>"  i i)))

                    
;;Todd, Cousins, Lee (2007) stimuli:

(defun play-pattern-in-ms-four-times (&key iois (r-key 60))
  (send-multiple-timed-notes (loop repeat 4 append iois)  r-key 100 2))


;SSL 125
;( play-pattern-in-ms-four-times :iois '(125 125 250))


;SSL 280
;( play-pattern-in-ms-four-times :iois '(280 280 560))


;SSL 706
;(play-pattern-in-ms :iois '(706 706 1412))

; (play-patterns :rhythm-onsets '(8 10 15 16) :beats-onsets'(0 4 8))