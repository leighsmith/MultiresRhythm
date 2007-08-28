;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Essen folksong database decoder to anthems internal notation format.
;;;; 03/03/2003 Henkjan Honing
;;;; Updated by Leigh M. Smith <lsmith@science.uva.nl>

(defpackage #:esac (:use #:cl #:asdf)
	    (:export :read-esac-file))

(in-package :esac)

;; ;evaluate whole file, and code in red here:
;;
;; (def-logical-directory "DATABASES"   "Honing HD:HH:PROJECTS:DOP-BEAT [master]:database:")
;; (def-logical-directory "CONVERSIONS" "Honing HD:HH:PROJECTS:DOP-BEAT [master]:conversions:")
;;
;; (def-logical-directory "DATABASES"   "Projects:Essen Folksong collection:database:")
;; (def-logical-directory "CONVERSIONS" "Projects:Essen Folksong collection:conversions:")
;;
;; ;(choose-file-dialog)
;;
;; ;test
;; (read-records-from-file :in-file  "DATABASES;Ani:Han1.sm"
;;                         :out-file "Honing HD:Users:honing:Desktop:test.txt")
;;
;; ;conversion Essen to Anthems format
;; (read-records-from-file :in-file  "DATABASES;Essen Folksongs.txt"
;;                         :out-file "CONVERSIONS;Essen in interval notation.txt"
;;                         :ensure-meter nil)
;;
;; ;conversion Ani's databases
;; (read-records-from-file :in-file  "DATABASES;Ani:Han1.sm"
;;                         :out-file "CONVERSIONS;Ani:Han1-new.sm.txt"
;;                         :ensure-meter nil)
;;
;; (read-records-from-file :in-file  "DATABASES;Ani:Han2.sm"
;;                         :out-file "CONVERSIONS;EAni:Han2-new.sm.txt"
;;                         :ensure-meter t)
;;
;; (read-records-from-file :in-file  "DATABASES;Ani:Shanxi.sm"
;;                         :out-file "CONVERSIONS;Ani:Shanxi-new.txt"
;;                         :ensure-meter nil)


;; TODO:
;; meter decoding, now all "Frei" meter are skipped
;; extendibility, keyword for anthems slots
;; module in poco


;**************************************************************************************
;**************************************************************************************
;;; Essen in-between coding

(defmethod read-esac-file ((in-pathname pathname) &key (ensure-meter t))
  (with-open-file (in-stream in-pathname :direction :input)
    (read-melodies in-stream t ensure-meter)))

(defun read-melodies (in out &optional (ensure-meter t))
  (loop with skipped = 0
        with count = 0
        for pos from 0 
        as new = (read-record in (file-position in))
        when (null new)
        do (progn (format t "~%; N=~A, skipped=~A" count skipped) (return))
        else  
        do (let* ((key (select-ID-info new))
                  (object (convert-to-anthems-format 
                           (list key  (convert-melody-to-onset-pattern (select-mel new)))
                           :region (select-slot 'reg new)
                           :title (or (select-slot 'cut new) 
                                   (string (car key)))
                           :ensure-meter ensure-meter
                           )))
             (if object 
               (progn 
                 (incf count)
                 (print object out))
               (progn 
                 (incf skipped))))))


(defun read-records (stream)
  (loop for pos from 0 
        as new = (read-record stream (file-position stream))
        collect new into result
        ;do (print new)
        when (null new)
        do (return result)))

(defun read-record (stream &optional (position 0))
  (loop with found? = nil 
        initially (file-position stream position)
        initially (read-line stream nil :eof) ;read header
        as line =  (read-line stream nil :eof)
        as empty? = (string-equal line "")
        as whole-line = (strip-start-spaces line) then (format nil "~A~A" whole-line (strip-start-spaces line) )
        until (or (and found? empty?) ; lege regel na record
                  (eql line :eof))
        when (not empty?)
        do (setf found? t)
        and when (find #\] whole-line) collect whole-line and do (setf whole-line "")))

(defun select-mel (record)
  (select-slot "MEL" record))


#|
(convert-melody-to-onset-pattern
 (select-mel '("F[BALLADECUT[Der SpielmannssohnAls ich ein kleines Buebeli war,]" "CNR[klein's Buebeli in der Wiege,]"
               "REG[Mitteleuropa, Deutschland / Frankreich, Lothringen]" "SRC[3, S. 1781890 aufgezeichnet]" 
               "KEY[Q0062E 16  F 6/8]" 
"MEL[-5_  1__1_2__5_  3_.23_1__1_  2_.34_5_.43_  1_.23_2__2_  5_.65_4_3_22  1_.-53_2__1_  2_3_5_5_.43_  2__.1__ //] >>" 
"FCT[Ballade, Verfuehrung, Standesunterschied, Strafe, Gnade]" 
"CMT[Kleinere Melodievarianten aus anderer Quelle im gleichen Systemabgedruckt]")))
|#

(defun select-slot (slot record)
  (cond ((string-equal "mel" slot)
         (let* ((string (find slot  record :test #'(lambda(x y ) (string-equal x (subseq y 0 3)))))
                (start (1+ (position #\[ string)) )
                (end (1- (or (position #\/ string)
                             (position #\] string)))))
           (and (subseq string start end))))
        
        (t (let* ((string (find slot  record :test #'(lambda(x y ) (string-equal x (subseq y 0 3))))))
             (and string (let ( (start (1+ (position #\[ string)) )
                                (end (position #\] string)))
                           (subseq string start end)))))))

(defun select-ID-info (record)
  (let* ((raw-line (select-slot  "KEY" record))
         (line (substitute #\space #\. raw-line)) ;bug in database
         (pos 0))
    (loop as (item end) = (multiple-value-list 
                           (read-from-string line nil :eof :start pos))
       repeat 2
       do (setf pos end)
       collect item into result
       ;;do (print (subseq line end (length line)))
       finally (let* ((meters (subseq line end (length line)))
		      (meter (subseq meters 0 (position #\space meters)))
		      ;;(final-meter (when (member meter '("FREI") :test #'equalp) "?"))
		      )
		 (return (append result 
				 (list meter)))))))

                                  
#|
(select-ID-info 
 '("F[BALLADECUT[Graf und Nonne (Die Nonne)\"Mei Schatz, du hast g'sagt, du naehmest mich,]"
 "CNR[Sobald der Sommer kommt;]" "REG[Osteuropa, Ungarn, Banat, Almaskamaras]"
 "SRC[8, S. 288???? aufgezeichnet]" "KEY[Q0155A.16  G 2/4 6/8]"
 "MEL[-5_  1_1_-7_-6_  -6_-5_-5_-7_  1_1_2_-5_  3__0_1_  1_.33_5_  5__4_3_  2_1_-7_6_  6__5_4_  3_3_5_4_  3__0_1_  1_.33_5_  5__4_3_  2_1_-7_6_  6__5_4_  3_3_5_4_  3__0_ //] >>"
 "FCT[Ballade, Standesunterschied, vergebliche Werbung, Liebes - Lied]"
 "CMT[Sehr viele Verzierungen (Vorschlaege) . Schlusston rhythmischkorrigiert.]")
 )
|#

(defun strip-start-spaces (string)
  (if (and (stringp string) (< 0 (length string)))
    (let ((start (position-if-not #'(lambda(x) (eql x #\space)) string)))
      (subseq string start))
    ""))

;(strip-start-spaces "  3_22  1_0-5  3322  1_01  4422  5533  4422  553_4_22  1_0_")

(defun strip-pitch (melody) ;identity
  (with-output-to-string (string)
    (loop  for item across melody
        do (format string "~A" item ))))

(defun convert-melody-to-onset-pattern (melody)
  (with-output-to-string (string)
    (loop with bar? = nil
          with duration = 1
          for item across (ensure-two-or-no-spaces-at-beginning melody)
          as i from 0
          when (and (eql item #\space)                        ; double space -> bar
                    (not bar?)                                ; not already just inserted bar
                    (not (= i 0)))                            ; no insert at begin 
          do (progn (loop repeat (1- duration )
                          do (format string "~A" #\. )
                          finally (setf duration 1))          ;add rests before new bar (never slurred?)
                    (format string "~A" #\| )
                    (setf bar? t))
          else do (progn 
                   ; (print (list duration item))
                    (case item
                      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7)          ; pitches -> onsets
                       (loop repeat (1- duration) 
                             do (format string "~A" #\. )
                             finally (setf duration 1))       ;add rests before new event
                       (format string "~A" 'x ))
                      ((#\_) (setf duration (* duration 2)))  ; double duration -> rest
                      ((#\.) (setf duration (* duration 1.5))); add 50% duration -> rest, for post process
                      ((#\0)
                       (loop repeat (1- duration) 
                             do (format string "~A" #\. )
                             finally (setf duration 1))       ; add rests before new event
                       (format string "~A" #\. ))             ; pause -> rest
                      ((#\+ #\- #\# #\b) nil)                 ; ignore register and alterations
                      (otherwise
                       ;(print item)
                       ;(format string "~A" item )
                       ))
                    (setf bar? nil)
                    )
          )))

(defun ensure-two-or-no-spaces-at-beginning (string)
  (let ((start (position-if #'(lambda (x) (not (eql #\space x))) string)))
    (cond ((zerop start) string) ; usually upbeat
          ((= start 1) (subseq string  1 (length string)))   ; single space also happens? mostly upbeat
          ((= start 2) string)   ; barline at start
          (t (subseq string  (- start 2) (length string)))   ;to many spaces, interpret as barline
          )))

;(ensure-two-or-no-spaces-at-beginning  "0_1_1__  1_5__5")
;(ensure-two-or-no-spaces-at-beginning  "  0_1_1__  1_5__5")
;(ensure-two-or-no-spaces-at-beginning  "    0_1_1__  1_5__5")


;(convert-melody-to-onset-pattern "-5_  1__1_2__5_  3_.23_1__1_")
;(convert-melody-to-onset-pattern "  3_.0_1__.  1_")
;(convert-melody-to-onset-pattern "-5_  1__1_2__5_  3_.23_1__1_  0")
;(convert-melody-to-onset-pattern  "  0_1_1__  1_5__5_  4_3b_2__0_1_1__ _4_  3b_2_  4_3b_2_.3b  4__.^4 ") ; komt ook voor!? rust aan begin
;(convert-melody-to-onset-pattern  "(567)  +1_6_44  6_5_(-513)  5_4_2_  6_5_(567)") ;; komt ook voor!? noten tussen haakjes
;encoding bugs, catch:
;(convert-melody-to-onset-pattern  "    0_1_1__  1_5__5_  ") ; more than two spaces
;(convert-melody-to-onset-pattern  " 34  5_.655  4_0_5+1  ") ; single space

;(convert-melody-to-onset-pattern " 34  5_.655  4_0_5+1  +3_.+2+2+1  5_0_+17  6_.666  5_0_55  4_7_  +1_0_+17  6_.666  5_0_55  4_7_  +1_0_")


;**************************************************************************************
;**************************************************************************************
;;; Essen in-between to anthem coding

(defun convert-to-anthems-format (item &key region title (ensure-meter t))
  (when (or  (not ensure-meter) (ensure-metrical item))
    (let* ((id (caar item))
           (shortest-unit (cadar item))
           (meter (caddar item))
           (string (cadr item))
           (factor 1))
      (list
       (list id meter (list title)

             :QUARTER-NOTE (get-quarter-note-value shortest-unit) 
             :BAR-DURATION (get-bar-duration string factor) 
             :START-AT (get-start-at string factor)

             :region region)
       (X-to-intervals (second item) factor)))))

(defun ensure-metrical (item)
  (let* ((header (first item))
         (string (second item))
         (pos (position #\| string)))
    (cond ((member "FREI" Header :test #'equalp)
           (format t "~%; 'Frei' Meter, Skipped: ~A." (first header))
           nil)
          ((and pos (find #\| string :start (1+ pos)))
           t)
          (t
           (format t "~%; No Meter, Skipped: ~A." (first header))
           nil))))

(defun x-to-intervals (string &optional (factor 1))
  (loop with flag = nil
        with result
        with duration = 1
        for item across string
        do (case item
             ((#\X)    
              (setf flag t) ;upbeat rests are also notated, sometimes
              (setf result (nconc result (list (* duration factor))))
              (setf duration 1))
             ((#\.)    
              (when flag (incf duration)))
             ((#\|)    
              (setf flag t) ;start with barline
              ))
        finally (return (rest (nconc result (list (* duration factor)))))))

;(x-to-intervals "|X.XX|X..X|XXXX|X..")
;(x-to-intervals ".XX|X..X|XXXX|X..")
;(x-to-intervals "X|X..X|XXXX|X..")

(defun get-quarter-note-value (shortest-note)
  ;1 = shortest unit
  (/  shortest-note 4))

;; (defun get-bar-duration (string &optional (factor 1)) ;old
;;   (let* ((first (position #\| string))
;;          (second (position #\| string :start (1+ first))))
;;     (* (- second first ) factor)))

(defun get-bar-duration (string &optional (factor 1))
  (let* ((first (position #\| string))
         (pos (and first (position #\| string :start (1+ first)))))
    (cond (pos 
           (* (- (position #\| string :start (1+ first)) first ) factor))
          (first (* first  factor)) ;only one barline
          (t nil)                    ;no barline
          )))

(defun get-start-at (string &optional (factor 1))
  (let* ((bar (get-bar-duration string))
         (onset (position #\| string)))
    (if bar
      (* (mod (- bar onset) bar) factor)
      nil)))
  
                
;(convert-to-anthems-format *item*)

#|

(x-to-intervals "XX..X.X...")
(get-bar-duration "XX|X.XXXXXX|X...X.X.|X.XXX.XX|X...")
(get-bar-duration "XX|X...|X...X.X.|X.XXX.XX|X...")
(get-start-at "XX|X...|X...X.X.|X.XXX.XX|X...")
(get-start-at "|X...|X...")

(setf *item* 
'((Q0155A 16 "2/4") "XX|X.XXXXXX|X...X.X.|X.XXX.XX|X.....XXX|X.X.XXXX|X...X...|X...X.X.|X.XXX.X.|X...."))

=>

((COSTA-RICA "4/4" ("himno nacional") :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 36) 
(9 3 12 9 3 12 9 3 12 24 9 3 12 9 3 12 9
 3 36 9 3 12 9 3 12 9 3 12 24 9 3 12 9 3 12 9 3 36 9 3 12 6 6 12 9 3 12 
6 6 12 9 3 12 9 3 12 9 3 28 4 4 4 4 4 12 9 3 12 9 3 12 9 3 12 9 3 12 9 3 6 
6 6 6 12 9 3 12 9 3 12 9 3 6 6 6 6 36 9 3 36 9 3 36 9 3 36 9 3 28 4 4 4 4 4 
12 9 3 12 9 3 12 9 3 12 9 3 12 9 3 6 6 6 6 12 9 3 12 9 3 12 12 6 6 6 6 36 9 3
 12 9 3 12 9 3 12 24 9 3 12 9 3 12 9 3 36 9 3 12 9 3 12 9 3 12 24 9 3 12 9 3 12 9 3))

|#
