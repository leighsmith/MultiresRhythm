;;; -*- Lisp -*-
;;;
;;; $Id$
;;;
;;; Temperley's performances of Essen folk song melodies, as described in:
;;; David Temperley "Music and Probability", MIT Press 2007

(in-package :dorys)

;;; Derived from Kern format files of the Essen folk song collection taken from HumDrum, sitting in the local
;;; directory 

(defparameter *essen-perform-directory* (merge-pathnames 
					 (make-pathname :directory '(:relative "Temperley" "essen-perf"))
					 *data-directory*))

(defparameter *essen-perf-meters* '(

("czech01" :meter "2/4" :meter-description "simple duple" :anacrusis 0 :anacrusis-intervals "")
("czech10" :meter "2/4" :meter-description "simple duple" :anacrusis 0 :anacrusis-intervals "")
("czech13" :meter "3/4" :meter-description "simple triple" :anacrusis 1 :anacrusis-intervals "4 ")
("czech35" :meter "2/4" :meter-description "simple duple" :anacrusis 0 :anacrusis-intervals "")
("danmark3" :meter "3/4" :meter-description "simple triple" :anacrusis 2 :anacrusis-intervals "8. 16 ")
("danmark6" :meter "2/4" :meter-description "simple duple" :anacrusis 1 :anacrusis-intervals "16 ")
("danmark7" :meter "4/4" :meter-description "simple quadruple" :anacrusis 1 :anacrusis-intervals "4 ")
("danmark9" :meter "2/4" :meter-description "simple duple" :anacrusis 0 :anacrusis-intervals "")
("deut0206" :meter "4/4" :meter-description "simple quadruple" :anacrusis 0 :anacrusis-intervals "")
("deut0214" :meter "3/4" :meter-description "simple triple" :anacrusis 0 :anacrusis-intervals "")
("deut0717" :meter "6/8" :meter-description "compound duple" :anacrusis 1 :anacrusis-intervals "8 ")
("deut1334" :meter "4/4" :meter-description "simple quadruple" :anacrusis 2 :anacrusis-intervals "4. 8 ")
("elsass01" :meter "2/4" :meter-description "simple duple" :anacrusis 0 :anacrusis-intervals "")
("elsass45" :meter "6/8" :meter-description "compound duple" :anacrusis 1 :anacrusis-intervals "8 ")
("elsass54" :meter "2/4" :meter-description "simple duple" :anacrusis 3 :anacrusis-intervals "8 8 8 ")
("elsass71" :meter "6/8" :meter-description "compound duple" :anacrusis 0 :anacrusis-intervals "")
("france01" :meter "4/4" :meter-description "simple quadruple" :anacrusis 3 :anacrusis-intervals "8 8 8 ")
("france03" :meter "2/4" :meter-description "simple duple" :anacrusis 3 :anacrusis-intervals "8 16 16 ")
("france08" :meter "6/8" :meter-description "compound duple" :anacrusis 1 :anacrusis-intervals "8 ")
("france13" :meter "2/4" :meter-description "simple duple" :anacrusis 1 :anacrusis-intervals "8 ")
("italia02" :meter "3/4" :meter-description "simple triple" :anacrusis 3 :anacrusis-intervals "8 8 8 ")
("italia04" :meter "2/4" :meter-description "simple duple" :anacrusis 2 :anacrusis-intervals "16 16 ")
("italia08" :meter "6/8" :meter-description "compound duple" :anacrusis 1 :anacrusis-intervals "8 ")
("jugos003" :meter "3/2" :meter-description "simple triple" :anacrusis 4 :anacrusis-intervals "4 8 8 4 ")
("jugos051" :meter "4/4" :meter-description "simple quadruple" :anacrusis 1 :anacrusis-intervals "4 ")
("jugos117" :meter "6/8" :meter-description "compound duple" :anacrusis 1 :anacrusis-intervals "8 ")
("lothr017" :meter "3/4" :meter-description "simple triple" :anacrusis 2 :anacrusis-intervals "8 8 ")
("lothr043" :meter "2/4" :meter-description "simple duple" :anacrusis 1 :anacrusis-intervals "8 ")
("luxemb03" :meter "4/4" :meter-description "simple quadruple" :anacrusis 1 :anacrusis-intervals "4 ")
("luxemb04" :meter "2/4" :meter-description "simple duple" :anacrusis 2 :anacrusis-intervals "16 16 ")
("magyar21" :meter "3/4" :meter-description "simple triple" :anacrusis 3 :anacrusis-intervals "8 8 8 ")
("magyar26" :meter "3/4" :meter-description "simple triple" :anacrusis 0 :anacrusis-intervals "")
("magyar29" :meter "4/4" :meter-description "simple quadruple" :anacrusis 1 :anacrusis-intervals "4 ")
("neder040" :meter "6/4" :meter-description "compound duple" :anacrusis 3 :anacrusis-intervals "4 4 4 ")
("oestr035" :meter "3/4" :meter-description "simple triple" :anacrusis 2 :anacrusis-intervals "8 8 ")
("oestr081" :meter "3/4" :meter-description "simple triple" :anacrusis 2 :anacrusis-intervals "8 8 ")
("oestr102" :meter "6/8" :meter-description "compound duple" :anacrusis 1 :anacrusis-intervals "8 ")
("polska01" :meter "6/8" :meter-description "compound duple" :anacrusis 1 :anacrusis-intervals "8 ")
("polska11" :meter "6/8" :meter-description "compound duple" :anacrusis 1 :anacrusis-intervals "8 ")
("polska22" :meter "4/4" :meter-description "simple quadruple" :anacrusis 1 :anacrusis-intervals "4 ")
("romani01" :meter "3/4" :meter-description "simple triple" :anacrusis 2 :anacrusis-intervals "8 8 ")
("romani09" :meter "4/4" :meter-description "simple quadruple" :anacrusis 1 :anacrusis-intervals "4 ")
("romani13" :meter "3/4" :meter-description "simple triple" :anacrusis 2 :anacrusis-intervals "8 8 ")
("romani22" :meter "3/8" :meter-description "simple triple" :anacrusis 1 :anacrusis-intervals "8 ")
("suisse28" :meter "4/4" :meter-description "simple quadruple" :anacrusis 1 :anacrusis-intervals "4 ")
("suisse35" :meter "2/4" :meter-description "simple duple" :anacrusis 1 :anacrusis-intervals "8 ")
("suisse57" :meter "6/8" :meter-description "compound duple" :anacrusis 1 :anacrusis-intervals "8 ")
("suisse67" :meter "3/8" :meter-description "simple triple" :anacrusis 1 :anacrusis-intervals "8 ")
("sverig01" :meter "6/8" :meter-description "compound duple" :anacrusis 1 :anacrusis-intervals "8 ")
("sverig06" :meter "4/4" :meter-description "simple quadruple" :anacrusis 0 :anacrusis-intervals "")
("sverig07" :meter "4/4" :meter-description "simple quadruple" :anacrusis 1 :anacrusis-intervals "8 ")
("sverig09" :meter "6/8" :meter-description "compound duple" :anacrusis 1 :anacrusis-intervals "8 ")
("tirol03" :meter "4/4" :meter-description "simple quadruple" :anacrusis 2 :anacrusis-intervals "8. 16 ")
("tirol06" :meter "3/4" :meter-description "simple triple" :anacrusis 2 :anacrusis-intervals "8 8 ")
("tirol12" :meter "3/4" :meter-description "simple triple" :anacrusis 0 :anacrusis-intervals "")
("tirol14" :meter "2/4" :meter-description "simple duple" :anacrusis 1 :anacrusis-intervals "8 ")
("ukrain06" :meter "6/8" :meter-description "compound duple" :anacrusis 1 :anacrusis-intervals "8 ")
("ukrain11" :meter "4/4" :meter-description "simple quadruple" :anacrusis 1 :anacrusis-intervals "8 ")
("ukrain12" :meter "4/4" :meter-description "simple quadruple" :anacrusis 1 :anacrusis-intervals "8 ")
("ussr21" :meter "3/8" :meter-description "simple triple" :anacrusis 1 :anacrusis-intervals "8 ")
("ussr25" :meter "4/4" :meter-description "simple quadruple" :anacrusis 0 :anacrusis-intervals "")
("ussr26" :meter "2/4" :meter-description "simple duple" :anacrusis 0 :anacrusis-intervals "")
("ussr36" :meter "6/8" :meter-description "compound duple" :anacrusis 1 :anacrusis-intervals "8 ")))

(defun essen-of-meter (meter) 
  (remove-if-not (lambda (x) (equal x meter)) *essen-perf-meters* :key #'third))

(defun essen-named (name)
  (find name *essen-perf-meters* :test #'equal :key #'first))

;; Read Temperley's Melisma "notefiles". These are described as:
;;  in "notelist" format, "Note [ontime] [offtime] [pitch]", where ontime and offtime
;; are in milliseconds and pitch is an integer, middle C = 60.
;; There are however, other lines in the file, which need pruning.
(defun part-of-melisma-file (filepath)
  "Reads the data file, creates a note-list"
  (with-open-file (input-stream filepath)
    (loop
       for note-type = (read input-stream nil)
       while note-type
       if (string-equal note-type "Note")
       collect
	 (let ((on-time (read input-stream))
	       (off-time (read input-stream))
	       (midi-note (read input-stream)))
	   ;; (format t "~a, ~a, ~a, ~a~%" note-type on-time off-time midi-note)
	   (list (/ on-time 1000.0d0) (/ (- off-time on-time) 1000.0d0) 1.0 midi-note))
       else
       do (read-line input-stream nil))))

(defun rhythm-of-melisma-file (filepath)
  "Returns a rhythm object, from the melisma file"
  (let ((melisma-note-list (part-of-melisma-file filepath)))
    (multires-rhythm::rhythm-of-part (pathname-name filepath) melisma-note-list)))

(defun essen-rhythm (name)
  "Returns a rhythm given the name of the EsAC data"
  (rhythm-of-melisma-file (make-pathname :defaults *essen-perform-directory*
					 :name name 
					 :type "notes")))

(defun load-essen-rhythms (essen-scores)
  "Reads in the files from the score descriptions passed in, returns a list of rhythm instances."
  (loop 
     for (filename m meter d description) in essen-scores
     collect (essen-rhythm filename)))

;;; (setf rs (load-essen-rhythms (subseq *essen-perf-meters* 0 5)))
;;; (setf performed-44 (essen-of-meter "4/4"))
;;; (setf performed-34 (essen-of-meter "3/4"))
;;; (setf performed-68 (essen-of-meter "6/8"))
;;; (setf expectancy-set (mapcar #'last-expectations (load-essen-rhythms performed-34)))
;;; (setf all-expectations (reduce #'append expectancy-set))
;;; (cl-musickit:play-timed-notes (cl-musickit::note-list-of-part (part-of-melisma-file
;;; (make-pathname *essen-perform-directory* "deut0214.notes"))))

