;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for testing clapping accompaniment.
;;;;
;;;; In nlisp (Matlab-like Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006
;;;;
;;;; See 
;;;;   author =  {Leigh M. Smith},
;;;;   title =   {A Multiresolution Time-Frequency Analysis and Interpretation of Musical Rhythm},
;;;;   school =  {Department of Computer Science, University of Western Australia},
;;;;   year =    1999,
;;;;   month =   {June},
;;;;   annote =  {\url{http://www.leighsmith.com/Research/Papers/MultiresRhythm.pdf}}
;;;;

(in-package :dorys)
(use-package :nlisp)
(use-package :multires-rhythm)

(defun accompany-essen-rhythms (essen-scores)
  "Reads in the files from the score descriptions, writes out the notes and the handclaps to a scorefile"
  (loop 
     for (filename m meter d description) in essen-scores
     for filepath = (make-pathname :directory "/Volumes/iDisk/Research/Data/Temperley/essen-perf" 
				   :name filename 
				   :type "notes")
     for melisma-part = (part-of-melisma-file filepath)
     for accompaniment-part = (multires-rhythm::accompaniment-part-to melisma-part filename)
     for filename-to-write = (make-pathname :directory "/Volumes/iDisk/Research/Data/Handclap Examples/Temperley Essen Performed"
					    :name (concatenate 'string filename "_handclap") 
					    :type "score")
     do
       (format t "Writing rhythm ~a and clapping accompaniment to ~a~%" filename filename-to-write)
       (save-scorefile filename-to-write
		       (list melisma-part accompaniment-part)
		       :instrument "midi"
		       :midi-channels '(1 10)
		       :description (format nil "Handclapping to ~a" filename))))

;;; (setf performed-rhythms (essen-of-meter "3/4"))
;;; (setf performed-rhythms (essen-of-meter "4/4"))
;;; (setf performed-rhythms (list (essen-named "romani09")))
;;; (accompany-essen-rhythms performed-rhythms)


;; (setf france01 (essen-rhythm "france01"))
;; (multires-rhythm::find-downbeat-new france01 100)

(defun evaluate-downbeat-of-essen (essen-score)
  "Evaluates the downbeat finder against the performed Essen data"
  (let ((filename (first essen-score))
	(anacrusis (seventh essen-score)))
    (= (multires-rhythm::find-downbeat-short (essen-rhythm filename)) anacrusis)))

;;; (setf bad-essen-downbeats (evaluate-with-music #'evaluate-downbeat-of-essen :music-dataset *essen-perf-meters* :music-name #'first))
;;; #<FUNCTION EVALUATE-DOWNBEAT-OF-ESSEN> 17 failed, correct 73.02%
;;; Look at the problems all at once:
;;; (evaluate-with-music #'evaluate-downbeat-of-essen :music-dataset bad-essen-downbeats :music-name #'first)

