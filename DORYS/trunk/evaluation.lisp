;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for testing functions against a ground truth set.
;;;;
;;;; In nlisp (Matlab-like Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2008
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

;;; Defines where our data resides.
(defparameter *data-directory* (merge-pathnames 
				(make-pathname :directory '(:relative "Research" "Data"))
				(user-homedir-pathname)))

(defparameter *handclap-directory* (merge-pathnames
				    (make-pathname :directory '(:relative "Handclap Examples"))
				    *data-directory*))

(defun evaluate-with-music (evaluation-function &key (music-dataset) (music-name))
  "Applies the evaluation function to the music dataset, by applying to each example of music (as an
  member in the supplied list) in turn, returning those that fail and printing the proportion that pass."
  (let* ((failing-examples (loop
			      for music in music-dataset
			      do (format t "Evaluating ~a~%" (funcall music-name music))
			      when (not (print (funcall evaluation-function music)))
			      collect music))
	 (number-failed (length failing-examples)))
    (format t "~%~a ~d failed, correct ~,2f%~%" 
	    evaluation-function 
	    number-failed
	    (* (- 1.0 (/ (float number-failed) (length music-dataset))) 100.0))
    failing-examples))

;;; For example:
;;; (evaluate-with-music #'evaluate-downbeat-of-anthem :music-dataset *national-anthems* :music-name #'anthem-name)


;;; TODO, need to finalise as general evaluation function
(defun evaluate-accuracy-with-corpus (corpus-name corpus precision-window evaluation-function) 
  "Selects the best evaluations between the annotations and filterings thereof on the
   given corpus. We score on original annotations, half and interpolated beats."
  (loop
     for scores-per-track = (funcall evaluation-function corpus-name corpus precision-window)
     for f-scores = (nlisp::array-to-list (.column scores-per-track 2))
     do
       (format t "~%for ~a mean scores ~a~%" corpus-name (mean-scores scores-per-track))
     collect scores-per-track into scores
     collect f-scores into f-score-comparison
     finally (return (select-from-indices scores (mrr::find-column-maxima (make-narray f-score-comparison))))))

