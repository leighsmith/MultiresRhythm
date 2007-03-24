;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; We define a skeleton to hold all the ridges of a scaleogram together with its dimensions 
;;;; and it's frequency resolution.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
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

(in-package :multires-rhythm)
(use-package :nlisp)

(defclass skeleton ()
  ((ridges-list
    :documentation "Holds the list of ridge objects for a time-frequency representation"
    :initarg :ridges
    :accessor ridges)
   (voices-per-octave 
    :documentation "Frequency resolution as number of scales for a doubling of frequency"
    :initarg :voices-per-octave 
    :accessor voices-per-octave 
    :initform 16)
   (skip-highest-octaves
    :documentation "Number of octaves of the highest frequencies that have not been analysed for efficiency"
    :initarg :skip-highest-octaves
    :initform 0
    :accessor skip-highest-octaves)
   (duration
    :documentation "Time duration in samples"
    :initarg :duration
    :reader duration-in-samples)
   (scales
    :documentation "Frequency in number of scales"
    :initarg :scales
    :reader number-of-scales)))

(defgeneric select-longest-lowest-tactus (skeleton)
  (:documentation "Returns the longest duration and lowest scale ridge."))

(defgeneric ridge-containing-scale-and-time (skeleton scale time-sample)
  (:documentation "Returns the ridge containing the given scale and time element."))

(defgeneric ridges-at-time (skeleton time)
  (:documentation "Returns a list of ridges which have energy in a scale at the given time."))

(defgeneric ridges-containing-scale (skeleton scale)
  (:documentation "Returns a list of ridges which have energy in a given scale."))

(defgeneric read-skeleton-from-file (file-stream-or-name)
  (:documentation "Read the ridge or skeleton contained in the named file or given stream"))

;;; Methods

(defmethod print-object ((skeleton-to-print skeleton) stream)
  (call-next-method skeleton-to-print stream) ;; to print the superclass.
  (format stream " ~d scales x ~d samples, ~d VPO, ~d ridges" 
	  (number-of-scales skeleton-to-print)
	  (duration-in-samples skeleton-to-print)
	  (voices-per-octave skeleton-to-print)
	  (length (ridges skeleton-to-print))))

;; Sorting the entire table gives us a graded set of good alternatives. 
;; (defmethod select-longest-tactus ((skeleton-to-analyse skeleton))
;;   "Returns a time sequence of scales."
;;   (let ((searchable-skeleton (ridges skeleton-to-analyse)))
;;     (first (sort searchable-skeleton #'>= :key #'duration-in-samples))))

(defmethod select-longest-lowest-tactus ((skeleton-to-analyse skeleton))
  "Returns the longest duration and lowest scale ridge."
  (let ((max-ridge (make-instance 'ridge)))
    (dolist (ridge (ridges skeleton-to-analyse))
      (if (or (> (duration-in-samples ridge) (duration-in-samples max-ridge))
	      ;; average-scale returns scale numbers indexed from the highest scales, so 
	      (and (eql (duration-in-samples ridge) (duration-in-samples max-ridge))
		   (> (average-scale ridge) (average-scale max-ridge))))
	  (setf max-ridge ridge)))
    max-ridge))

(defmethod ridge-containing-scale-and-time ((skeleton-to-analyse skeleton) scale time-sample)
  "Returns the ridge containing the given scale and time element."
  (find-if (lambda (ridge) (contains-scale-and-time ridge scale time-sample)) (ridges skeleton-to-analyse)))

(defmethod ridges-at-time ((skeleton-to-analyse skeleton) time)
  "Returns a list of ridges which have energy in a scale at the given time"
  (loop
     for ridge-candidate in (ridges skeleton-to-analyse)
     when (scale-at-time ridge-candidate time)
     collect ridge-candidate))

(defmethod ridges-containing-scale ((skeleton-to-analyse skeleton) scale)
  "Returns a list of ridges which have energy in a scale"
  (loop
     for ridge-candidate in (ridges skeleton-to-analyse)
     when (contains-scale-p ridge-candidate scale)
     collect ridge-candidate))

;;; File I/O

(defmethod save-to-file ((skeleton-to-write skeleton) (filename pathname))
  "Writes out all ridges in the skeleton"
  (with-open-file (file-stream filename :direction :output :if-exists :supersede)
    (format file-stream ";; Format: duration, scales, voices per octave, skip highest octaves, list of ridges~%")
    (format file-stream "~a ~a ~a ~a~%"
	    (duration-in-samples skeleton-to-write)
	    (number-of-scales skeleton-to-write)
	    (voices-per-octave skeleton-to-write)
	    (skip-highest-octaves skeleton-to-write))
    (dolist (ridge (ridges skeleton-to-write))
      (save-to-file ridge file-stream))))

(defmethod read-skeleton-from-file ((file-stream stream))
  "Read the entire file"
  (multiple-value-bind (duration scale-number voices-per-octave skip-highest-octaves)
      (read-scaleogram-file-header file-stream)	; we read the same header.
    (make-instance 'skeleton
		   :ridges (loop for ridge = (read-ridge-from-file file-stream) while ridge collect ridge)
		   :duration duration
		   :scales scale-number
		   :voices-per-octave voices-per-octave
		   :skip-highest-octaves skip-highest-octaves)))
    
(defmethod read-skeleton-from-file ((filename pathname))
  (with-open-file (file-stream filename :direction :input)
    (read-skeleton-from-file file-stream)))

;;; Test routines.
;; (setf longest-tactus (select-longest-lowest-tactus skeleton))
;; (start-sample longest-tactus)

;; 	 (example-ridge (ridge-containing-scale-and-time skeleton (- 144 (1+ 54)) 209)))
;;     (.save-to-octave-file (scales example-ridge)
;;			  "/Users/leigh/Research/Data/NewAnalysedRhythms/greensleeves-example-ridge.tactus"
;;			  :variable-name "tactus")
;;    (format t "start sample ~a~%" (start-sample example-ridge))

;;(test-ridges "greensleeves-perform-medium")
