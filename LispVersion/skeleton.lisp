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

(defgeneric ridge-containing-scale-and-time (skeleton scale time-sample)
  (:documentation "Returns the ridge containing the given scale and time element."))

(defgeneric ridges-at-time (skeleton time)
  (:documentation "Returns a list of ridges which have energy in a scale at the given time."))

(defgeneric ridges-containing-scale (skeleton scale)
  (:documentation "Returns a list of ridges which have energy in a given scale."))

(defgeneric ridge-persistency-of (skeleton)
  (:documentation "Returns the normalised scale profile of the ridges found in the skeleton."))

(defgeneric read-skeleton-from (file-stream-or-name)
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
(defmethod longest-tactus-candidates ((skeleton-to-analyse skeleton))
   "Returns a time sequence of scales."
   (let ((searchable-skeleton (copy-seq (ridges skeleton-to-analyse))))
     (sort searchable-skeleton #'>= :key #'duration-in-samples)))
;; TODO get range of durations, determine when the first duration falls a certain amount
;; below the top value.

;; (first (longest-tactus-candidates skeleton))

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

(defmethod make-ridge-plane ((skeleton-to-analyse skeleton))
  "Creates a time-frequency plane from the skeleton, similar to correlated-ridge-scale-peaks, but without variation in value."
  (let ((tf-plane (make-double-array (list (number-of-scales skeleton-to-analyse)
					   (duration-in-samples skeleton-to-analyse)))))
    (dolist (ridge (ridges skeleton-to-analyse) tf-plane)
      (insert-ridge ridge tf-plane :constant-value 1d0))))

;;; TODO perhaps I should be using the correlated ridge scale peaks, using the weighting?
(defmethod ridge-persistency-of ((skeleton-to-analyse skeleton))
  (scale-persistency (make-ridge-plane skeleton-to-analyse)))

(defun persistency-of-scale-in (ridge-plane period-scale)
  "Returns the proportion that the given scale is present in the ridge plane for the given ridge plane"
  (./ (.sum (.or (.row ridge-plane (1- period-scale))
		 (.row ridge-plane period-scale)
		 (.row ridge-plane (1+ period-scale))))
      (.column-count ridge-plane)))

(defun persistency-of-period-in (skeleton period)
  "Returns the proportion of the degree to which the given period duration is present in the skeleton."
  (let* ((period-scale (round (scale-from-period period (voices-per-octave skeleton))))
	 (ridge-plane (make-ridge-plane skeleton)))
    (format t "interval ~a, scaleogram scale = ~a, duration ~a samples~%" 
	    period period-scale (time-support period-scale (voices-per-octave skeleton)))
    ;; TODO persistency-of-scale-in-plane?
    (./ (.sum (.or (.row ridge-plane (1- period-scale))
		   (.row ridge-plane period-scale)
		   (.row ridge-plane (1+ period-scale))))
	(duration-in-samples skeleton))))

(defun skeleton-of-ridge-peaks (analysis-scaleogram ridge-peaks)
  "Returns the skeleton instance given the scaleogram and ridge peaks."
  (make-instance 'skeleton 
		 :ridges (extract-ridges ridge-peaks)
		 :duration (duration-in-samples analysis-scaleogram)
		 :scales (number-of-scales analysis-scaleogram)
		 :voices-per-octave (voices-per-octave analysis-scaleogram)
		 :skip-highest-octaves (skip-highest-octaves analysis-scaleogram)))

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

(defun read-scaleogram-file-header (file-stream)
  ;; Throw away the first comment line for now, one day, we may check it.
  (read-line file-stream)
  (values (read file-stream nil)	; duration
	  (read file-stream nil)	; scale-number
	  (read file-stream nil)	; voices-per-octave
	  (read file-stream nil)))	; skip-highest-octaves

(defmethod read-skeleton-from ((file-stream stream))
  "Read the entire file"
  (multiple-value-bind (duration scale-number voices-per-octave skip-highest-octaves)
      (read-scaleogram-file-header file-stream)	; we read the same header.
    (make-instance 'skeleton
		   :ridges (loop for ridge = (read-ridge-from-file file-stream) while ridge collect ridge)
		   :duration duration
		   :scales scale-number
		   :voices-per-octave voices-per-octave
		   :skip-highest-octaves skip-highest-octaves)))

(defmethod read-skeleton-from ((filename pathname))
  (with-open-file (file-stream filename :direction :input)
    (read-skeleton-from file-stream)))

;;; Test routines.
;; (setf longest-tactus (select-longest-lowest-tactus skeleton))
;; (start-sample longest-tactus)

;; 	 (example-ridge (ridge-containing-scale-and-time skeleton (- 144 (1+ 54)) 209)))
;;     (.save (scales example-ridge)
;;			  "/Users/leigh/Research/Data/NewAnalysedRhythms/greensleeves-example-ridge.tactus"
;;			  :variable-name "tactus" :format :octave)
;;    (format t "start sample ~a~%" (start-sample example-ridge))

;;(test-ridges "greensleeves-perform-medium")
