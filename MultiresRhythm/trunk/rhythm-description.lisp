;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Class defining an annotated rhythm and functions for reading and processing instances.
;;;;
;;;; In nlisp (Matlab-like Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <leigh.smith@ircam.fr> 
;;;;
;;;; Copyright (c) 2009
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

(defclass rhythm-description () ; should this be a mixin of rhythm and meter, or just encapsulation?
  ;; Rhythm without meter:
  ((rhythm :initarg :rhythm :accessor rhythm) ; of salience-trace-rhythm
   (meter :initarg :meter :accessor meter))   ; of meter 
  (:documentation "Holds a rhythm, annotated with metrical set detection function, beat times and metrical structure"))

(defun read-annotated-rhythm (filename &key 
			      (rhythm-directory-root)
			      (sample-rate 172.27d0))
  "Returns an instance of annotated rhythm with the onset detection function, times of
   each beat, beats per measure, downbeat times, meter assigned."
  (let* ((odf-filepath (merge-pathnames (make-pathname :directory '(:relative "Analysis")
						       :name filename 
						       :type "odf")
					rhythm-directory-root))
	 (annotation-filepath (merge-pathnames (make-pathname :directory '(:relative "Annotation")
							      :name filename 
							      :type "b.xml")
					       rhythm-directory-root))
	 (odf (rhythm-from-ircam-odf odf-filepath :sample-rate sample-rate :weighted nil))
	 (annotated-beat-times (read-annotated-beats annotation-filepath))
	 (start-from (.aref annotated-beat-times 0))
	 (odf-subset (subset-of-rhythm odf (list (round (* sample-rate start-from)) t)))
	 (beats-per-measure (mapcar #'first (read-ircam-annotation-timesignatures annotation-filepath))))
    (make-instance 'rhythm-description
		   :rhythm odf-subset
		   :meter (make-instance 'meter
					 :beat-times annotated-beat-times
					 ;; TODO hardwired! should match beats-per-measure * subdivision-per-beat
					 :hierarchy '(2 2 2 2)
					 :beats-per-measure (first beats-per-measure)))))

;; (setf u2 (read-annotated-rhythm "0001 - U2 - The Joshua Tree - With or without you" :rhythm-directory-root dorys::*quaero-selection-directory*))

;; (setf eminem (read-annotated-rhythm "0011 - Eminem - The Eminem Show - Cleanin Out my Closet" :rhythm-directory-root dorys::*quaero-selection-directory*))


;; (setf wes (read-annotated-rhythm "0300 - Wes - Alane" :rhythm-directory-root *quaero-selection-directory*))

