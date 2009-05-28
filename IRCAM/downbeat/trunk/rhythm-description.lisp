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

(defun read-annotated-rhythm (filename &key (rhythm-directory-root) (sample-rate 172.27d0))
  "Returns an instance of rhythm-description with the onset detection function, times of
   each beat, beats per measure, downbeat times, meter assigned from the annotation."
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
    (format t "start-from ~a start sample ~a~%" start-from (round (* sample-rate start-from)))
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

(defun read-analysed-rhythm (filename &key (rhythm-directory-root) (sample-rate 172.27d0))
  "Returns an instance of rhythm-description with the onset detection function, times of
   each beat, beats per measure, downbeat times, meter assigned from the ircambeat computed values."
  (let* ((odf-filepath (merge-pathnames (make-pathname :directory '(:relative "Analysis")
						       :name filename 
						       :type "odf")
					rhythm-directory-root))
	 (beat-markers-filepath (merge-pathnames 
				 (make-pathname :directory '(:relative "Analysis")
						:name (concatenate 'string filename ".wav.markers")
						:type "xml")
				 rhythm-directory-root))
	 (odf (rhythm-from-ircam-odf odf-filepath :sample-rate sample-rate :weighted nil))
	 (beat-times (read-ircam-marker-times beat-markers-filepath))
	 (start-from (.aref beat-times 0))
	 (odf-subset (subset-of-rhythm odf (list (round (* sample-rate start-from)) t)))
	 (beats-per-measure '(4))) ;; TODO hardwired
    (format t "Starting downbeat finding from ~,3f seconds~%" start-from)
    (make-instance 'rhythm-description
		   :rhythm odf-subset
		   :meter (make-instance 'meter
					 :beat-times beat-times
					 ;; TODO hardwired! should match beats-per-measure * subdivision-per-beat
					 :hierarchy '(2 2 2 2)
					 :beats-per-measure (first beats-per-measure)))))

(defmethod subset-of-rhythm ((rhythm-to-subset rhythm-description) start-from-beat)
  "Subset the rhythm, preserving the beat times"
  (let* ((beat-times (beat-times (meter rhythm-to-subset)))
	 (seconds-to-skip (- (.aref beat-times start-from-beat) (.aref beat-times 0)))
	 (samples-to-skip (round (* seconds-to-skip (sample-rate (rhythm rhythm-to-subset))))))
    (setf (rhythm rhythm-to-subset) (subset-of-rhythm (rhythm rhythm-to-subset) (list samples-to-skip t)))
    (setf (beat-times (meter rhythm-to-subset)) (.arefs beat-times (.iseq start-from-beat (1- (.length beat-times)))))
  rhythm-to-subset))
