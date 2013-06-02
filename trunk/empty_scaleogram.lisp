;;;; -*- Lisp -*-
;;;;
;;;; $Id: cwt.lisp 239 2007-03-21 16:47:48Z leigh $
;;;;
;;;; We define a scaleogram that doesn't actually hold anything except the dimensions of
;;;; the magnitude and phase and it's resolution. This allows us to use it for dimension
;;;; calculations without needing to hold, load or generate the actual data.
;;;;
;;;; However perhaps what we really need is skeleton to be a full object which holds these dimensions?
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

(defclass empty-scaleogram (scaleogram)
  ((duration
    :documentation "Fakes out having an actual matrix, just holding and returning the dimension"
    :initarg :duration
    :reader duration-in-samples)
   (scales
    :documentation "Fakes out having an actual matrix, just holding and returning the dimension"
    :initarg :scales
    :reader number-of-scales)))

(defgeneric read-empty-scaleogram-from-file (filename)
  (:documentation "Reads just the dimensions of the scaleogram contained in the named file."))

(defmethod read-empty-scaleogram-from-file ((filename pathname))
  "Reads just the dimensions of the scaleogram contained in the named file. 
This is *much* quicker than reading the two matrices."
  (with-open-file (file-stream filename :direction :input)
    (multiple-value-bind (duration scale-number voices-per-octave skip-highest-octaves)
	(read-scaleogram-file-header file-stream)
      (make-instance 'empty-scaleogram 
		     :voices-per-octave voices-per-octave
		     :skip-highest-octaves skip-highest-octaves
		     :duration duration
		     :scales scale-number))))

	      ;; We use read-empty-scaleogram-from-file for speed. If we need to use the
	      ;; data itself, we can use read-scaleogram-from-file.
;;	      (rhythm-scaleogram (read-empty-scaleogram-from-file (make-pathname :directory anthem-path 
;; 									   :name (name anthem-rhythm)
;; 									   :type "scaleogram")))
