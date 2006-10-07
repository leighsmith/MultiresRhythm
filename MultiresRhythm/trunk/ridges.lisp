;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;; 
;;;; Multiresolution ridge extraction.
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; See for background:
;;;;   author =  {Leigh M. Smith},
;;;;   title =   {A Multiresolution Time-Frequency Analysis and Interpretation of Musical Rhythm},
;;;;   school =  {Department of Computer Science, University of Western Australia},
;;;;   year =    1999,
;;;;   month =   {June},
;;;;   annote =  {\url{http://www.leighsmith.com/Research/Papers/MultiresRhythm.pdf}}
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

;;; A ridge is a span of time across a time-frequency plane.
(defclass ridge ()
  ((active :initform nil :initarg :set-active :accessor active)
   ;; Holds a time sequence of scale indices.
   ;; TODO This could become an array using fill pointers to reduce memory foot-print.
   (scale-list :initform nil :initarg :scales :accessor scales)
   ;; The time as a sample index that the ridge begins on.
   (start-sample :initarg :start-sample :accessor start-sample)))

(defgeneric most-recent-scale (ridge)
  (:documentation "Returns the most recent scale of the given ridge."))

(defgeneric reverse-time (ridge)
  (:documentation "Returns the ridge, with it's scales list reversed in time."))

(defgeneric duration (ridge)
  (:documentation "Returns the duration in samples of the ridge."))

(defgeneric scale-at-time (ridge time)
  (:documentation "Returns the scale at the given time"))

(defgeneric contains-scale-and-time (ridge scale time)
  (:documentation "Returns t if the given scale and time are part of this ridge."))

(defgeneric average-scale (ridge)
  (:documentation "Returns the average scale number of a ridge"))

(defgeneric tactus-image (ridge ridges &key maximum-colour-value)
  (:documentation "Returns an Imago image object displaying all ridges and the extracted tactus ridge."))

(defgeneric scales-as-array (ridge)
  (:documentation "Returns the scales list as an nlisp array"))

(defgeneric insert-ridge (ridge time-frequency-plane &key constant-value)
  (:documentation "Insert the given ridge into the time-frequency plane, overwriting existing values with constant-value"))

(defgeneric copy-object (object)
  (:documentation "Creates a deep copy of the object")) 

;;; Methods

(defmethod print-object ((ridge-to-print ridge) stream)
  (let ((the-start-sample (start-sample ridge-to-print)))
	(format stream "RIDGE from scale ~d @ ~d to scale ~d @ ~d" 
		(first (scales ridge-to-print)) the-start-sample
		(last (scales ridge-to-print)) (+ the-start-sample (duration ridge-to-print)))))

(defmethod most-recent-scale ((the-ridge ridge))
  "Returns the most recent scale of the given ridge."
  (first (scales the-ridge)))

(defmethod reverse-time ((the-ridge ridge))
  "Returns the ridge, with it's scales list reversed in time."
  (setf (scales the-ridge) (nreverse (scales the-ridge)))
  the-ridge)

(defmethod duration ((the-ridge ridge))
  "Returns the duration in samples of the ridge."
  (length (scales the-ridge)))

(defmethod scale-at-time ((the-ridge ridge) time)
  "Returns the scale at the given time"
  (let ((scales-index (- time (start-sample the-ridge))))
    (if (and (>= scales-index 0)
	     (< scales-index (duration the-ridge)))
	(elt (scales the-ridge) scales-index)
	nil)))

(defmethod contains-scale-and-time ((the-ridge ridge) scale time)
  "Returns t if the given scale and time are part of this ridge."
  (equal scale (scale-at-time the-ridge time)))

(defmethod average-scale ((the-ridge ridge))
  "Returns the mean average of the scale numbers"
  (floor (.sum (scales-as-array the-ridge)) (duration the-ridge)))

(defmethod decimate ((the-ridge ridge) reduce-list &key (start-indices '(0 0)))
  "Returns the ridge instance with it's data decimated using the decimation-parameter-list"
  (declare (ignore start-indices))
  (let* ((reduce-by (second reduce-list))
	 (original-scale-list (scales the-ridge)))
    (setf (scales the-ridge) 
	  (loop
	     for scale-index in original-scale-list by (lambda (x) (nthcdr reduce-by x))
	     collect scale-index))
    (setf (start-sample the-ridge) (floor (start-sample the-ridge) reduce-by)))
  the-ridge)

(defun find-scales-of-peaks (scale-peaks current-time-index)
  "Returns the scale indexes of each non-zero element in the scale peaks matrix."
  (loop 
     for scale-index from 0 below (.array-dimension scale-peaks 0)
     when (plusp (.aref scale-peaks scale-index current-time-index))
     collect scale-index))

;;; TODO Alternatively concatenate the two lists and sort them then remove all elements with a
;;; first order difference less than the tolerance. Problem is distinguishing between the
;;; two ridges.
(defun matching-ridges (ridge-scales-1 ridge-scales-2 &key (tolerance 1))
  "Compute the difference in scale number and height between scales of the two ridges.
  Return the list of ridges from ridge-scales-1 that match within a tolerance. 
  Second value returned is the list of ridges from ridge-scales-2 that does not match."
  (let (ridge-1-matches	ridge-2-matches)
    (dolist (current-ridge ridge-scales-1)
      (dolist (comparison-ridge ridge-scales-2)
	(if (<= (abs (- current-ridge comparison-ridge)) tolerance)
	    (progn (setf ridge-1-matches (append ridge-1-matches (list current-ridge)))
		   (setf ridge-2-matches (append ridge-2-matches (list comparison-ridge)))))))
    (values ridge-1-matches ridge-2-matches)))

;; ;; Perhaps do it with loop and collect into ridge-1-matches
;; (defun matching-ridges (ridge-scales-1 ridge-scales-2 &key (tolerance 1))
;;   "Compute the difference in scale number and height between scales of the two ridges.
;;   Return the list of ridges from ridge-scales-1 that match within a tolerance. 
;;   Second value returned is the list of ridges from ridge-scales-2 that does not match."
;;   (loop
;;      with ridge-1-matches 
;;      and ridge-2-matches
;;      for current-ridge in ridge-scales-1
;;       (dolist (comparison-ridge ridge-scales-2)
;; 	(if (<= (abs (- current-ridge comparison-ridge)) tolerance)
;; 	    (progn (setf ridge-1-matches (append ridge-1-matches (list current-ridge)))
;; 		   (setf ridge-2-matches (append ridge-2-matches (list comparison-ridge)))))))
;;    (values ridge-1-matches (set-difference ridge-scales-2 ridge-2-matches))))

(defun deactivate-ridges (all-ridges ridge-identifiers)
  "Sets all nominated ridges to inactive."
  (dolist (ridge-to-deactivate ridge-identifiers)
    (setf (active (find ridge-to-deactivate all-ridges :key #'most-recent-scale)) nil)))

;; If we want to reduce the size of the ridges to search over when updating the ridges, we
;; should return those deleted and those remaining.
;;  (defun deactivate-ridges (currently-active-ridges scale-ids-to-deactivate)
;;    "Removes all nominated ridges from the active ridge list"
;;     (loop 
;;        for ridge-to-deactivate in scale-ids-to-deactivate
;;        collect (find ridge-to-deactivate currently-active-ridges :key #'most-recent-scale) into inactive-ridges
;;        do (delete ridge-to-deactivate currently-active-ridges :key #'most-recent-scale)
;;        finally (return (values inactive-ridges currently-active-ridges))))

(defun add-new-ridges (new-ridge-scales current-time-index)
  "Creates a list of new ridge instances from the list of scales in new-ridge-scales"
  (loop
     for new-ridge-scale in new-ridge-scales
     collect (make-instance 'ridge
  			    :start-sample current-time-index 
  			    :scales (list new-ridge-scale)
 			    :set-active t)))

;;; TODO can we do it more functionally using substitute? Probably not because what we want
;;; to substitute is a modification of the data already there.
(defun add-ridge-scale (prev-matching-scale new-scale active-ridges)
  "Adds new-scale to the ridge in active-ridges that has the most recent scale being prev-matching-scale."
  (dolist (ridge-candidate active-ridges)
    (if (equal (most-recent-scale ridge-candidate) prev-matching-scale)
	(setf (scales ridge-candidate) (cons new-scale (scales ridge-candidate))))))

(defun update-ridges (active-ridges prev-matching-ridges matching-ridges)
  (mapcar #'(lambda (prev current) (add-ridge-scale prev current active-ridges))
	  prev-matching-ridges matching-ridges))

;;; TODO Assumes we have determined the peaks, in the final version the correlated time-frequency
;;; profile should be passed in and the ridges determined within this function.
;;; TODO Probably keeping the active and inactive ridges in two separate lists will be more
;;; efficient since it removes the requirement to extract out on each iteration.
(defun extract-ridges (scale-peaks)
  "Extract ridges by ``hill trekking''. That is, hike along the tops of the ridges,
  following the peaks. Returns a list of each ridge in time order (actually reversed)."
  ;; Moves causally, though there is not really a biological requirement (since the wavelet is non-causal anyway).
    (loop
       with time-span = (.array-dimension scale-peaks 1)
       for current-time-index from 0 below time-span
       with current-ridge-scales
       with all-ridges = '()
       for prev-ridge-scales = '() then current-ridge-scales
       do
	 (setf current-ridge-scales (find-scales-of-peaks scale-peaks current-time-index))
	 ;; (format t "~a~%" current-ridge-scales)
	 
         ;; Compute the difference in scale number and height between current-ridge-scales and n previous
         ;; scales in time. At the moment, n is hardwired as 1.
	 (multiple-value-bind (scales-of-matching-ridges prev-matching-ridges)
	     (matching-ridges current-ridge-scales prev-ridge-scales)

	   ;; (format t "scales-of-matching-ridges ~a~%prev-matching-ridges ~a~%"
		;;   scales-of-matching-ridges prev-matching-ridges)
	   
	   ;; Any ridges no longer matching are deemed inactive and are retired from the all-ridges.
	   ;; TODO (append inactive-ridges (deactivate-ridges active-ridges (set-difference  prev-ridge-scales prev-matching-ridges))
	   (deactivate-ridges all-ridges (set-difference prev-ridge-scales prev-matching-ridges))
	   
	   ;; All those within the difference threshold become the new state of each
	   ;; active ridge. Update the history of those ridges.
	   (update-ridges (remove-if-not #'active all-ridges) prev-matching-ridges scales-of-matching-ridges)
	   
	   ;; Those ridges not in scales-of-matching-ridges are new ridges. Add them to the ridge list.
	   (setf all-ridges (append (add-new-ridges 
				     (set-difference current-ridge-scales scales-of-matching-ridges)
				     current-time-index) all-ridges)))
       ;; update-ridges will build the scale lists in reverse time order, so we reverse
       ;; them before returning them.
       finally (return (mapcar #'reverse-time all-ridges))))


;; TODO Or should the tempo preferencing influence the selection rather than the ridge height?

;; Sorting the entire table gives us a graded set of good alternatives. 
#|
(defun select-longest-tactus (skeleton)
  "Returns a time sequence of scales."
  (let ((searchable-skeleton skeleton))
    (first (sort searchable-skeleton #'>= :key #'duration))))
|#

(defun select-longest-lowest-tactus (skeleton)
  "Returns the longest duration and lowest scale ridge."
  (let ((max-ridge (make-instance 'ridge)))
    (dolist (ridge skeleton)
      (if (or (> (duration ridge) (duration max-ridge))
	      ;; average-scale returns scale numbers indexed from the highest scales, so 
	      (and (eql (duration ridge) (duration max-ridge))
		   (> (average-scale ridge) (average-scale max-ridge))))
	  (setf max-ridge ridge)))
    max-ridge))

(defun ridge-containing-scale-and-time (scale time-sample skeleton)
  "Returns the ridge containing the given scale and time element."
  (find-if (lambda (ridge) (contains-scale-and-time ridge scale time-sample)) skeleton))

(defun ridges-at-time (skeleton time)
  "Returns a list of ridges which have energy in a scale at the given time"
  (loop
     for ridge-candidate in skeleton
     when (scale-at-time ridge-candidate time)
     collect ridge-candidate))

(defmethod scales-as-array ((the-ridge ridge))
  "Returns the scales list as an nlisp array"
  (make-instance 'n-fixnum-array :ival (make-array (duration the-ridge) :initial-contents
					     (scales the-ridge))))

#|
  (loop
       for row-index in (scales tactus)
       and tactus-index = 0 then (1+ tactus-index)
       with start-column = (start-sample tactus)
       do
	 (setf (.aref plotable-ridges row-index (+ start-column tactus-index)) maximum-colour-value))

  ;; Octave uses column major indices.
  column-major-indices (.+ (.* (.iseq (start-sample ridge-to-insert) 
			     (1- (duration ridge-to-insert))) number-of-scales)
		      (scales-as-array ridge-to-insert))
|#

(defmethod insert-ridge ((ridge-to-insert ridge) time-frequency-plane &key constant-value)
  "Insert the given ridge into the time-frequency plane, overwriting existing values with constant-value"
  (let* ((time-in-samples (.array-dimension time-frequency-plane 1))
	 (row-major-indices (.+ (.* (scales-as-array ridge-to-insert) time-in-samples) 
				(.iseq (start-sample ridge-to-insert) 
				       (+ (start-sample ridge-to-insert) (duration ridge-to-insert) -1)))))
    (map nil 
	 (lambda (row-major-index) (setf (row-major-aref (val time-frequency-plane) row-major-index) constant-value)) 
	 (val row-major-indices))
    time-frequency-plane))

;;; Why oh why doesn't CLOS have a instance copying function as standard? sheesh.
(defmethod copy-object ((ridge-to-copy ridge))
  "Deep copy the given object"
  (make-instance (class-of ridge-to-copy) ; handles subclasses
		 :set-active (active ridge-to-copy)
		 :scales (copy-list (scales ridge-to-copy))
		 :start-sample (start-sample ridge-to-copy)))

;;; Test routines.

(defun test-ridges (filename)
  (let* ((data-directory "/Users/leigh/Research/Data/NewAnalysedRhythms/")
	 (absolute-pathname (concatenate 'string data-directory filename ".ridges"))
	 (determined-ridges (.load-octave-file absolute-pathname))
	 (skeleton (extract-ridges determined-ridges))
	 (longest-tactus (select-longest-lowest-tactus skeleton)))
    (.save-to-octave-file (scales longest-tactus)
			  (concatenate 'string data-directory filename ".tactus")
			  :variable-name "tactus")
    skeleton))

;; (setf skeleton (test-ridges "greensleeves-perform-medium"))
;; (setf determined-ridges (.load-octave-file "/Users/leigh/Research/Data/NewAnalysedRhythms/greensleeves-perform-medium.ridges"))
;; (setf skeleton (extract-ridges determined-ridges))
;; (setf longest-tactus (select-longest-lowest-tactus skeleton))
;; (.save-to-octave-file (scales longest-tactus) "/Users/leigh/Research/Data/NewAnalysedRhythms/greensleeves-perform-medium.tactus" :variable-name "tactus")
;; (start-sample longest-tactus)

;; 	 (example-ridge (ridge-containing-scale-and-time (- 144 (1+ 54)) 209 skeleton)))
;;     (.save-to-octave-file (scales example-ridge)
;;			  "/Users/leigh/Research/Data/NewAnalysedRhythms/greensleeves-example-ridge.tactus"
;;			  :variable-name "tactus")
;;    (format t "start sample ~a~%" (start-sample example-ridge))

;;(test-ridges "greensleeves-perform-medium")
