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

;;; A ridge is a zero-based span of time across a time-frequency plane.
;;; A collection of ridges is held in a skeleton instance.
(defclass ridge ()
  ((active :initform nil :initarg :set-active :accessor active)
   ;; Holds a time sequence of scale indices.
   ;; TODO This could become an array using fill pointers to reduce memory foot-print.
   (scale-list :initform nil :initarg :scales :accessor scales)
   ;; TODO an equally long list holding the number of scales between adjacent troughs.
   ;; (widths :initform nil :initarg :widths :accessor widths)
   ;; The time as a sample index that the ridge begins on.
   (start-sample :initarg :start-sample :accessor start-sample)))

(defgeneric most-recent-scale (ridge)
  (:documentation "Returns the most recent scale of the given ridge."))

(defgeneric reverse-time (ridge)
  (:documentation "Returns the ridge, with it's scales list reversed in time."))

(defgeneric last-sample (ridge)
  (:documentation "Returns the sample index of the last sample in the ridge."))

(defgeneric duration-in-samples (ridge)
  (:documentation "Returns the duration in samples of the ridge."))

(defgeneric scale-at-time (ridge time)
  (:documentation "Returns the scale at the given time"))

(defgeneric contains-scale-and-time (ridge scale time)
  (:documentation "Returns t if the given scale and time are part of this ridge."))

(defgeneric contains-scale-p (ridge scale)
  (:documentation "Returns t if the given scale is within this ridge."))

(defgeneric count-contains-scale (ridge scale)
  (:documentation "Returns the number of the given scale in this ridge"))

(defgeneric average-scale (ridge)
  (:documentation "Returns the average scale number of a ridge"))

(defgeneric scale-range (ridge)
  (:documentation "Returns the range of scales of a ridge"))

(defgeneric scales-in-ridge (ridge)
  (:documentation "Returns a list of the scales the ridge spans"))

(defgeneric scales-as-array (ridge)
  (:documentation "Returns the scales list as an nlisp array"))

(defgeneric insert-ridge (ridge time-frequency-plane &key constant-value)
  (:documentation "Insert the given ridge into the time-frequency plane, overwriting existing values with constant-value"))

(defgeneric copy-object (object)
  (:documentation "Creates a deep copy of the object")) 

(defgeneric phase-of (the-ridge phase)
  (:documentation "Returns the phase values at each scale and time in the ridge"))

(defgeneric plot-ridge (the-ridge)
  (:documentation "Plots the given ridge."))

(defgeneric plot-phase (the-ridge phase)
  (:documentation "Plots the phase of the ridge."))

;;; Methods

(defmethod print-object ((ridge-to-print ridge) stream)
  (call-next-method ridge-to-print stream) ;; to print the superclass.
  (let ((the-start-sample (start-sample ridge-to-print)))
    (format stream " from scale ~d @ ~d to scale ~d @ ~d" 
	    (first (scales ridge-to-print)) the-start-sample
	    (first (last (scales ridge-to-print))) (+ the-start-sample (duration-in-samples ridge-to-print) -1))))

(defmethod most-recent-scale ((the-ridge ridge))
  "Returns the most recent scale of the given ridge."
  (first (scales the-ridge)))

(defmethod reverse-time ((the-ridge ridge))
  "Returns the ridge, with it's scales list reversed in time."
  (setf (scales the-ridge) (nreverse (scales the-ridge)))
  the-ridge)

(defmethod duration-in-samples ((the-ridge ridge))
  "Returns the duration in samples of the ridge."
  (length (scales the-ridge)))

(defmethod last-sample ((the-ridge ridge))
  (+ (start-sample the-ridge) (1- (duration-in-samples the-ridge))))

(defmethod scale-at-time ((the-ridge ridge) time)
  "Returns the scale at the given time"
  (let ((scales-index (- time (start-sample the-ridge))))
    (if (and (>= scales-index 0)
	     (< scales-index (duration-in-samples the-ridge)))
	(elt (scales the-ridge) scales-index)
	nil)))

(defmethod contains-scale-and-time ((the-ridge ridge) scale time)
  "Returns t if the given scale and time are part of this ridge."
  (equal scale (scale-at-time the-ridge time)))

(defmethod contains-scale-p ((the-ridge ridge) scale)
  "Returns t if the given scale is within this ridge"
  (find scale (scales the-ridge)))

(defmethod count-contains-scale ((the-ridge ridge) scale)
  "Returns the number of occurances of the given scale in this ridge"
  (count scale (scales the-ridge)))

;;; duration-in-samples and the number of elements in scales-as-array will match, but we
;;; could just do (floor (mean (.* (scales-as-array the-ridge) 1d0)))
(defmethod average-scale ((the-ridge ridge))
  "Returns the mean (average) of the scale numbers"
  (floor (.sum (scales-as-array the-ridge)) (duration-in-samples the-ridge))) 

(defmethod median-scale ((the-ridge ridge))
  "Return the median scale number of the ridge"
  (floor (nlisp::median (.* (scales-as-array the-ridge) 1d0))))

(defmethod scale-range ((the-ridge ridge))
  "Returns the range of the scales a ridge spans"
  (let* ((scales (scales-as-array the-ridge)))
    (- (.max scales) (.min scales))))
    
(defmethod scales-in-ridge ((the-ridge ridge))
  "Returns a list of the scales the ridge spans and the counts of each."
  (let* ((scales (scales the-ridge))
	 (unique-scales (remove-duplicates scales :from-end t)))
    (mapcar (lambda (scale) (list scale (count scale scales))) unique-scales)))

;; (reduce #'+ (scales-in-ridge ridge) :key #'cadr) == (duration-in-samples ridge)

(defmethod scales-and-weights-in-ridge ((the-ridge ridge))
  "Returns an narray of the scales the ridge spans and the relative weights of each."
  (let ((ridge-scales-and-counts (scales-in-ridge the-ridge)))
    (values (make-narray (map 'list #'first ridge-scales-and-counts))
	    (./ (.* (make-narray (map 'list #'second ridge-scales-and-counts)) 1d0)
			       (duration-in-samples the-ridge)))))

;; (defmethod deviation-from-scale ((the-ridge ridge) scale)
;; "Returns the sum-squared deviation of each scale along the ridge from the given scale"


(defmethod .decimate ((the-ridge ridge) reduce-list &key (start-indices '(0 0)))
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
;;; We should assert the ridges are sorted to simplify the search.

;; (defun matching-ridges (ridge-scales-1 ridge-scales-2 &key (tolerance 1))
;;   "Compute the difference in scale number and height between scales of the two ridges.
;;   Return the list of ridges from ridge-scales-1 that match within a tolerance. 
;;   Second value returned is the list of ridges from ridge-scales-2 that matches."
;;   (let (ridge-1-matches	ridge-2-matches)
;;     (dolist (current-ridge ridge-scales-1)
;;       (dolist (comparison-ridge ridge-scales-2)
;; 	(if (<= (abs (- current-ridge comparison-ridge)) tolerance)
;; 	    ;; TODO do set addition, so there are no duplicates?
;; 	    (progn (setf ridge-1-matches (cons current-ridge ridge-1-matches))
;; 		   (setf ridge-2-matches (cons comparison-ridge ridge-2-matches))
;;     (values ridge-1-matches ridge-2-matches)))

;; (defun matching-ridges (ridge-scales-1 ridge-scales-2 &key (tolerance 1))
;;   "Compute the difference in scale number and height between scales of the two ridges.
;;   Return the list of ridges from ridge-scales-1 that match within a tolerance. 
;;   Second value returned is the list of ridges from ridge-scales-2 that matches."
;;   (let (ridge-1-matches	
;; 	ridge-2-matches
;; 	(next-unmatched-ridge 0))
;;     (dolist (current-ridge ridge-scales-1)
;;       ;; limit the loop by tolerance
;;       (loop
;; 	 for comparison-ridge in ridge-scales-2
;; 	 while (<= (nth next-unmatched-ridge ridge-scales-2) (+ current-ridge tolerance))
;; 	 do (if (<= (abs (- current-ridge comparison-ridge)) tolerance)
;; 		(progn (setf ridge-1-matches (cons current-ridge ridge-1-matches))
;; 		       (setf ridge-2-matches (cons comparison-ridge ridge-2-matches))
;; 		       (incf next-unmatched-ridge)
;; 		       (loop-finish)))))
;;     ;; need to increment next-unmatched-ridge 
;;     ;; if (>= (nth next-unmatched-ridge ridge-scales-2) (- current-ridge tolerance))
;;     ;; true.
;;     (setf ridge-scales-2 (rest ridge-scales-2))
;;     (values (reverse ridge-1-matches) (reverse ridge-2-matches))))

(defun matching-ridges (ridge-scales-1 ridge-scales-2 &key (tolerance 1))
  "Compute the difference in scale number and height between scales of the two ridges.
  Return the list of ridges from ridge-scales-1 that match within a tolerance. 
  Second value returned is the list of ridges from ridge-scales-2 that matches."
  (let (ridge-1-matches	
	ridge-2-matches
	(next-unmatched-ridge 0))
    (dolist (current-ridge ridge-scales-1)
      ;; find where the current ridge is located in the ridge-scales-2, remove all earlier
      ;; items from future tests.
      (loop 
	 for comparison-index from next-unmatched-ridge below (length ridge-scales-2)
	 for comparison-ridge = (nth comparison-index ridge-scales-2)
	 while (<= comparison-ridge (+ current-ridge tolerance))
	 do (cond ((<= (abs (- current-ridge comparison-ridge)) tolerance)
		   (setf ridge-1-matches (cons current-ridge ridge-1-matches))
		   (setf ridge-2-matches (cons comparison-ridge ridge-2-matches))
		   (incf comparison-index)
		   (loop-finish)))
	 finally (setf next-unmatched-ridge comparison-index)))
    (values (reverse ridge-1-matches) (reverse ridge-2-matches))))

;;; (matching-ridges '(4 5 6 7 8) '(3 4 5 7 10))
;;; (4 5 6 7) (3 4 5 7)
;;; (matching-ridges '(4 7 9 15) '(3 8 16 23)) ;; diverging into two ridges
;;; (4 7 15) (3 8 16)
;;; (matching-ridges '(4 7 9 15) '(3 8 14 16)) ;; merging into single ridge
;;; (4 7 15) (3 8 14)
;;; (matching-ridges '(9 15) '(3 4 5 8 10 16))
;;; (9 15) (8 16)

;;; We could integrate prune-ridges into deactivate-ridges, but as a separate function,
;;; this keeps the code more modular and the extra efficiency gain (not having to do a
;;; find twice) is pretty small, since we are reducing the list to search over anyway.
(defun prune-ridges (all-ridges &key (minimum-length 4))
  "Ridges under minimum-length are removed from all-ridges."
  (delete-if (lambda (ridge) (and (not (active ridge))
				  (< (duration-in-samples ridge) minimum-length)))
	     all-ridges))

(defun deactivate-ridges (all-ridges ridge-identifiers)
  "Sets all nominated ridges to inactive. Ridges under minimum-length are removed from all-ridges."
  (dolist (ridge-id-to-deactivate ridge-identifiers)
    (setf (active (find ridge-id-to-deactivate all-ridges :key #'most-recent-scale)) nil)))

;; If we want to reduce the size of the ridges to search over when updating the ridges, we
;; should return those deleted and those remaining.
;;  (defun deactivate-ridges (currently-active-ridges scale-ids-to-deactivate)
;;    "Removes all nominated ridges from the active ridge list"
;;     (loop 
;;        for ridge-id-to-deactivate in scale-ids-to-deactivate
;;        collect (find ridge-id-to-deactivate currently-active-ridges :key #'most-recent-scale) into inactive-ridges
;;        do (delete ridge-id-to-deactivate currently-active-ridges :key #'most-recent-scale)
;;        finally (return (values inactive-ridges currently-active-ridges))))

(defun add-new-ridges (new-ridge-scales current-time-index)
  "Creates a list of new ridge instances from the list of scales in new-ridge-scales"
  (loop
     for new-ridge-scale in new-ridge-scales
     collect (make-instance 'ridge
  			    :start-sample current-time-index 
  			    :scales (list new-ridge-scale)
 			    :set-active t)))

;;; TODO can we do it more functionally using substitute? Probably not because what we
;;; want to substitute is a modification of the data already there.
(defun add-ridge-scale (prev-matching-scale new-scale active-ridges)
  "Adds new-scale to the ridge in active-ridges that has the most recent scale being prev-matching-scale."
  (dolist (ridge-candidate active-ridges)
    (if (equal (most-recent-scale ridge-candidate) prev-matching-scale)
	(setf (scales ridge-candidate) (cons new-scale (scales ridge-candidate))))))

(defun update-ridges (active-ridges prev-matching-ridges matching-ridges)
  (mapcar #'(lambda (prev current) (add-ridge-scale prev current active-ridges))
	  prev-matching-ridges matching-ridges))

;;; TODO Assumes we have determined the peaks, in the final version the correlated
;;; time-frequency profile (not just the peaks) should be passed in and the ridges
;;; determined within this function.  TODO Probably keeping the active and inactive ridges
;;; in two separate lists will be more efficient since it removes the requirement to
;;; extract out on each iteration.
(defun extract-ridges (scale-peaks)
  "Extract ridges by ``hill trekking''. That is, hike along the tops of the ridges,
  following the peaks. Returns a list of each ridge in time order (actually reversed)."
  ;; Moves causally, though there is not really a biological requirement (since the wavelet is non-causal anyway).
    (loop
       with time-span = (.array-dimension scale-peaks 1)
       for current-time-index from 0 below time-span
       with current-ridge-scales
       with new-ridges
       with all-ridges = '()
       for prev-ridge-scales = '() then current-ridge-scales
       do
	 (setf current-ridge-scales (find-scales-of-peaks scale-peaks current-time-index))
	 ;; (format t "~a~%" current-ridge-scales)
	 
         ;; Compute the difference in scale number and height between current-ridge-scales and n previous
         ;; scales in time. At the moment, n is hardwired as 1. Returns those matched and
         ;; those matching the previous ridge.
	 (multiple-value-bind (scales-of-matching-ridges prev-matching-ridges)
	     (matching-ridges current-ridge-scales prev-ridge-scales)

	   ;;(format t "scales-of-matching-ridges ~a~%prev-ridge-scales ~a~%prev-matching-ridges ~a~%" 
	   ;; scales-of-matching-ridges prev-ridge-scales prev-matching-ridges)
	   
	   ;; Any ridges no longer matching are deemed inactive and are retired from the all-ridges.
	   ;; TODO (append inactive-ridges (deactivate-ridges active-ridges (set-difference  prev-ridge-scales prev-matching-ridges))
	   (deactivate-ridges all-ridges (set-difference prev-ridge-scales prev-matching-ridges))
	   (setf all-ridges (prune-ridges all-ridges)) ; removes tiny ridges.
	   
	   ;; All those within the difference threshold become the new state of each
	   ;; active ridge. Update the history of those ridges.
	   (update-ridges (remove-if-not #'active all-ridges) prev-matching-ridges scales-of-matching-ridges)
	   
	   ;; Those ridges not in scales-of-matching-ridges are new ridges. Add them to
	   ;; the ridge list.
	   (setf new-ridges (add-new-ridges (set-difference current-ridge-scales scales-of-matching-ridges) current-time-index))
	   ;; (format t "Created new ridges ~a~%" new-ridges)
	   (setf all-ridges (append new-ridges all-ridges)))
       ;; update-ridges will build the scale lists in reverse time order, so we reverse
       ;; them before returning them.
       finally (return (mapcar #'reverse-time all-ridges))))

;; TODO Or should the tempo preferencing influence the selection rather than the ridge height?

(defun make-monotone-ridge (constant-ridge-scale duration-in-samples &key (start 0))
  "Creates a new ridge instance with a single scale spanning the given duration"
  (make-instance 'ridge
		 :start-sample start
		 :scales (make-sequence 'list duration-in-samples :initial-element constant-ridge-scale)
		 :set-active nil))

(defmethod scales-as-array ((the-ridge ridge))
  "Returns the scales list as an nlisp array"
  (make-narray (scales the-ridge)))

(defmethod insert-ridge ((ridge-to-insert ridge) (time-frequency-plane n-double-array)
			 &key (constant-value 1.0d0)) 
  "Insert the given ridge into the time-frequency plane, overwriting existing values with constant-value"
  (let* ((time-in-samples (.array-dimension time-frequency-plane 1))
	 (row-major-indices (.+ (.* (scales-as-array ridge-to-insert) time-in-samples) 
				(.iseq (start-sample ridge-to-insert) 
				       (+ (start-sample ridge-to-insert) (duration-in-samples ridge-to-insert) -1)))))
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

(defmethod phase-of ((the-ridge ridge) phase)
  (loop
     with phases-of-ridge = (make-double-array (duration-in-samples the-ridge))
     for scale across (val (scales-as-array the-ridge))
     for time = 0 then (1+ time)
     do (setf (.aref phases-of-ridge time) (.aref phase scale (+ time (start-sample the-ridge))))
     finally (return phases-of-ridge)))

;;; File I/O routines.

(defmethod save-to-file ((ridge-to-save ridge) (file-stream stream))
  (format file-stream "~a ~a~%" 
	  (start-sample ridge-to-save)
	  (scales ridge-to-save)))

(defmethod read-ridge-from-file ((file-stream stream))
  "Reads a single ridge, returns nil when EOF"
  (let* ((start-sample (read file-stream nil))
	 (scales-list (read file-stream nil)))
    (if start-sample
	(make-instance 'ridge :scales scales-list :start-sample start-sample)
	nil)))

;;; Plotting
(defmethod plot-ridge (ridge-to-plot)
  (let* ((start (start-sample ridge-to-plot))
	 (time (.iseq start (+ start (duration-in-samples ridge-to-plot) -1))))
    (plot (scales-as-array ridge-to-plot) time :aspect-ratio 0.15)))

(defmethod plot-phase ((the-ridge ridge) phase)
  (plot (phase-of the-ridge phase) (.iseq (start-sample the-ridge) (last-sample the-ridge))
	:aspect-ratio 0.66
	:title (format nil "Phase of ~a" the-ridge)))
