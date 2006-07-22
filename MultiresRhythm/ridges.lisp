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

(defgeneric contains-scale-and-time (ridge scale time)
  (:documentation "Returns t if the given scale and time are part of this ridge."))

(defgeneric tactus-image (ridge ridges &key maximum-colour-value)
  (:documentation "Returns an Imago image object displaying all ridges and the extracted tactus ridge."))

(defgeneric decimate-ridge (ridge reduce-list)
  (:documentation "Returns the ridge instance with it's data decimated using the reduce-list"))

;;; Methods
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

(defmethod contains-scale-and-time ((the-ridge ridge) scale time)
  "Returns t if the given scale and time are part of this ridge."
  (let ((scales-index (- time (start-sample the-ridge))))
    (and (plusp scales-index)
	 (< scales-index (duration the-ridge))
	 (equal scale (elt (scales the-ridge) scales-index)))))

(defmethod decimate-ridge ((the-ridge ridge) reduce-list)
  "Returns the ridge instance with it's data decimated using the decimation-parameter-list"
  (let* ((reduce-by (second reduce-list))
	 (original-scale-list (scales the-ridge)))
    (setf (scales the-ridge) 
	  (loop
	     for scale-index in original-scale-list by (lambda (x) (nthcdr reduce-by x))
	     collect scale-index))
    (setf (start-sample the-ridge) (/ (start-sample the-ridge) reduce-by)))
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
;; TODO Unfortunately it
;; does it destructively and leaves ridge-set half it's size?
;; (defun select-longest-tactus (ridge-set)
;;   "Returns a time sequence of scales."
;;   (first (sort ridge-set #'>= :key #'duration)))

;; TODO Can this be implemented more simply?
(defun select-longest-tactus (ridge-set)
  "Returns the longest duration ridge."
  (let ((max-ridge (make-instance 'ridge)))
    (dolist (ridge ridge-set)
      (if (< (duration max-ridge) (duration ridge))
	  (setf max-ridge ridge)))
    max-ridge))

(defun ridge-containing-scale-and-time (scale time-sample ridge-set)
  "Returns the ridge containing the given scale and time element."
  (find-if (lambda (ridge) (contains-scale-and-time ridge scale time-sample)) ridge-set))


(defun test-ridges (filename)
  (let* ((data-directory "/Users/leigh/Research/Data/NewAnalysedRhythms/")
	 (absolute-pathname (concatenate 'string data-directory filename ".ridges"))
	 (determined-ridges (.load-octave-file absolute-pathname))
	 (ridge-set (extract-ridges determined-ridges))
	 (longest-tactus (select-longest-tactus ridge-set)))
    (.save-to-octave-file (scales longest-tactus)
			  (concatenate 'string data-directory filename ".tactus")
			  :variable-name "tactus")
    ridge-set))

;; (setf ridge-set (test-ridges "greensleeves-perform-medium"))
;; (setf determined-ridges (.load-octave-file "/Users/leigh/Research/Data/NewAnalysedRhythms/greensleeves-perform-medium.ridges"))
;; (setf ridge-set (extract-ridges determined-ridges))
;; (setf longest-tactus (select-longest-tactus ridge-set))
;; (.save-to-octave-file (scales longest-tactus) "/Users/leigh/Research/Data/NewAnalysedRhythms/greensleeves-perform-medium.tactus" :variable-name "tactus")
;; (start-sample longest-tactus)

;; 	 (example-ridge (ridge-containing-scale-and-time (- 144 (1+ 54)) 209 ridge-set)))
;;     (.save-to-octave-file (scales example-ridge)
;;			  "/Users/leigh/Research/Data/NewAnalysedRhythms/greensleeves-example-ridge.tactus"
;;			  :variable-name "tactus")
;;    (format t "start sample ~a~%" (start-sample example-ridge))

;;(test-ridges "greensleeves-perform-medium")
