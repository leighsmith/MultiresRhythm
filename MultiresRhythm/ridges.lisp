;;;; -*- Lisp -*-
;;;;
;;;; $Id: multires_rhythm.lisp 10 2006-05-17 16:49:06Z leigh $
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

(defclass ridge ()
  ((active :initform nil :initarg :set-active :accessor active)
   (scale-list :initform nil :initarg :scale-list :accessor scale-list)
   (start-sample :initarg :start-sample :accessor start-sample)))

(defmethod most-recent-scale ((the-ridge ridge))
  "Returns the most recent scale of the given ridge."
  (first (scale-list the-ridge)))

(defmethod reverse-time ((the-ridge ridge))
  "Returns the ridge, with it's scale-list reversed in time."
  (setf (scale-list the-ridge) (reverse (scale-list the-ridge))))

(defmethod duration ((the-ridge ridge))
  "Returns the duration in samples of the ridge."
  (length (scale-list the-ridge)))

(defun find-scales-of-ridges (ridges current-time-index)
  (loop 
     for scale-index from 0 below (.array-dimension ridges 0)
     when (plusp (.aref ridges scale-index current-time-index))
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

;(defun deactivate-ridge (ridge-identifiers)
;  "Moves all nominated ridges to the inactive list"
;  (setf inactive-ridges (cons (find 23 active-ridges :key #'car) inactive-ridges))
;  (delete 23 active-ridges :key #'car))

;(defun deactivate-ridge (all-ridges ridge-identifiers)
;  "Moves all nominated ridges to the inactive list"
;  (dolist (ridge-to-deactivate ridge-identifiers)
;    (set-active (find ridge-to-deactivate all-ridges :key #'car) nil))
;  (delete 23 active-ridges :key #'car))

(defun add-new-ridges (new-ridge-scales current-time-index)
  "Creates a list of new ridge instances from the list of scales in new-ridge-scales"
  (loop
     for new-ridge-scale in new-ridge-scales
     collect (make-instance 'ridge
			    :start-sample current-time-index 
			    :scale-list (list new-ridge-scale)
			    :set-active t)))

;;; TODO can we do it more functionally using substitute? Probably not because what we want
;;; to substitute is a modification of the data already there.
(defun add-ridge-scale (prev-matching-scale new-scale active-ridges)
  "Adds new-scale to the ridge in active-ridges that has the most recent scale being prev-matching-scale."
  (dolist (ridge-candidate active-ridges)
    (if (equal (most-recent-scale ridge-candidate) prev-matching-scale)
	(setf (scale-list ridge-candidate) (cons new-scale (scale-list ridge-candidate))))))

(defun update-ridges (active-ridges prev-matching-ridges matching-ridges)
  (mapcar #'(lambda (prev current) (add-ridge-scale prev current active-ridges))
	  prev-matching-ridges matching-ridges))

;;; TODO Assumes we have determined the peaks, in the final version the correlated TF
;;; profile should be passed in and the ridges determined within this function.
;;; TODO perhaps keeping the active and inactive ridges in two separate lists will be more
;;; efficient since it removes the requirement to extract out on each iteration.
(defun extract-ridges (scale-peaks)
  "Extract ridges by ``hill trekking''. That is, hike along the tops of the ridges,
  following the peaks. Returns a list of each ridge in time order (actually reversed)."
  ;; Moves causally, though there is not really a biological requirement (since the wavelet is non-causal anyway).
  (let ((time-span (.array-dimension scale-peaks 1)))
    (loop
       for current-time-index from 0 below 50 ; time-span
       with current-ridge-scales
       with all-ridges = '()
       for prev-ridge-scales = '() then current-ridge-scales
       do
	 (setf current-ridge-scales (find-scales-of-ridges scale-peaks current-time-index))
	 ;; (format t "~a~%" current-ridge-scales)
	 
         ;; Compute the difference in scale number and height between current-ridge-scales and n previous
         ;; scales in time. At the moment, n is hardwired as 1.
	 (multiple-value-bind (scales-of-matching-ridges prev-matching-ridges)
	     (matching-ridges current-ridge-scales prev-ridge-scales)

	   ;; (format t "scales-of-matching-ridges ~a~%prev-matching-ridges ~a~%"
		;;   scales-of-matching-ridges prev-matching-ridges)
	   
	   ;; Any ridges no longer matching are deemed inactive and are retired from the all-ridges.
	   (deactivate-ridges all-ridges (set-difference prev-ridge-scales prev-matching-ridges))
	   
	   ;; All those within the difference threshold become the new state of each
	   ;; active ridge. Update the history of those ridges.
	   (update-ridges (remove-if-not #'active all-ridges) prev-matching-ridges scales-of-matching-ridges)
	   
	   ;; Those ridges not in scales-of-matching-ridges are new ridges. Add them to the ridge list.
	   (setf all-ridges (append (add-new-ridges 
				     (set-difference current-ridge-scales scales-of-matching-ridges)
				     current-time-index) all-ridges)))
       finally (return all-ridges))))


;; TODO Or should the tempo preferencing influence the selection?
;; This could be more efficient but for now, sorting the entire table gives us some good alternatives.
(defun select-longest-tactus (ridge-set)
  "Returns a time sequence of scales"
  (first (sort ridge-set #'>= :key #'duration)))

; (reduce #'max ridge-set :key #'duration)

(defun test-ridges (filename)
  (let* ((absolute-pathname (concatenate 'string "/Users/leigh/Research/Data/NewAnalysedRhythms/" filename ".ridges"))
	 (determined-ridges (.load-octave-file absolute-pathname))
	 (ridge-set (extract-ridges determined-ridges)))
    (select-longest-tactus ridge-set)))

;; (test-ridges "greensleeves-perform-medium")
;; (setf determined-ridges (.load-octave-file "/Users/leigh/Research/Data/NewAnalysedRhythms/greensleeves-perform-medium.ridges"))
;; (setf ridge-set (extract-ridges determined-ridges))
;; (scale-list (select-longest-tactus ridge-set))

