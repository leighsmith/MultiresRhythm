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
  ((active :initform nil :initarg :set-active :accessor set-active)
   (scale-list :initform nil :initarg :scale-list)
   (start-sample :accessor start-sample)))

(defun find-scales-of-ridges (ridges current-time-index)
  (loop 
     for scale-index from 0 below (.array-dimension ridges 0)
     when (plusp (.aref ridges scale-index current-time-index))
     collect scale-index))

;; TODO Alternatively concatenate the two lists and sort them then remove all elements with a
;; first order difference less than the tolerance. Problem is distinguishing between the
;; two ridges.
(defun matching-ridges (ridge-scales-1 ridge-scales-2 &key (tolerance 1))
  "Compute the difference in scale number and height between scales of the two ridges.
  Return the list of ridges from ridge-scales-1 that match within a tolerance. 
  Second value returned is the list of ridges from ridge-scales-2 that does not match."
  (let (ridge-1-matches
	ridge-2-matches)
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
    (setf (set-active (find ridge-to-deactivate all-ridges :key #'car)) nil)))

;(defun deactivate-ridge (all-ridges ridge-identifiers)
;  "Moves all nominated ridges to the inactive list"
;  (dolist (ridge-to-deactivate ridge-identifiers)
;    (set-active (find ridge-to-deactivate all-ridges :key #'car) nil))
;  (delete 23 active-ridges :key #'car))

(defun add-new-ridges (all-ridges new-ridge-scales current-time-index)
  (dolist (new-ridge-scale new-ridge-scales)
    (setf all-ridges (cons (make-instance 'ridge
					  :start-time current-time-index 
					  :scale-list (list new-ridge-scale)
					  :set-active t)
			   all-ridges))))

;; Since the ridges are sorted in scale order, simply cons'ing them will match the
;; order. Using cons, ridges are constructed in reverse time order and so we will finally
;; reverse the result.
(defun update-ridges (all-ridges prev-matching-ridges matching-ridges)
  (mapcar #'cons matching-ridges all-ridges))


;; Extract ridges by "hill trekking". That is, hike along the tops of the ridges.  Assumes
;; we have determined the peaks, in the final version the correlated TF profile should be
;; passed in and the ridges determined within this function.
(defun extract-ridges (scale-peaks)
  (let* ((time-frequency-dimensions (.array-dimensions scale-peaks))
;	 (nScale (first time-frequency-dimensions))
;	 (nTime  (second time-frequency-dimensions))
;	 (ridges (make-double-array time-frequency-dimensions))
)
    
    ;; move causally, though there is not really a biological requirement (since the wavelet is non-causal anyway).
    (loop
       for current-time-index from 0 below 30 ; nTime
       with current-ridge-scales
       with all-ridges = '()
       for prev-ridge-scales = '() then current-ridge-scales
       do
	 (setf current-ridge-scales (find-scales-of-ridges scale-peaks current-time-index))
	 (format t "~a~%" current-ridge-scales)

         ;; Compute the difference in scale number and height between current-ridge-scales and n previous
         ;; scales in time. At the moment, n is hardwired as 1.
	 (multiple-value-bind (scales-of-matching-ridges prev-matching-ridges)
	     (matching-ridges current-ridge-scales prev-ridge-scales)

	   (format t "scales-of-matching-ridges ~a~%prev-matching-ridges ~a~%"
		   scales-of-matching-ridges prev-matching-ridges)
	   
	   ;; Any ridges no longer matching are deemed inactive and are retired from the all-ridges.
	   (deactivate-ridges all-ridges (set-difference prev-ridge-scales prev-matching-ridges))
	   
	   ;; All those within the difference threshold become the new state of each
	   ;; active ridge. Update the history of those ridges.
	   (update-ridges all-ridges prev-matching-ridges scales-of-matching-ridges)
	   
	   ;; Those ridges not in scales-of-matching-ridges are new ridges. Add them to
	   ;; the ridge list.
	   (add-new-ridges all-ridges 
			   (set-difference current-ridge-scales scales-of-matching-ridges)
			   current-time-index))
    finally (return all-ridges))))


;; TODO Or should the tempo preferencing influence the selection?
(defun select-longest-tactus (ridge-set)
  "Returns a time sequence of scales"
  ;;(make-fixnum-array 
)

(defun test-ridges (filename)
  (let* ((absolute-pathname (concatenate 'string "/Users/leigh/Research/Data/NewAnalysedRhythms/" filename ".ridges"))
	 (determined-ridges (.load-octave-file absolute-pathname))
	 (ridge-set (extract-ridges determined-ridges)))
    ridge-set))

;; (test-ridges "greensleeves-perform-medium")
