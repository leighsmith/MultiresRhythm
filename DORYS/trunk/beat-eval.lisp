;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for evaluating the performance of a beat induction system.
;;;;
;;;; In nlisp (Matlab-like Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <Leigh.Smith@ircam.fr> 
;;;;
;;;; Copyright (c) 2009
;;;;

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :dorys)
(use-package :nlisp)
(use-package :multires-rhythm)

(defun vector-distance (vec1 vec2)
  "Compute a distance matrix between two vectors, returns a matrix with rows of vec1, columns vec2"
  (let ((distances (nlisp::narray-of-type vec1 (list (.length vec1) (.length vec2)))))
    (dotimes (i (.length vec1) distances)
      (dotimes (j (.length vec2))
	(setf (.aref distances i j) (abs (- (.aref vec1 i) (.aref vec2 j))))))))

(defun counter-beats (beat-times)
  "Returns counter-beats, that is, times of beats phase shifted by pi radians"
  (.+ (mrr::.butlast beat-times) (./ (.diff beat-times) 2.0d0)))

(defun interpolate-beats-new (beat-times division)
  "Returns a new set of beat times with division-1 number of beats interpolated between them"
  (let* ((tatum (./ (.diff beat-times) division))
	 (butlast-beat (mrr::.butlast beat-times)) ;; we interpolate upto the last beat.
	 (new-length (1+ (* (1- (.length beat-times)) division)))
	 (new (make-double-array new-length))
	 (indices (.* (.iseq 0 (- (.length beat-times) 2)) division)))
    (setf (.aref new (1- new-length)) (.last beat-times)) 
    (dotimes (tatum-index division new)
      (setf (.arefs new (.+ indices tatum-index)) (.+ butlast-beat (.* tatum tatum-index))))))

(defun interpolate-beats (beat-times)
  "Returns a new set of beat times with counter-beats interpolated between"
  (mrr::.sort (.concatenate beat-times (counter-beats beat-times)) :ordering #'<))

(defun half-beats (beat-times)
  "Returns every second beat of the given array of times"
  (downbeats-of-times beat-times 0 2))

(defun counter-half-beats (beat-times)
  "Returns every second beat of the given array of times, skipping the first beat"
  (downbeats-of-times beat-times 1 2))

(defun plot-min-distance (title computed-beat-times annotation-times &optional (precision-window 0.100d0))
  (let* ((time-limited-markers (mrr::prune-outliers computed-beat-times 
						    :lower-limit (- (.aref annotation-times 0) precision-window)
						    :upper-limit (+ (.last annotation-times) precision-window)))
	 (minimum-distance (mrr::reduce-dimension (vector-distance time-limited-markers annotation-times) 
						  #'.min)))
    (window)
    (plot minimum-distance nil 
	  :aspect-ratio 0.66 
	  :title title 
	  :xlabel "annotated beats"
	  :ylabel "minimum distance (seconds)")
    (close-window)))

;; Recall = number correct / number annotated => determines deletions
;; Precision = number correct / number computed => determines additions
;; Single combined value.
(defun evaluate-beat-times (computed-beat-times annotation-times precision-window)
  "Return the precision, recall and f-scores given the times of computed and annotated beats.
   Precision-window can be scalar or a vector of annotation-times length."
  (let* ((precision-windows (if (.vectorp precision-window)
				(.transpose (mrr::expand-dimension precision-window (.length computed-beat-times)))
				precision-window))
	 (within-precision (.< (vector-distance computed-beat-times annotation-times) precision-windows))
	 (matches-per-annotation (mrr::.partial-sum within-precision))
	 (matching-annotations-per-marker (mrr::.partial-sum (.transpose within-precision)))
	 (duplicated-matches (.sum (.> matching-annotations-per-marker 1)))
	 ;; remove any annotations that match a marker more than once
	 (number-correct (- (.sum (.> matches-per-annotation 0)) duplicated-matches))
	 (recall (/ number-correct (.length annotation-times)))
	 (precision (/ number-correct (.length computed-beat-times)))
	 (f-score (if (zerop (+ precision recall)) 0.0 (/ (* 2 precision recall) (+ precision recall)))))
    (format t "number of annotations ~d number of markers within annotation range ~d number correct ~d~%"
	    (.length matches-per-annotation) (.length computed-beat-times) (floor number-correct))
    (format t "number of annotations matching more than one marker ~d~%" (floor duplicated-matches))
    ;; (format t "first 10 annotations ~a~%" (.subseq annotation-times 0 9))
    ;; (format t "last 10 annotations ~a~%" (.subseq annotation-times (- (.length annotation-times) 10)))
    (list precision recall f-score)))

(defun precision-window-of-times (annotation-times relative-precision-window)
  "Retrieve the precision window in seconds from annotated beat times and relative
  precision window as a proportion of the beat period"
  ;; Add an extra beat period at the start to match the number of annotations.
  (let* ((annotated-beat-periods (.concatenate (.diff (.subseq annotation-times 0 2))
					       (.diff annotation-times)))
	 (precision-windows (.* annotated-beat-periods relative-precision-window)))
    (format t "annotated beat periods ~a~%absolute precision windows ~a~%" 
	    (.subseq annotated-beat-periods 0 5) (.subseq precision-windows 0 5))
    precision-windows))

;;; TODO perhaps change this into print-object and define an information-retrieval class?
(defmacro print-prf (scores)
  "Returns a formatted description of the precision, recall and f-score measures for a list of those values"
  `(format nil "~{precision ~,3f recall ~,3f f-score ~,3f~}" ,scores))
;; TODO or use:
;; (format nil "~:{~a ~,3f ~}~%" (mapcar #'list '("precision" "recall" "f-score") mean-scores))

(defun mean-scores (scores-per-track)
  "Returns mean precision, recall and f-score measures as a list, printing the mean values, from the track scores"
  (let* ((mean-scores (nlisp::array-to-list (mrr::reduce-dimension scores-per-track #'mean))))
    (format t "Mean ~a~%" (print-prf mean-scores))
    mean-scores))

