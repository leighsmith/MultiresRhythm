;;;; -*- Lisp -*-
;;;;
;;;; $Id: ircam.lisp 5413 2009-01-16 16:26:26Z leigh $
;;;;
;;;; Functions for evaluating the performance of IRCAMbeat.
;;;;
;;;; In nlisp (Matlab-like Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <Leigh.Smith@ircam.fr> 
;;;;
;;;; Copyright (c) 2008
;;;;

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :dorys)
(use-package :nlisp)
(use-package :multires-rhythm)

(defparameter *rwc-annotations-directory* (merge-pathnames 
					   (make-pathname :directory '(:relative "RWC" "Annotation" "AIST.RWC-MDB-P-2001.BEAT"))
					   *rhythm-data-directory*))

(defun vector-distance (vec1 vec2)
  "Compute a distance matrix between two vectors"
  (let ((distances (nlisp::narray-of-type vec1 (list (.length vec1) (.length vec2)))))
    (dotimes (i (.length vec1) distances)
      (dotimes (j (.length vec2))
	(setf (.aref distances i j) (abs (- (.aref vec1 i) (.aref vec2 j))))))))

(defun counter-beats (beat-times)
  "Returns counter-beats, that is, times of beats phase shifted by pi radians"
  (.+ (.subarray beat-times (list 0 (list 0 (- (.length beat-times) 2)))) (./ (.diff beat-times) 2.0d0)))

(defun evaluate-ircambeat-times (ircambeat-marker-times annotation-times precision-window)
  "Retrieve the times of nominated downbeats from an annotation file"
  (let* ((time-limited-markers (mrr::prune-outliers ircambeat-marker-times 
						    :lower-limit (- (.aref annotation-times 0) precision-window)
						    :upper-limit (+ (.last annotation-times) precision-window)))
	 (within-precision (.< (vector-distance time-limited-markers annotation-times) precision-window))
	 (matches-per-annotation (mrr::.partial-sum within-precision))
	 (number-correct (.sum (.> matches-per-annotation 0)))
	 (recall (/ number-correct (.length annotation-times)))
	 (precision (/ number-correct (.length time-limited-markers)))
	 (f-score (if (zerop (+ precision recall)) 0.0 (/ (* 2 precision recall) (+ precision recall)))))
    (format t "number of annotations ~d number of markers with annotation range ~d number correct ~d~%"
	    (.length matches-per-annotation) (.length time-limited-markers) (floor number-correct))
    (list precision recall f-score)))

(defun evaluate-ircambeat (ircambeat-marker-filepath annotation-filepath precision-window)
  "Retrieve the times of nominated downbeats from an annotation file"
  (let* ((annotation-times (mrr::read-ircam-annotation annotation-filepath))
	 (ircambeat-marker-times (read-ircam-marker-times ircambeat-marker-filepath)))
    (evaluate-ircambeat-times ircambeat-marker-times annotation-times precision-window)))

(defun evaluate-ircambeat-counter-beats (ircambeat-marker-filepath annotation-filepath precision-window)
  "Retrieve the times of nominated downbeats from an annotation file"
  (let* ((annotation-times (mrr::read-ircam-annotation annotation-filepath))
	 (ircambeat-marker-times (read-ircam-marker-times ircambeat-marker-filepath)))
    (evaluate-ircambeat-times ircambeat-marker-times (counter-beats annotation-times) precision-window)))

(defun print-stats (name proportional-scores)
  (let ((mean-score (mean proportional-scores)))
    (format t "Mean ~a ~,3f Stddev ~a ~,3f~%" 
	    name mean-score name (stddev proportional-scores))
    mean-score))
  
(defun evaluate-ircambeat-on-corpus (corpus-name corpus precision-window &key (evaluation #'evaluate-ircambeat))
  "Evaluate the list of tracks in corpus for the given precision-window"
  (loop
     with analysis-directory = (merge-pathnames (make-pathname :directory (list :relative corpus-name)) *rhythm-data-directory*)
     for annotation-filepath in corpus
     for nothing = (format t "~a~%" annotation-filepath)
     for audio-filepath = (make-pathname :name (pathname-name (pathname-name annotation-filepath)) :type "wav")
     for ircambeat-marker-filepath = (beat-marker-filepath audio-filepath :analysis-directory analysis-directory)
     for (precision recall f-score) = (funcall evaluation ircambeat-marker-filepath annotation-filepath precision-window)
     do (format t "precision-window ~,3f recall ~,3f precision ~,3f f-score ~,3f~%"
		precision-window recall precision f-score)
     collect recall into recall-list
     collect precision into precision-list
     collect f-score into f-score-list
     finally (return (list (make-narray precision-list) (make-narray recall-list) (make-narray f-score-list)))))

(defun summary-evaluation-ircambeat (corpus-name corpus precision-window)
  "Returns mean precision, recall and f-score measures as a list"
  (map 'list #'print-stats 
       '("precision" "recall" "f-score") 
       (evaluate-ircambeat-on-corpus corpus-name corpus precision-window :evaluation #'evaluate-ircambeat)))

(defun evaluate-precision-width (corpus-name corpus)
  "Runs the evaluation on the given corpus over a range of precision widths"
  (loop
     for precision-width across (val (.rseq 0.025d0 0.150d0 6))
     collect (cons precision-width 
		   (summary-evaluation-ircambeat corpus-name corpus precision-width)) into scores
     finally (return (make-narray scores))))

(defun is-hidden-file (pathname)
  "Returns T if the filename of the path has a leading period."
  (char= (aref (pathname-name pathname) 0) #\.))

(defun no-hidden-files (file-list)
  (remove-if #'is-hidden-file file-list))

(defun plot-ircambeat-evaluation (corpus-name corpus-directory)
  "Run the evaluations and plot the results"
  (let ((scores (evaluate-precision-width corpus-name (no-hidden-files (cl-fad:list-directory corpus-directory)))))
    (window)
    (plot-command "set yrange [ 0.0 : 1.0 ]")
    (plot (list (.column scores 1) (.column scores 2)) (.column scores 0) 
	  :title (format nil "Evaluation of IRCAM-beat on ~a dataset" corpus-name)
	  :legends  '("Precision" "Recall")
	  :styles '("linespoints" "linespoints") 
	  :xlabel "Precision Width" :ylabel "Proportion Correct"
	  :reset nil)
    (close-window)))

;; (plot-ircambeat-evaluation "RWC" *rwc-annotations-directory*)
;; (plot-ircambeat-evaluation "Quaero" *quaero-annotations-directory*)


#|
(setf x (evaluate-ircambeat #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/Quaero/Analysis/0005 - Pink Floyd - Dark Side of the Moon - 05 Great Gig in the sky.wav.markers.xml"
		    #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0005 - Pink Floyd - Dark Side of the Moon - 05 Great Gig in the sky.b_p.xml"
		    0.050))

(setf x (evaluate-ircambeat #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/Quaero/Analysis/0136 - The Beatles - Abbey Road - 10 Sun King.wav.markers.xml"
		    #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0136 - The Beatles - Abbey Road - 10 Sun King.b_q.xml"
		    0.050))

(evaluate-ircambeat-on-corpus "Quaero"
			      (list #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0005 - Pink Floyd - Dark Side of the Moon - 05 Great Gig in the sky.b_p.xml"
				    #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0136 - The Beatles - Abbey Road - 10 Sun King.b_q.xml")
			      0.050)


(evaluate-ircambeat-on-corpus "Quaero" (no-hidden-files (cl-fad:list-directory )) 0.050)

(setf score-list (evaluate-ircambeat-on-corpus "RWC" (no-hidden-files (cl-fad:list-directory *rwc-annotations-directory*)) 0.050))

(defun plot-histogram (list-of-columns)
  (plot-command "set style data histogram")
  (plot-command "set style histogram cluster gap 1")
  (plot-command "set style fill solid border -1")
  (plot-command "set boxwidth 0.9")
  (plot score-list nil 
	:reset nil
	:styles (nlisp::ntimes "boxes fill solid border 9" (length score-list)) 
	:legends '("precision" "recall" "f-score")
	:aspect-ratio 0.66))
  
(setf high-recall (.find (.> (nth 1 score-list) 0.97d0)))
|#
