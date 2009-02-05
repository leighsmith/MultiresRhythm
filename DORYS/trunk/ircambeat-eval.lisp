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
  "Return the precision, recall and f-scores given the times of computed and annotated beats"
  (let* ((time-limited-markers (mrr::prune-outliers ircambeat-marker-times 
						    :lower-limit (- (.aref annotation-times 0) precision-window)
						    :upper-limit (+ (.last annotation-times) precision-window)))
	 (within-precision (.< (vector-distance time-limited-markers annotation-times) precision-window))
	 (matches-per-annotation (mrr::.partial-sum within-precision))
	 (number-correct (.sum (.> matches-per-annotation 0)))
	 (recall (/ number-correct (.length annotation-times)))
	 (precision (/ number-correct (.length time-limited-markers)))
	 (f-score (if (zerop (+ precision recall)) 0.0 (/ (* 2 precision recall) (+ precision recall)))))
    (format t "number of annotations ~d number of markers within annotation range ~d number correct ~d~%"
	    (.length matches-per-annotation) (.length time-limited-markers) (floor number-correct))
    (list precision recall f-score)))

(defun precision-window-of-times (annotation-times relative-precision-window)
  "Retrieve the precision window in seconds from annotated beat times and relative
  precision window as a proportion of the beat period"
  (let* ((average-annotated-beat-period (mean (.diff annotation-times)))
	 (precision-window (* average-annotated-beat-period relative-precision-window)))
    (format t "average annotated beat period ~,3f precision window ~,3f~%" 
	    average-annotated-beat-period precision-window)
    precision-window))

(defun evaluate-ircambeat-computed-tempo (bpm-filepath annotation-filepath precision-window)
  "Returns t if the difference between the tempo of the annotated beat and computed tempo matches with the
  given absolute tolerance in BPM"
  (let* ((annotation-tempo (mrr::tempo-from-times (annotated-beats annotation-filepath)))
	 (ircambeat-computed-tempo (ircambeat-computed-bpm bpm-filepath)))
    (format t "annotation tempo ~d, ircambeat computed tempo ~d~%"
	    annotation-tempo ircambeat-computed-tempo)
    (list (.< (abs (- annotation-tempo ircambeat-computed-tempo)) precision-window) 0.0d0 0.0d0)))

(defun evaluate-ircambeat-computed-tempo-relative (bpm-filepath annotation-filepath precision-window)
  "Returns t if the difference between the tempo of the annotated beat and computed tempo matches with the
  given relative tolerance"
  (let* ((annotation-tempo (mrr::tempo-from-times (annotated-beats annotation-filepath)))
	 (ircambeat-computed-tempo (ircambeat-computed-bpm bpm-filepath)))
    (format t "annotation tempo ~d, ircambeat computed tempo ~d~%"
	    annotation-tempo ircambeat-computed-tempo)
    (list (.< (abs (- annotation-tempo ircambeat-computed-tempo)) (* precision-window annotation-tempo))
	  0.0d0 0.0d0)))

(defun evaluate-ircambeat-tempo (ircambeat-marker-filepath annotation-filepath precision-window)
  "Returns t if the difference between the tempo of annotated and computed beats with the given tolerance"
  (let* ((annotation-tempo (mrr::tempo-from-times (annotated-beats annotation-filepath)))
	 (ircambeat-marker-tempo (mrr::tempo-from-times (read-ircam-marker-times ircambeat-marker-filepath))))
    (format t "annotation tempo ~d, median ircambeat tempo ~d~%"
	    annotation-tempo ircambeat-marker-tempo)
    (list (.< (abs (- annotation-tempo ircambeat-marker-tempo)) precision-window) 0.0d0 0.0d0)))

(defun evaluate-ircambeat (ircambeat-marker-filepath annotation-filepath precision-window)
  "Retrieve the times of nominated beats from an annotation file"
  (let* ((annotation-times (annotated-beats annotation-filepath))
	 (ircambeat-marker-times (read-ircam-marker-times ircambeat-marker-filepath)))
    (evaluate-ircambeat-times ircambeat-marker-times annotation-times precision-window)))

(defun evaluate-ircambeat-relative-precision (ircambeat-marker-filepath annotation-filepath relative-precision-window)
  "Retrieve the times of nominated downbeats from an annotation file"
  (let* ((annotation-times (annotated-beats annotation-filepath))
	 (ircambeat-marker-times (read-ircam-marker-times ircambeat-marker-filepath))
	 (precision-window (precision-window-of-times annotation-times relative-precision-window)))
    (evaluate-ircambeat-times ircambeat-marker-times annotation-times precision-window)))

(defun evaluate-ircambeat-counter-beats (ircambeat-marker-filepath annotation-filepath precision-window)
  "Retrieve the times of nominated downbeats from an annotation file"
  (let* ((annotation-times (annotated-beats annotation-filepath))
	 (ircambeat-marker-times (read-ircam-marker-times ircambeat-marker-filepath)))
    (evaluate-ircambeat-times ircambeat-marker-times (counter-beats annotation-times) precision-window)))

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
     collect (list precision recall f-score) into scores-list
     finally (return (make-narray scores-list))))

(defun evaluate-ircambeat-tempo-on-corpus (corpus-name corpus precision-window 
					   &key (evaluation #'evaluate-ircambeat-computed-tempo))
  "Evaluate the list of tracks in corpus for the given precision-window"
  (loop
     with analysis-directory = (merge-pathnames (make-pathname :directory (list :relative corpus-name)) *rhythm-data-directory*)
     for annotation-filepath in corpus
     for nothing = (format t "~a~%" annotation-filepath)
     for audio-filepath = (make-pathname :name (pathname-name (pathname-name annotation-filepath)) :type "wav")
     for ircambeat-bpm-filepath = (bpm-filepath audio-filepath :analysis-directory analysis-directory)
     for tempo-matches = (funcall evaluation ircambeat-bpm-filepath annotation-filepath precision-window)
     do (format t "precision-window ~,3f tempo-matches ~,3f~%" precision-window tempo-matches)
     collect tempo-matches into f-score-list
     finally (return (make-narray f-score-list))))

(defun summarise-evaluation-ircambeat (corpus-name corpus precision-window &key (evaluation #'evaluate-ircambeat))
  "Returns mean precision, recall and f-score measures as a list, printing the mean values"
  (let ((mean-scores
	 (nlisp::array-to-list
	  (mrr::reduce-dimension (evaluate-ircambeat-on-corpus corpus-name corpus precision-window
							       :evaluation evaluation)
				 #'mean))))
    (format t "Mean ~:{~a ~,3f ~}~%" (mapcar #'list '("precision" "recall" "f-score") mean-scores))
    mean-scores))

(defun evaluate-precision-width (corpus-name corpus)
  "Runs the evaluation on the given corpus over a range of precision widths"
  (loop
     with precision-samples = 6
     for precision-width across (val (.rseq 0.025d0 0.150d0 precision-samples))
     collect (cons precision-width 
		   (summarise-evaluation-ircambeat corpus-name corpus precision-width
						 :evaluation #'evaluate-ircambeat)) into scores
     finally (return (make-narray scores))))

(defun evaluate-relative-precision-width (corpus-name corpus)
  "Runs the evaluation on the given corpus over a range of relative precision widths"
  (loop
     for precision-width across (val (.rseq 0.01d0 0.50d0 11))
     collect (cons precision-width 
		   (summarise-evaluation-ircambeat corpus-name corpus precision-width
						 :evaluation #'evaluate-ircambeat-relative-precision)) into scores
     finally (return (make-narray scores))))

(defun random-select (list maximum-selection)
  "Returns a randomly drawn subset of list to maximum-selection"
  (loop
     while (< (length selection) maximum-selection)
     for new-select = (nth (random (length list)) list)
     unless (find new-select selection)
     collect new-select into selection
     finally (return selection)))

(defun is-hidden-file (pathname)
  "Returns T if the filename of the path has a leading period."
  (char= (aref (pathname-name pathname) 0) #\.))

(defun no-hidden-files (file-list)
  (remove-if #'is-hidden-file file-list))

(defun plot-ircambeat-evaluation (corpus-name corpus)
  "Run the evaluations and plot the results"
  (let ((scores (evaluate-precision-width corpus-name corpus)))
    (window)
    (plot-command "set yrange [ 0.0 : 1.0 ]")
    (plot (list (.column scores 1) (.column scores 2)) (.column scores 0) 
	  :title (format nil "Evaluation of IRCAM-beat on ~a dataset" corpus-name)
	  :legends  '("Mean Precision" "Mean Recall")
	  :styles '("linespoints" "linespoints") 
	  :xlabel "Precision Width (seconds)" :ylabel "Proportion Correct"
	  :reset nil)
    (close-window)))

(defun plot-ircambeat-evaluation-relative (corpus-name corpus)
  "Run the evaluations and plot the results"
  (let ((scores (evaluate-relative-precision-width corpus-name corpus)))
    (window)
    (plot-command "set yrange [ 0.0 : 1.0 ]")
    (plot (list (.column scores 1) (.column scores 2)) (.column scores 0) 
	  :title (format nil "Evaluation of IRCAM-beat on ~a dataset" corpus-name)
	  :legends  '("Mean Precision" "Mean Recall")
	  :styles '("linespoints" "linespoints") 
	  :xlabel "Precision Width (% of beat)" :ylabel "Proportion Correct"
	  :reset nil)
    (close-window)))

(defun plot-ircambeat-evaluation-directory (corpus-name corpus-directory)
  "Run the evaluations and plot the results"
  (plot-ircambeat-evaluation corpus-name (no-hidden-files (cl-fad:list-directory corpus-directory))))

(defun plot-corpus-scores (scores &key (score-index 0))
  (plot (.column scores score-index) nil 
	:reset nil
	:styles "boxes fill solid border 9" 
	:legends (list (nth score-index '("precision" "recall" "f-score")))
	:aspect-ratio 0.66))

(defun rwc-evaluation ()
  "Performs and plots evaluations with a range of precision widths, both absolute and relative"
  (plot-ircambeat-evaluation-directory "RWC" *rwc-annotations-directory*)
  (plot-ircambeat-evaluation-relative "RWC" (no-hidden-files (cl-fad:list-directory *rwc-annotations-directory*))))

(defun quaero-evaluation ()
  "Performs and plots evaluations with a range of precision widths, both absolute and relative"
  (let* ((quaero-subset (random-select (cl-fad:list-directory *quaero-annotations-directory*) 100))
	 (scores (evaluate-ircambeat-on-corpus "Quaero" quaero-subset 0.050))
	 (shortened-scores (.subarray scores '(t (0 49)))))
    (plot-corpus-scores scores :score-index 1)
    (window)
    (nlisp::plot-histogram shortened-scores nil :legends '("precision" "recall" "f-score"))
    ;;(plot-ircambeat-evaluation "Quaero" quaero-subset)
    (plot-ircambeat-evaluation-relative "Quaero" quaero-subset)))

;; (rwc-evaluation)
;; (quaero-evaluation)

#|
(setf x (evaluate-ircambeat #P"/Users/lsmith/Research/Data/IRCAM-Beat/Quaero/Analysis/0005 - Pink Floyd - Dark Side of the Moon - 05 Great Gig in the sky.wav.markers.xml"
		    #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0005 - Pink Floyd - Dark Side of the Moon - 05 Great Gig in the sky.b_p.xml"
		    0.050))

(setf x (evaluate-ircambeat #P"/Users/lsmith/Research/Data/IRCAM-Beat/Quaero/Analysis/0136 - The Beatles - Abbey Road - 10 Sun King.wav.markers.xml"
		    #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0136 - The Beatles - Abbey Road - 10 Sun King.b_q.xml"
		    0.050))

(evaluate-ircambeat-on-corpus "Quaero"
			      (list #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0005 - Pink Floyd - Dark Side of the Moon - 05 Great Gig in the sky.b_p.xml"
				    #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0136 - The Beatles - Abbey Road - 10 Sun King.b_q.xml")
			      0.25d0 
			      :evaluation #'evaluate-ircambeat-relative-precision)

(setf scores (evaluate-ircambeat-on-corpus "Quaero" (no-hidden-files (cl-fad:list-directory *quaero-annotations-directory*)) 0.050))

(setf score-list (evaluate-ircambeat-tempo-on-corpus "RWC" 
			      (no-hidden-files (cl-fad:list-directory *rwc-annotations-directory*))
			      2.0d0 :evaluation #'evaluate-ircambeat-tempo))

(setf score-list (evaluate-ircambeat-tempo-on-corpus "RWC" 
			      (no-hidden-files (cl-fad:list-directory *rwc-annotations-directory*))
			      2.0d0 :evaluation #'evaluate-ircambeat-computed-tempo))

(setf scores (summarise-evaluation-ircambeat "RWC" 
					     (no-hidden-files (cl-fad:list-directory *rwc-annotations-directory*))
					     0.050d0 :evaluation #'evaluate-ircambeat))


(evaluate-ircambeat-tempo 
 #P"/Users/lsmith/Research/Data/IRCAM-Beat/Quaero/Analysis/0136 - The Beatles - Abbey Road - 10 Sun King.wav.markers.xml"
 #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0136 - The Beatles - Abbey Road - 10 Sun King.b_q.xml" 
2.0d0)

(evaluate-ircambeat-tempo 
 #P"/Users/lsmith/Research/Data/IRCAM-Beat/RWC/Analysis/RM-P001.wav.markers.xml"
 #P"/Users/lsmith/Research/Data/IRCAM-Beat/RWC/Annotation/AIST.RWC-MDB-P-2001.BEAT/RM-P001.BEAT.xml"
2.0d0)


(evaluate-ircambeat-computed-tempo 
		    #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0136 - The Beatles - Abbey Road - 10 Sun King.b_q.xml" 
 #P"/Users/lsmith/Research/Data/IRCAM-Beat/Quaero/Analysis/0136 - The Beatles - Abbey Road - 10 Sun King.wav.bpm.xml"
2.0d0)

(setf high-precision (.find (.> (nth 0 score-list) 0.97d0)))
(setf high-recall (.find (.> (nth 1 score-list) 0.97d0)))
(setf high-f-score (.find (.> (nth 2 score-list) 0.97d0)))
(setf low-f-score (.find (.< (nth 2 score-list) 0.2d0)))

;; Below 50%
(.length (.find (.< (nth 2 score-list) 0.5d0)))

|#
