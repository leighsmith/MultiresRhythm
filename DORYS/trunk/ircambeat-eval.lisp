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
;;;; Copyright (c) 2009
;;;;

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :dorys)
(use-package :nlisp)
(use-package :multires-rhythm)

(defparameter *rwc-annotations-directory* (merge-pathnames 
					   (make-pathname :directory '(:relative "RWC" "Annotation" "AIST.RWC-MDB-P-2001.BEAT"))
					   *rhythm-data-directory*))

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

;; Recall = number correct / number annotated => determines deletions
;; Precision = number correct / number computed => determines additions
(defun evaluate-ircambeat-times (ircambeat-marker-times annotation-times precision-window)
  "Return the precision, recall and f-scores given the times of computed and annotated beats"
  (let* ((time-limited-markers (mrr::prune-outliers ircambeat-marker-times 
						    :lower-limit (- (.aref annotation-times 0) precision-window)
						    :upper-limit (+ (.last annotation-times) precision-window)))
	 (within-precision (.< (vector-distance time-limited-markers annotation-times) precision-window))
	 (matches-per-annotation (mrr::.partial-sum within-precision))
	 (matching-annotations-per-marker (mrr::.partial-sum (.transpose within-precision)))
	 (duplicated-matches (.sum (.> matching-annotations-per-marker 1)))
	 ;; remove any annotations that match a marker more than once
	 (number-correct (- (.sum (.> matches-per-annotation 0)) duplicated-matches))
	 (recall (/ number-correct (.length annotation-times)))
	 (precision (/ number-correct (.length time-limited-markers)))
	 (f-score (if (zerop (+ precision recall)) 0.0 (/ (* 2 precision recall) (+ precision recall)))))
    (format t "number of annotations ~d number of markers within annotation range ~d number correct ~d~%"
	    (.length matches-per-annotation) (.length time-limited-markers) (floor number-correct))
    (format t "number of annotations matching more than one marker ~d~%" (floor duplicated-matches))
    ;; (format t "first 10 annotations ~a~%" (.subseq annotation-times 0 9))
    ;; (format t "last 10 annotations ~a~%" (.subseq annotation-times (- (.length annotation-times) 10)))
    (list precision recall f-score)))

(defun precision-window-of-times (annotation-times relative-precision-window)
  "Retrieve the precision window in seconds from annotated beat times and relative
  precision window as a proportion of the beat period"
  (let* ((average-annotated-beat-period (mean (.diff annotation-times)))
	 (precision-window (* average-annotated-beat-period relative-precision-window)))
    (format t "average annotated beat period ~,3f absolute precision window ~,3f~%" 
	    average-annotated-beat-period precision-window)
    precision-window))

(defun evaluate-ircambeat (ircambeat-marker-filepath annotation-filepath precision-window &key annotation-filter)
  "Retrieve the times of nominated beats from an annotation file & return the match scores"
  (let* ((annotation-times (annotated-beats annotation-filepath))
	 (filtered-annotation-times (if annotation-filter (funcall annotation-filter annotation-times)
					annotation-times))
	 (ircambeat-marker-times (read-ircam-marker-times ircambeat-marker-filepath)))
    (evaluate-ircambeat-times ircambeat-marker-times filtered-annotation-times precision-window)))

(defun evaluate-ircambeat-on-corpus (corpus-name corpus precision-window &key annotation-filter relative-precision)
  "Evaluate the list of tracks in corpus for the given precision-window"
  (loop
     with analysis-directory = (merge-pathnames (make-pathname :directory (list :relative corpus-name)) *rhythm-data-directory*)
     for annotation-filepath in corpus
     for nothing = (format t "~a~%" annotation-filepath)
     for audio-filepath = (make-pathname :name (pathname-name (pathname-name annotation-filepath)) :type "wav")
     for ircambeat-marker-filepath = (beat-marker-filepath audio-filepath :analysis-directory analysis-directory)
     for annotation-times = (annotated-beats annotation-filepath)
     for filtered-annotation-times = (if annotation-filter (funcall annotation-filter annotation-times) annotation-times)
     for ircambeat-marker-times = (read-ircam-marker-times ircambeat-marker-filepath)
     for absolute-precision-window = (if relative-precision 
					 (precision-window-of-times annotation-times precision-window) 
					 precision-window)
     for (precision recall f-score) = (evaluate-ircambeat-times ircambeat-marker-times filtered-annotation-times absolute-precision-window)
     do (format t "precision-window ~,3f recall ~,3f precision ~,3f f-score ~,3f~%" precision-window recall precision f-score)
     collect (list precision recall f-score) into scores-list
     finally (return (make-narray scores-list))))

(defun evaluate-ircambeat-computed-tempo (bpm-filepath annotation-filepath precision-window)
  "Returns t if the difference between the tempo of the annotated beat and computed tempo matches with the
  given absolute tolerance in BPM"
  (let* ((annotation-tempo (mrr::tempo-from-times (annotated-beats annotation-filepath)))
	 (ircambeat-computed-tempo (ircambeat-computed-bpm bpm-filepath)))
    (format t "annotation tempo ~d, ircambeat computed tempo ~d~%"
	    annotation-tempo ircambeat-computed-tempo)
    (.< (abs (- annotation-tempo ircambeat-computed-tempo)) precision-window)))

(defun evaluate-ircambeat-computed-tempo-relative (bpm-filepath annotation-filepath precision-window)
  "Returns t if the difference between the tempo of the annotated beat and computed tempo matches with the
  given relative tolerance"
  (let* ((annotation-tempo (mrr::tempo-from-times (annotated-beats annotation-filepath)))
	 (comparison-tempi (.* annotation-tempo (make-narray '(1.0d0 0.5d0 2.0d0))))
	 (ircambeat-computed-tempo (ircambeat-computed-bpm bpm-filepath))
	 (matches (.< (.abs (.- comparison-tempi ircambeat-computed-tempo))
		      (.* precision-window comparison-tempi))))
    (format t "annotation tempo ~,3f, ircambeat computed tempo ~,3f absolute precision ~,3f~%"
	    annotation-tempo ircambeat-computed-tempo (* precision-window annotation-tempo))
    (nlisp::array-to-list matches)))

(defun evaluate-ircambeat-tempo (ircambeat-marker-filepath annotation-filepath precision-window)
  "Returns t if the difference between the tempo of annotated and computed beats with the given tolerance"
  (let* ((annotation-tempo (mrr::tempo-from-times (annotated-beats annotation-filepath)))
	 (ircambeat-marker-tempo (mrr::tempo-from-times (read-ircam-marker-times ircambeat-marker-filepath))))
    (format t "annotation tempo ~d, median ircambeat tempo ~d~%"
	    annotation-tempo ircambeat-marker-tempo)
    (list (.< (abs (- annotation-tempo ircambeat-marker-tempo)) precision-window) 0.0d0 0.0d0)))

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
     do (format t "precision-window ~,3f tempo-matches ~a~%" precision-window tempo-matches)
     collect tempo-matches into f-score-list
     finally (return (make-narray f-score-list))))

(defun mean-scores (scores-per-track)
  "Returns mean precision, recall and f-score measures as a list, printing the mean values, from the track scores"
  (let* ((mean-scores (nlisp::array-to-list (mrr::reduce-dimension scores-per-track #'mean))))
    (format t "Mean ~:{~a ~,3f ~}~%" (mapcar #'list '("precision" "recall" "f-score") mean-scores))
    mean-scores))

(defun evaluate-accuracy-2 (corpus-name corpus precision-window &key relative-precision) 
  "Combines the evaluations on the given corpus against the annotations and filterings thereof"
  (loop
     ;; we assume three scores (precision, recall, f-score) are returned.
     with mean-scores = (make-double-array (list (length corpus) 3))
     for annotation-filter in (list nil #'interpolate-beats #'half-beats) 
     for scores-per-track = (evaluate-ircambeat-on-corpus corpus-name 
							  corpus 
							  precision-window
							  :relative-precision relative-precision
							  :annotation-filter annotation-filter)
     do
       (format t "~%for ~a ~a mean scores ~a~%" corpus-name annotation-filter (mean-scores scores-per-track))
       (setf mean-scores (.+ mean-scores scores-per-track))
     count t into filter-number
     finally (return (./ mean-scores filter-number))))
  
(defun summarise-evaluation-ircambeat (corpus-name corpus precision-window &key annotation-filter relative-precision)
  "Returns mean precision, recall and f-score measures as a list, printing the mean values"
  (let* ((scores-per-track (evaluate-ircambeat-on-corpus corpus-name 
							 corpus 
							 precision-window
							 :relative-precision relative-precision
							 :annotation-filter annotation-filter)))
    (mean-scores scores-per-track)))

(defun evaluate-precision-window (corpus-name corpus &key annotation-filter)
  "Runs the evaluation on the given corpus over a range of precision widths"
  (loop
     with precision-samples = 6
     for precision-window across (val (.rseq 0.025d0 0.150d0 precision-samples))
     for scores-per-track = (evaluate-ircambeat-on-corpus corpus-name 
							  corpus 
							  precision-window
							  :relative-precision nil
							  :annotation-filter annotation-filter)
     collect (cons precision-window (mean-scores scores-per-track)) into scores
     finally (return (make-narray scores))))

(defun evaluate-relative-precision-window (corpus-name corpus &key annotation-filter)
  "Runs the evaluation on the given corpus over a range of relative precision widths"
  (loop
     for precision-window across (val (.rseq 0.01d0 0.50d0 6))
     for scores-per-track = (evaluate-ircambeat-on-corpus corpus-name 
							  corpus 
							  precision-window
							  :relative-precision t
							  :annotation-filter annotation-filter)
     collect (cons precision-window (mean-scores scores-per-track)) into scores
     finally (return (make-narray scores))))

(defun evaluate-relative-accuracy-2 (corpus-name corpus)
  "Runs the evaluation on the given corpus over a range of relative precision widths"
  (loop
     for precision-window across (val (.rseq 0.01d0 0.50d0 6))
     for scores-per-track = (evaluate-accuracy-2 corpus-name 
						 corpus 
						 precision-window
						 :relative-precision t)
     collect (cons precision-window (mean-scores scores-per-track)) into scores
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

(defun plot-ircambeat-evaluation (corpus-name scores units)
  "Plot the results of the evaluations"
  (window)
  (plot-command "set yrange [ 0.0 : 1.0 ]")
  (plot (list (.column scores 1) (.column scores 2)) (.column scores 0) 
	:title (format nil "Evaluation of IRCAM-beat on ~a dataset" corpus-name)
	:legends  '("Mean Precision" "Mean Recall")
	:styles '("linespoints" "linespoints") 
	:xlabel (format nil "Precision Window (~a)" units)
	:ylabel "Proportion Correct"
	:reset nil)
  (close-window))

(defun plot-corpus-scores (scores &key (score-index 0))
  (plot (.column scores score-index) nil 
	:reset nil
	:styles "boxes fill solid border 9" 
	:legends (list (nth score-index '("precision" "recall" "f-score")))
	:aspect-ratio 0.66))

(defun plot-evaluation (corpus-name corpus-directory)
  "Performs and plots evaluations with a range of precision widths, both absolute and relative"
  (loop
     with corpus = (no-hidden-files (cl-fad:list-directory corpus-directory)) 
     for annotation-filter in (list nil #'counter-beats #'interpolate-beats #'half-beats) 
     ;; for scores = (evaluate-precision-window corpus-name corpus :annotation-filter annotation-filter)
     for relative-scores = (evaluate-relative-precision-window corpus-name corpus :annotation-filter annotation-filter)
     for evaluation-description = (format nil "~a ~a" corpus-name annotation-filter) 
     do (plot-ircambeat-evaluation evaluation-description relative-scores "% of beat")))
     ;; do (plot-ircambeat-evaluation evaluation-description scores "seconds")))

(defun plot-evaluation-2 (corpus-name corpus-directory)
  (let* ((corpus (no-hidden-files (cl-fad:list-directory corpus-directory)))
	 (relative-scores (evaluate-relative-accuracy-2 corpus-name corpus)))
    (plot-ircambeat-evaluation (format nil "~a accuracy 2" corpus-name) relative-scores "% of beat")))

(defun plot-tempo-evaluation (corpus-name corpus-directory)
  (let* ((corpus (no-hidden-files (cl-fad:list-directory corpus-directory))) 
	 (scores (evaluate-ircambeat-tempo-on-corpus corpus-name corpus 0.04d0
						     :evaluation #'evaluate-ircambeat-computed-tempo-relative))
	 ;; calculating the mean on binary valued scores computes the % result.
	 (mean-scores (mrr::reduce-dimension scores #'mean)))
    (format t "mean scores for tempo evaluation ~a~%" mean-scores)))
    ;; (plot-ircambeat-evaluation corpus-name scores "% of annotated BPM")

(defun quaero-evaluation ()
  "Performs and plots evaluations with a range of precision widths, both absolute and relative"
  (let* ((quaero-subset (random-select (cl-fad:list-directory *quaero-annotations-directory*) 100))
	 (scores (evaluate-ircambeat-on-corpus "Quaero" quaero-subset 0.050))
	 (shortened-scores (.subarray scores '(t (0 49)))))
    (plot-corpus-scores scores :score-index 1)
    (window)
    (nlisp::plot-histogram shortened-scores nil :legends '("precision" "recall" "f-score"))
    (plot-ircambeat-evaluation "Quaero" scores "% of beat")))

;;; Evaluate RWC with relative scores
;; (plot-evaluation "RWC" *rwc-annotations-directory*)
;; (plot-tempo-evaluation "RWC" *rwc-annotations-directory*)
;; (plot-evaluation-2 "RWC" *rwc-annotations-directory*)
;; (quaero-evaluation)

#|
(setf x (evaluate-ircambeat #P"/Users/lsmith/Research/Data/IRCAM-Beat/Quaero/Analysis/0005 - Pink Floyd - Dark Side of the Moon - 05 Great Gig in the sky.wav.markers.xml"
		    #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0005 - Pink Floyd - Dark Side of the Moon - 05 Great Gig in the sky.b_p.xml"
		    0.050))

(setf x (evaluate-ircambeat #P"/Users/lsmith/Research/Data/IRCAM-Beat/Quaero/Analysis/0136 - The Beatles - Abbey Road - 10 Sun King.wav.markers.xml"
		    #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0136 - The Beatles - Abbey Road - 10 Sun King.b_q.xml"
		    0.050))

(setf x (evaluate-ircambeat #P"/Users/lsmith/Research/Data/IRCAM-Beat/RWC/Analysis/RM-P090.wav.markers.xml"
			    #P"/Users/lsmith/Research/Data/IRCAM-Beat/RWC/Annotation/AIST.RWC-MDB-P-2001.BEAT/RM-P090.BEAT.xml"
			    0.165  :annotation-filter #'interpolate-beats))

(evaluate-ircambeat-on-corpus "Quaero"
			      (list #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0005 - Pink Floyd - Dark Side of the Moon - 05 Great Gig in the sky.b_p.xml"
				    #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0136 - The Beatles - Abbey Road - 10 Sun King.b_q.xml")
			      0.25d0 
			      :evaluation #'evaluate-ircambeat-relative-precision)

(setf scores (evaluate-ircambeat-on-corpus "Quaero" (no-hidden-files (cl-fad:list-directory *quaero-annotations-directory*)) 0.050))



(setf score-list (evaluate-ircambeat-tempo-on-corpus "RWC" 
			      (no-hidden-files (cl-fad:list-directory *rwc-annotations-directory*))
			      2.0d0 :evaluation #'evaluate-ircambeat-computed-tempo))

(setf scores (summarise-evaluation-ircambeat "RWC" 
					     (no-hidden-files (cl-fad:list-directory *rwc-annotations-directory*))
					     0.050d0))

(setf scores (summarise-evaluation-ircambeat "RWC" 
					     (no-hidden-files (cl-fad:list-directory *rwc-annotations-directory*))
					     0.35d0
					     :relative-precision t
					     :annotation-filter #'interpolate-beats))

(setf accuracy-2-scores (evaluate-accuracy-2 "RWC"
					     (no-hidden-files (cl-fad:list-directory *rwc-annotations-directory*))
					     0.04d0 
					     :relative-precision t))

(setf x (evaluate-accuracy-2 "RWC" (list
				    #P"/Users/leigh/Research/Data/IRCAM-Beat/RWC/Annotation/AIST.RWC-MDB-P-2001.BEAT/RM-P100.BEAT.xml")
			     0.10d0 
			     :relative-precision t))


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
