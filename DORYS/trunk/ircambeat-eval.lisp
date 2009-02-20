;;;; -*- Lisp -*-
;;;;
;;;; $Id$
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

(defun corpus-beat-marker-filepath (corpus-name annotation-filepath)
  "Given the corpus and the annotation name, return the beat-marker filepath"
  (let ((analysis-directory (merge-pathnames (make-pathname :directory (list :relative corpus-name)) *rhythm-data-directory*))
	(audio-filepath (make-pathname :name (pathname-name (pathname-name annotation-filepath)) :type "wav")))
    (beat-marker-filepath audio-filepath :analysis-directory analysis-directory)))

(defun evaluate-within-annotation (computed-beat-times annotation-times precision-window)
  "Remove times before and after the annotation times"
  (let ((time-limited-markers (mrr::prune-outliers computed-beat-times 
						   :lower-limit (- (.aref annotation-times 0) precision-window)
						   :upper-limit (+ (.last annotation-times) precision-window))))
    (evaluate-beat-times time-limited-markers annotation-times precision-window)))

(defun evaluate-ircambeat (ircambeat-marker-filepath annotation-filepath precision-window
			   &key annotation-filter relative-precision)
  "Retrieve the times of nominated beats from an annotation file & return the match scores"
  (let* ((annotation-times (annotated-beats annotation-filepath))
	 (filtered-annotation-times (if annotation-filter (funcall annotation-filter annotation-times)
					annotation-times))
	 (absolute-precision-window (if relative-precision 
					(precision-window-of-times annotation-times precision-window) 
					precision-window))
	 (ircambeat-marker-times (read-ircam-marker-times ircambeat-marker-filepath)))
    (plot-min-distance (format nil "~a ~a" (pathname-name annotation-filepath) annotation-filter)
		       ircambeat-marker-times filtered-annotation-times precision-window)
    (evaluate-within-annotation ircambeat-marker-times filtered-annotation-times absolute-precision-window)))

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
     for (precision recall f-score) = (evaluate-within-annotation ircambeat-marker-times filtered-annotation-times absolute-precision-window)
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

;; TODO this should be able to be replaced by (.arefs scores max-score-indices t t)
;; And/or (reduce-dimension #'.arefs)
;; This is not as general as the name suggests, it makes big assumptions about the data
;; structure (lists of narrays).
(defun select-from-indices (scores max-score-indices)
  (let* ((track-count (.length max-score-indices))
	 (max-scores (make-double-array (list track-count (length scores)))))
    (dotimes (track-index track-count max-scores)
      (setf (.row max-scores track-index)
	    (.row (nth (.aref max-score-indices track-index) scores) track-index)))))

(defun evaluate-accuracy-2 (corpus-name corpus precision-window &key relative-precision) 
  "Selects the best evaluations between the annotations and filterings thereof on the given corpus"
  (loop
     for annotation-filter in (list nil #'interpolate-beats #'half-beats) 
     for scores-per-track = (evaluate-ircambeat-on-corpus corpus-name 
							  corpus 
							  precision-window
							  :relative-precision relative-precision
							  :annotation-filter annotation-filter)
     for f-scores = (nlisp::array-to-list (.column scores-per-track 2))
     do
       (format t "~%for ~a ~a mean scores ~a~%" corpus-name annotation-filter (mean-scores scores-per-track))
     collect scores-per-track into scores
     collect f-scores into f-score-comparison
     finally (return (select-from-indices scores (mrr::find-column-maxima (make-narray f-score-comparison))))))

;; Alternative:
;; (setf f-score-comparison (mapcar (lambda (x) (nlisp::array-to-list (.column x 2))) accuracy-2-scores) )


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
  (plot-command "set title font \"Times,24\"")
  (plot-command "set xlabel font \"Times,24\"")
  (plot-command "set ylabel font \"Times,24\"")
  (plot (list (.column scores 1) (.column scores 2)) (.column scores 0) 
	:title (format nil "Evaluation of IRCAM-beat on ~a dataset" corpus-name)
	:legends  '("Mean Precision" "Mean Recall")
	:styles '("linespoints linewidth 2" "linespoints linewidth 2 linetype 3") 
	:xlabel (format nil "Precision Window (~a)" units)
	:ylabel "Proportion Correct"
	:reset nil)
  (close-window)
  scores)

(defun plot-low-scores (corpus-name corpus-directory &key (precision-window 0.15d0))
  "Plots evaluations with low values in greater detail than plot-evaluation"
  (let* ((f-score-index 2)
	 (corpus (no-hidden-files (cl-fad:list-directory corpus-directory)))
	 (scores (evaluate-ircambeat-on-corpus corpus-name corpus precision-window :relative-precision t))
	 (counter-beat-scores (evaluate-ircambeat-on-corpus corpus-name corpus precision-window
							    :relative-precision t
							    :annotation-filter #'counter-beats))
	 (interpolated-beat-scores (evaluate-ircambeat-on-corpus corpus-name corpus precision-window
								 :relative-precision t
								 :annotation-filter #'interpolate-beats))
	 (half-beat-scores (evaluate-ircambeat-on-corpus corpus-name corpus precision-window
							 :relative-precision t
							 :annotation-filter #'half-beats))
	 (f-scores (.column scores f-score-index))
	 (c-f-scores (.column counter-beat-scores f-score-index))
	 (indices-of-low-f-scores (.find (.and (.< f-scores 0.5d0)
					       (.< c-f-scores f-scores)
					       (.< (.column interpolated-beat-scores f-score-index) f-scores)
					       (.< (.column half-beat-scores f-score-index) f-scores))))
	 (low-scores (nlisp::.arefs-multi scores indices-of-low-f-scores t))
	 (low-score-corpus (map 'list (lambda (x) (nth x corpus)) (val indices-of-low-f-scores))))
    (window)
    ;; TODO Need to be able to handle vectors.
    ;; (plot-histogram f-scores nil 
    ;;	  :legends '("f-score")
    ;;	  :aspect-ratio 0.66)
    (plot f-scores nil 
    	  :styles "boxes fill solid border 9" 
    	  :legends (list (nth f-score-index '("precision" "recall" "f-score")))
    	  :aspect-ratio 0.66)
    (close-window)
    (window)
    (plot-command "set xtics nomirror rotate by -45")
    (plot-command "set xtics rotate (~{~{\"~a\" ~5d~}~^, ~})~%" 
		  (loop 
		     for name in low-score-corpus
		     for name-index = 0 then (1+ name-index)
		     collect (list (pathname-name name) name-index)))
    (plot-histogram (make-narray (list (.arefs f-scores indices-of-low-f-scores) 
				       (.arefs c-f-scores indices-of-low-f-scores)))
		    nil
		    :reset nil
		    :legends '("f-score" "counter beat f-score")
		    :title (format nil "Low scores and counter beat scores of ~a" corpus-name)
		    :ylabel "Proportion Correct"
		    :aspect-ratio 0.66
		    :gap 2)
    (close-window)
    (window)
    ;; (.arefs (.column low-scores 0) indices-of-low-f-scores)
    (plot-histogram (.transpose low-scores)
		    nil 
		    :legends '("precision" "recall" "f-score")
		    :aspect-ratio 0.66
		    :gap 1
		    :ylabel "Proportion Correct"
		    :title (format nil "Low scores of ~a" corpus-name))
    (close-window)
    indices-of-low-f-scores))

(defun plot-evaluation (corpus-name corpus-directory)
  "Performs and plots evaluations with a range of precision widths, both absolute and relative"
  (loop
     with corpus = (no-hidden-files (cl-fad:list-directory corpus-directory)) 
     for annotation-filter in (list nil #'counter-beats #'interpolate-beats #'half-beats) 
     ;; evaluate-precision-window
     for scores = (evaluate-relative-precision-window corpus-name corpus :annotation-filter annotation-filter)
     for evaluation-description = (format nil "~a ~a" corpus-name annotation-filter) 
     do (plot-ircambeat-evaluation evaluation-description scores "% of beat") ; "seconds"
     finally (plot-ircambeat-evaluation (format nil "~a accuracy 2" corpus-name)
					(evaluate-relative-accuracy-2 corpus-name corpus) "% of beat")))

(defun plot-tempo-evaluation (corpus-name corpus-directory)
  (let* ((corpus (no-hidden-files (cl-fad:list-directory corpus-directory))) 
	 (scores (evaluate-ircambeat-tempo-on-corpus corpus-name corpus 0.04d0
						     :evaluation #'evaluate-ircambeat-computed-tempo-relative))
	 ;; calculating the mean on binary valued scores computes the % result.
	 (mean-scores (mrr::reduce-dimension scores #'mean)))
    (format t "mean scores for tempo evaluation ~a~%" mean-scores)))
    ;; (plot-ircambeat-evaluation corpus-name scores "% of annotated BPM")

(defun evaluate-all-parameter-sets ()
  "The full enchilda..."
  ;; Evaluate RWC with relative scores
  (plot-evaluation "RWC" *rwc-annotations-directory*)
  (plot-evaluation "RWC_cl120n22" *rwc-annotations-directory*)
  (plot-evaluation "RWC_60n22" *rwc-annotations-directory*)
  (plot-evaluation "RWC_240n22" *rwc-annotations-directory*))

;; (plot-low-scores "RWC"  *rwc-annotations-directory*)
;; (plot-tempo-evaluation "RWC" *rwc-annotations-directory*)

;; (quaero-subset (random-select (cl-fad:list-directory *quaero-annotations-directory*) 100))
;; (plot-ircambeat-evaluation "Quaero" scores "% of beat")
;; (quaero-evaluation)

(defun examine-bad-file (name &key (precision 0.100d0))
  (let* ((annotation-path (make-pathname :defaults *rwc-annotations-directory* :name name :type "BEAT.xml"))
	 (marker-path (corpus-beat-marker-filepath "RWC" annotation-path))
	 (standard-score (evaluate-ircambeat marker-path annotation-path precision
					     :relative-precision t :annotation-filter nil))
	 (counter-beat-score (evaluate-ircambeat marker-path annotation-path precision
						 :relative-precision t :annotation-filter #'counter-beats)))
    (format t "standard score ~a~%counter beat score ~a~%" standard-score counter-beat-score)))


#|

(examine-bad-file "RM-P041")


(setf x (evaluate-ircambeat #P"/Users/lsmith/Research/Data/IRCAM-Beat/Quaero/Analysis/0005 - Pink Floyd - Dark Side of the Moon - 05 Great Gig in the sky.wav.markers.xml"
		    #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0005 - Pink Floyd - Dark Side of the Moon - 05 Great Gig in the sky.b_p.xml"
		    0.050))

(setf x (evaluate-ircambeat #P"/Users/lsmith/Research/Data/IRCAM-Beat/Quaero/Analysis/0136 - The Beatles - Abbey Road - 10 Sun King.wav.markers.xml"
		    #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/0136 - The Beatles - Abbey Road - 10 Sun King.b_q.xml"
		    0.050))

(setf x (evaluate-ircambeat #P"/Users/lsmith/Research/Data/IRCAM-Beat/RWC/Analysis/RM-P090.wav.markers.xml"
			    #P"/Users/lsmith/Research/Data/IRCAM-Beat/RWC/Annotation/AIST.RWC-MDB-P-2001.BEAT/RM-P090.BEAT.xml"
			    0.165  :annotation-filter #'interpolate-beats))

(setf x (evaluate-ircambeat #P"/Users/lsmith/Research/Data/IRCAM-Beat/RWC/Analysis/RM-P007.wav.markers.xml"
			    #P"/Users/lsmith/Research/Data/IRCAM-Beat/RWC/Annotation/AIST.RWC-MDB-P-2001.BEAT/RM-P007.BEAT.xml"
			    0.100d0 :annotation-filter nil))

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

