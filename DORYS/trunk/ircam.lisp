;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for testing downbeat identification.
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

(defparameter *rhythm-data-directory* (merge-pathnames (make-pathname :directory '(:relative "IRCAM-Beat"))
						       *data-directory*))

;;; The symlink seems to not work over AFP so that this path is unable to be retrieved by POSIX calls.
;;; annotations-directory #P"/Volumes/Quaerodb/annotation/annotation current state/"
(defparameter *quaero-annotations-directory* #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML/")

(defparameter *quaero-selection-annotations-directory* 
  (merge-pathnames (make-pathname :directory '(:relative "Quaero_Selection" "Annotation")) *rhythm-data-directory*))

(defparameter *quaero-selection-analysis-directory* 
  (merge-pathnames (make-pathname :directory '(:relative "Quaero_Selection" "Analysis")) *rhythm-data-directory*))

;;; The location of the audio files that were annotated.
(defparameter *quaero-audio-directory* #P"/Volumes/Quaerodb/annotation/wav-m22k/")

(defun rhythm-from-ircam-odf (&rest params)
  "IRCAMbeat ODF has 100mS silence appended onto the audio file before calculating the
  ODF, which is then compensated for when calculating the markers. We remove it."
  (let* ((leading-silence-seconds 0.1d0)
	 (raw-odf (apply #'mrr::rhythm-from-odf params)))
    (mrr::subset-of-rhythm raw-odf (list (round (* leading-silence-seconds (sample-rate raw-odf))) t))))

(defun beat-marker-filepath (original-sound-path &key (analysis-directory *rhythm-data-directory*))
  "Returns the filepath of the beat marker file associated with the given sound file"
  (merge-pathnames
   (make-pathname :name (concatenate 'string (file-namestring original-sound-path) ".markers")
		  :type "xml")
   analysis-directory))

(defun beat-marker-filepath-anno (annotation-path)
  "Returns the filepath of the beat marker file associated with the given annotation file"
  (make-pathname :defaults annotation-path
		 :directory (append (butlast (pathname-directory annotation-path)) '("Analysis"))
		 :name (concatenate 'string (file-namestring (pathname-name (pathname-name annotation-path))) ".wav.markers")
		 :type "xml"))

(defun bpm-filepath (original-sound-path &key (analysis-directory *rhythm-data-directory*))
  "Returns the filepath of the tempo file associated with the given sound file"
  (merge-pathnames
   (make-pathname :name (concatenate 'string (file-namestring original-sound-path) ".bpm")
		  :type "xml")
   analysis-directory))

(defun odf-filepath (original-sound-path &key (analysis-directory *rhythm-data-directory*))
  "Returns the filepath of the onset detection function file associated with the given sound file"
  (merge-pathnames
   (make-pathname :name (pathname-name original-sound-path)
		  :type "odf")
   analysis-directory))

(defun read-ircam-marker-times (beat-marker-filepath)
  (let* ((clap-times-in-seconds (multires-rhythm::read-ircambeat-markers beat-marker-filepath)))
    ;; Remove the clap times that are negative (!)
   (.arefs clap-times-in-seconds (.find (.> clap-times-in-seconds 0.0d0)))))

(defun ircambeat-computed-bpm (bpm-filepath)
  (.aref (mrr::read-ircambeat-bpm bpm-filepath) 0))

(defun downbeats-of-times (times anacrusis beats-per-measure)
  "Returns those times which occur on the downbeat, i.e once every measure, with an
  anacrusis (in notes). The anacrusis indicates the start location, not the number of preceding notes."
  (let ((downbeat-indices (.+ (.* (.iseq 0 (floor (- (.length times) anacrusis 1) beats-per-measure)) beats-per-measure) anacrusis)))
    (format t "downbeat indices ~a~%" downbeat-indices)
    (.arefs times downbeat-indices)))

(defun downbeat-times (beat-marker-path &key (anacrusis 0) (sound-every 1))
  "Returns the times of downbeats in the IRCAMbeat marker file"
  (downbeats-of-times (read-ircam-marker-times beat-marker-path) anacrusis sound-every))

(defun count-corpus (corpus-example)
  (.length (downbeat-times (make-pathname :directory *rhythm-data-directory* :name (first corpus-example) :type "marker.xml") 
			   :anacrusis (third corpus-example) 
			   :sound-every 4))) ; TODO we've fixed this as 4/4, needs fixing.

;;; Find the average number of downbeats in each example
;;; (mean (.* (make-narray (mapcar #'count-corpus *ircam-downbeats*)) 1d0))
;;; Find the total number of downbeats.
;;; (.sum (make-narray (mapcar #'count-corpus *ircam-downbeats*)))

;;;
(defun annotated-downbeats (ircam-annotation-path)
  "Retrieve the times of nominated downbeats from an annotation file"
  (multiple-value-bind (times beats) 
      (mrr::read-ircam-annotation ircam-annotation-path)
    (let* ((downbeat-indices (.find (.= beats 1)))
	   (downbeats (.arefs times downbeat-indices)))
      ;; (format t "~a~%first downbeat indices ~a~%" (pathname-name ircam-annotation-path) (.subseq downbeat-indices 0 10))
      (values downbeats downbeat-indices))))

(defun annotated-beats (ircam-annotation-path)
  "Retrieve the times of beats and downbeats, skipping beat = 0, which are tatums, in the IRCAM annotation convention"
  (multiple-value-bind (times beats) 
      (mrr::read-ircam-annotation ircam-annotation-path)
    (let* ((beat-indices (.find (.> beats 0)))
	   (beat-times (.arefs times beat-indices)))
      (values beat-times beat-indices))))

(defun track-named (number corpus)
  "Find the example with the leading text matching the number"
  ;; (format nil "~4,0D" number)
  (find number corpus :key #'first 
	:test (lambda (x y) (string= x y :end1 4 :end2 4))))  

;;;
;;; Sonification
;;;

(defun correct-clap-file (original-sound-file)
  "Returns the right audio file matching the sample rate of the file to be accompanied"
  (merge-pathnames 
   (make-pathname :directory '(:relative "Handclap Examples") 
		  :name (case (nlisp::audio-parameters original-sound-file)
			  (22050 "cowbell_22kHz_mono") 
			  (44100 "cowbell"))
		  :type "aiff")
   *data-directory*))

;;; The default is to sonify all beats
(defun sonify-ircambeat (original-sound-path &key (directory *rhythm-data-directory*)
			  (anacrusis 0) (sound-every 1) 
			  (clap-sample-file (correct-clap-file original-sound-path)))
  "Write out a sound file mixed with downbeats as computed by IRCAMbeat"
  ;; TODO (accompaniment-filepath original-sound-path :analysis-directory directory)
  (let* ((accompaniment-sound-path (merge-pathnames (make-pathname 
						     :directory '(:relative "Verification")
						     :name (concatenate 'string (pathname-name original-sound-path) "_mixed")
						     :type "wav")
						    directory))
	 (beat-marker-path (beat-marker-filepath original-sound-path :analysis-directory directory)))
    (mrr:save-rhythm-mix accompaniment-sound-path 
			 original-sound-path 
			 (downbeat-times beat-marker-path 
					 :anacrusis anacrusis
					 :sound-every sound-every)
			 :clap-sample-file clap-sample-file)
    (format t "Wrote mix as soundfile ~a~%" accompaniment-sound-path)))

;;; (sonify-ircambeat *rhythm-data-directory* "rwc_p_99_excerpt" "wav")

(defun sonify-corpus (corpus-name corpus-example)
  (sonify-ircambeat (merge-pathnames (make-pathname :name (first corpus-example) :type "wav") 
				     *quaero-audio-directory*)
		    :directory (merge-pathnames 
				(make-pathname :directory (list :relative corpus-name)) 
				*rhythm-data-directory*)
		     :anacrusis (third corpus-example) 
		     :sound-every 4)) ; TODO we've fixed this as 4/4, needs fixing.

;;; (mapcar #'sonify-corpus *ircam-downbeats*)
;;; (sonify-corpus "Quaero" (nth 0 *ircam-downbeats*))

(defun sonify-annotation (ircam-annotation-path original-sound-path accompaniment-directory
				&key (clap-sample-file (correct-clap-file original-sound-path)))
  "Make the annotated downbeats audible"
  (let* ((downbeats (annotated-downbeats ircam-annotation-path))
	 (accompaniment-sound-path (make-pathname :directory accompaniment-directory
						  :name (concatenate 'string (pathname-name original-sound-path) "_mixed")
						  :type "wav")))
    (multires-rhythm:save-rhythm-mix accompaniment-sound-path 
				     original-sound-path 
				     downbeats
				     :clap-sample-file clap-sample-file)
    (format t "Wrote mix as soundfile ~a~%" accompaniment-sound-path)))

(defun sonify-quaero-annotation (annotation-filepath &key (audio-directory *quaero-audio-directory*))
  "Creates a sonification from the annotation-pathspec, which can have a relative directory component"
  (sonify-annotation annotation-filepath
		     (merge-pathnames audio-directory
				      (make-pathname :directory (pathname-directory annotation-filepath)
						     :name (pathname-name (pathname-name annotation-filepath))
						     :type "wav"))
		     ;; (merge-pathnames (make-pathname :directory '(:relative "Quaero" "Verification"))
		     ;; *rhythm-data-directory*)
		     "/Volumes/iDisk/Research/Data/IRCAM-Beat/Quaero/Verification"))

(defun sonify-random-sample (max-number)
  "Sonify a random sample of the annotations for evaluation"
  (loop
     with rs = (make-random-state t)
     with annotation-files = (cl-fad:list-directory *quaero-annotations-directory*)
     with corpus-length = (length annotation-files)
     repeat max-number
     for corpus-sample-pathname = (nth (random corpus-length rs) annotation-files) 
     when (cl-fad:file-exists-p corpus-sample-pathname)
     do (sonify-quaero-annotation corpus-sample-pathname)))

(defun sonify-downbeat-estimate (original-sound-file)
  "Sonify just the downbeats as estimated by the downbeat finder"
  (sonify-ircambeat original-sound-file
		     :anacrusis (ircambeat-marker-of-downbeat original-sound-file)
		     :sound-every 4  ; TODO we've fixed this as 4/4, needs correcting.
		     :clap-sample-file (correct-clap-file original-sound-file)))

(defun sonify-evaluation (example)
  (sonify-downbeat-estimate (make-pathname :directory *rhythm-data-directory* :name (first example) :type "wav")))

(defun nearest-beat-to-time (time-in-seconds beat-markers)
  "Returns the index of the beat marker computed by IRCAMbeat which is closest to the given time"
  (let* ((distance-from-markers (.abs (.- beat-markers time-in-seconds)))
	 (nearest-beat (argmin distance-from-markers))
	 (nearest-distance (.aref distance-from-markers nearest-beat))
	 ;; This isn't strictly right, it should be the max of interval either side of the
	 ;; nearest beat, but the difference will be small and this is easier to deal with corners.
	 (beat-duration (- (.aref beat-markers (1+ nearest-beat)) (.aref beat-markers nearest-beat))))
    ;; (format t "distance from beat-markers ~a~%" distance-from-markers)
    (format t "nearest distance to beat-markers ~a, beat-duration ~a~%" nearest-distance beat-duration)
    ;; check the markers are too far away from the downbeat.
    (if (> (/ nearest-distance beat-duration) 0.25)
	(format t "ERROR: beat marker ~,3f too far away from estimated downbeat position~%"
		(.aref beat-markers nearest-beat)))
    nearest-beat))

(defun nearest-beat-to-downbeat (beat-markers downbeat tempo-in-bpm)
  (let* ((beat-duration (/ 60.0d0 tempo-in-bpm))
	 (downbeat-in-seconds (* beat-duration downbeat))
	 (nearest (nearest-beat-to-time downbeat-in-seconds beat-markers)))
    (format t "From ~,2f BPM, downbeat should be ~,3f seconds, nearest marker ~d~%"
	    tempo-in-bpm downbeat-in-seconds nearest)
    (format t "From ircambeat markers (search from first):~%~,3a~%" (.subseq beat-markers 0 5))
    nearest))

;;; Holds the learnt probabilities derived from observations of anacrusis from locations
;;; of beat gaps.
(defparameter *downbeat-probabilities* nil)

(defun ircam-find-downbeat (odf-filepath beat-markers-filepath &key (sample-rate 172.27d0))
  "Given the files, compute the downbeat"
  (let* ((rhythm (rhythm-from-ircam-odf odf-filepath :sample-rate sample-rate :weighted nil))
	 (beat-markers (read-ircam-marker-times beat-markers-filepath))
	 ;; TODO beats-per-measure is derived from time signature. hardwired to 4/4 for now.
	 (meter '(2 2 2))		; TODO hardwired for now
	 (beats-per-measure 4)		; TODO hardwired for now
	 (start-from (.aref beat-markers 0))
	 (odf-subset (mrr::subset-of-rhythm rhythm (list (round (* sample-rate start-from)) t))))
    (format t "Starting downbeat finding from ~,3f seconds~%" start-from)
    ;; (mrr::downbeat-estimation-fixed odf-subset bpm meter beats-per-measure))) ; Test the null hypothesis
    ;; (mrr::downbeat-estimation-random odf-subset bpm meter beats-per-measure))) ; Test the null hypothesis
    ;; (mrr::downbeat-estimation-phase odf-subset bpm meter beats-per-measure)))
    ;; (mrr::downbeat-estimation-duration odf-subset bpm meter beats-per-measure)))
    ;; (mrr::downbeat-estimation-amplitude odf-subset bpm meter beats-per-measure)))
    (mrr:downbeat-estimation odf-subset beat-markers meter beats-per-measure *downbeat-probabilities*)))

;;; (ircam-find-downbeat #P"/Users/lsmith/Research/Data/IRCAM-Beat/Quaero_excerpts/Analysis/0186 - Dillinger_excerpt.odf" #P"/Users/lsmith/Research/Data/IRCAM-Beat/Quaero_excerpts/Analysis/0186 - Dillinger_excerpt.wav.markers.xml")

(defun ircambeat-marker-of-downbeat (original-sound-path &key (analysis-directory *rhythm-data-directory*))
  "Given the original sound file, determine the downbeat, and find the marker produced by
  IRCAMbeat that matches it in time"
  (let* ((beat-markers-file (beat-marker-filepath original-sound-path :analysis-directory analysis-directory))
	 (odf-file (odf-filepath original-sound-path :analysis-directory analysis-directory))
;;	 (bpm-file (bpm-filepath original-sound-path :analysis-directory analysis-directory))
;;	 (bpm (.aref (mrr::read-ircambeat-bpm bpm-file) 0))
	 ;; TODO Start the odf based on the first marker. This needs replacing
	 ;; with starting from the first identified onset.
	 (found-downbeat (ircam-find-downbeat odf-file beat-markers-file)))
	 ;; Since the downbeat is determined from an ODF that begins on the first IRCAMbeat
	 ;; marker, we assume the beat matches.
;;;	 (nearest-marker (nearest-beat-to-time ircambeat-markers found-downbeat bpm)))
;;;    (format t "Found downbeat shifted by ~d beats, nearest ircambeat marker ~d, starts at ~,3f seconds~%" 
;;;	    found-downbeat nearest-marker (.aref ircambeat-markers nearest-marker))
    found-downbeat))

(defun quaero-downbeat (filename)
  "To analyse a single Quaero selection example"
  (ircambeat-marker-of-downbeat
   (merge-pathnames (make-pathname :directory '(:relative "Quaero_Selection" "Audio")
				   :name filename
				   :type "wav")
		    *rhythm-data-directory*)
   :analysis-directory *quaero-selection-analysis-directory*))

;; (quaero-downbeat "0001 - U2 - The Joshua Tree - With or without you")
;; (quaero-downbeat "0008 - Pink Floyd - Dark Side of the Moon - 08 Any color you like")
;; (quaero-downbeat "0028 - Jedi Mind Tricks - Visions of Ghandi - 02 Tibetan Black Magician")
;; (quaero-downbeat "0100 - Madcon - So Dark The Con Of Man - Beggin")

(defun learn-observation-probabilities (corpus &key 
					(sample-rate 172.27d0)
					(analysis-directory 
					 (merge-pathnames (make-pathname :directory '(:relative "Quaero_Selection" "Analysis"))
							  *rhythm-data-directory*)))

  "Given the corpus, compute the observation probabilties and return as a matrix"
  (loop
     with meter = '(2 2 2)		; TODO hardwired! read this from annotation file.
     with beats-per-measure = 4		; TODO hardwired! read this from annotation file.
     with observation-symbols = (* beats-per-measure mrr::*subdivisions-of-beat*)
     with observation-probs = (make-double-array (list beats-per-measure observation-symbols))
     with number-of-anacruses = (make-double-array beats-per-measure)
     for music in corpus
     for filename = (first music)
     for original-sound-path = (merge-pathnames (make-pathname :directory '(:relative "Quaero_Selection" "Audio")
							       :name filename
							       :type "wav")
						*rhythm-data-directory*)
     for anacrusis = (third music)
     for first-downbeat-time = (fifth music)
     do
       (format t "Evaluating ~a~%" filename)
       (let* ((beat-markers-filepath (beat-marker-filepath original-sound-path :analysis-directory analysis-directory))
	      (odf-filepath (odf-filepath original-sound-path :analysis-directory analysis-directory))
	      (rhythm (rhythm-from-ircam-odf odf-filepath :sample-rate sample-rate :weighted nil))
	      (beat-markers (read-ircam-marker-times beat-markers-filepath))
	      (time-limited-markers (mrr::prune-outliers beat-markers :lower-limit first-downbeat-time))
	      (start-from first-downbeat-time) ; was (.aref beat-markers 0), now skip the intro's.
	      (odf-subset (mrr::subset-of-rhythm rhythm (list (round (* sample-rate start-from)) t)))
	      (rhythm-obs-probs (mrr::observation-probabilities odf-subset time-limited-markers meter beats-per-measure)))
	 (format t "For anacrusis ~a observation probs ~a~%" anacrusis rhythm-obs-probs)
	 (setf (.row observation-probs anacrusis) (.+ (.row observation-probs anacrusis) rhythm-obs-probs))
	 (incf (.aref number-of-anacruses anacrusis)))
     finally 
       (return 
	 (progn
	   ;; Set any 0 valued counts to 1 to avoid divide by zero errors.
	   (setf (.arefs number-of-anacruses (.find (.not number-of-anacruses))) 1.0d0)
	   (dotimes (beat-index beats-per-measure observation-probs)
	     (setf (.row observation-probs beat-index) (./ (.row observation-probs beat-index)
							   (.aref number-of-anacruses beat-index))))))))

;;;
;;; Evaluate the downbeat finder.
;;;

(defun evaluate-ircam-downbeat (ircam-example &key (audio-directory *rhythm-data-directory*)
				(analysis-directory *rhythm-data-directory*))
  "Evaluate the downbeat finder against the annotated example"
  (let* ((filename (first ircam-example))
	 (anacrusis (third ircam-example))
	 (sound-filename (merge-pathnames (make-pathname :name filename :type "wav") audio-directory))
	 (found-downbeat (ircambeat-marker-of-downbeat sound-filename :analysis-directory analysis-directory)))
    (format t "For ~a found downbeat ~a vs. ground truth ~a~%" filename found-downbeat anacrusis)
    (= found-downbeat anacrusis)))

(defun evaluate-quaero-downbeat (ircam-example)
  "Simply reassigns directories where the sounds are located"
  (evaluate-ircam-downbeat ircam-example
			   :analysis-directory *quaero-selection-analysis-directory*
			   :audio-directory *quaero-audio-directory*))

(defun annotated-anacrusis (ircam-annotation-path ircambeat-marker-path beats-per-measure)
  "Returns the beat location of the ircambeat marker nearest the first annotated downbeat"
  (let* ((annotated-downbeat-times (annotated-downbeats ircam-annotation-path))
	 (ircambeat-markers (read-ircam-marker-times ircambeat-marker-path))
	 ;; TODO this could be expanded to find the number of ircambeat-markers across the whole rhythm.
	 (nearest-beat (nearest-beat-to-time (.aref annotated-downbeat-times 0) ircambeat-markers)))
    (format t "nearest-beat ~a = ~a~%" nearest-beat (.aref ircambeat-markers nearest-beat))
    (format t "downbeat times ~a~%first ircambeat markers ~a~%" 
	    (.subseq annotated-downbeat-times 0 2) 
	    (.subseq ircambeat-markers (max (1- nearest-beat) 0) (1+ nearest-beat)))
    ;; folds pieces with long preceding non-metrical intervals to the nearest beat-phase.
    (list (mod nearest-beat beats-per-measure) (.aref annotated-downbeat-times 0))))

(defun make-quaero-dataset (max every &key (annotations-directory *quaero-annotations-directory*))
  "Generate a list of entries containing names and anacruses, suitable for testing evaluate-quaero-downbeat with"
  (loop
     with annotation-files = (cl-fad:list-directory annotations-directory)
     for song-index from 0 below max by every
     for annotation-pathname = (nth song-index annotation-files)
     for annotation-name = (pathname-name (pathname-name annotation-pathname))
     for ircambeat-marker-pathname = (beat-marker-filepath-anno annotation-pathname)
     for beats-per-measure = 4 ; TODO hardwired for now (should get from annotation).
     ;; Get the initial upbeat
     for (anacrusis downbeat-time) = (annotated-anacrusis annotation-pathname ircambeat-marker-pathname beats-per-measure)
     do (format t "~a anacrusis ~d~%" annotation-name anacrusis)
     when (cl-fad:file-exists-p ircambeat-marker-pathname) ; TODO this is rather useless here.
     ;; if we want to exclude pieces with long preceding non-metrical intervals.
     ;; when (< anacrusis beats-per-measure) 
     collect (list annotation-name :anacrusis anacrusis :first-downbeat-time downbeat-time)))

(defun train-on-quaero-dataset (size)
  (let ((training-dataset (make-quaero-dataset (* size 2) 2 :annotations-directory *quaero-selection-annotations-directory*)))
    (setf *downbeat-probabilities* (learn-observation-probabilities training-dataset))
    ;; (diag-plot 'downbeat-probabilities
    (image *downbeat-probabilities* nil nil 
	   :title (format nil "Emission probability of observing gap location given downbeat from ~a examples" 
			  (length training-dataset))
	   :xlabel "Observed Gap location" 
	   :ylabel "Actual downbeat (hidden)")
    *downbeat-probabilities*))


;; (setf small-quaero (make-quaero-dataset 10 1 :annotations-directory *quaero-selection-annotations-directory*))
;; (setf bad-examples (evaluate-with-music #'evaluate-quaero-downbeat :music-dataset small-quaero :music-name #'first))
;; (setf half-quaero (make-quaero-dataset 300 2))
;; (setf select-quaero (make-quaero-dataset 100 1 :annotations-directory *quaero-selection-annotations-directory*))
;; (setf bad-examples (evaluate-with-music #'evaluate-quaero-downbeat :music-dataset select-quaero :music-name #'first))

(defun save-annotations ()
  "Determines the anacruses for all of the Quaero dataset and writes the anacruses to a file"
  (with-open-file (out (merge-pathnames (make-pathname :directory '(:relative "Quaero" "Annotation") 
						       :file "anacruses" :type "lisp")
					*rhythm-data-directory*)
		       :direction :output :if-exists :supersede) 
    (write (make-quaero-dataset 250 1) :stream out :readable t :pretty t)))

;; Load in the anacruses.
;; (setf full-quaero (with-open-file (f "Quaero/Annotation/anacruses.lisp" :direction :input) (read f)))


#|

(setf bad-examples (evaluate-with-music #'evaluate-ircam-downbeat :music-dataset *ircam-downbeats* :music-name #'first))

(sonify-ircambeat #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186 - Dillinger_excerpt.wav")

(sonify-ircambeat #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0051 - Buenavista_excerpt.wav")

;; Sonification of Local files
(sonify-quaero #P"0186b - Dillinger - Cocaine - 01 Cocaine In My Brain" 
	       :annotations-directory *rhythm-data-directory*
	       :audio-directory *rhythm-data-directory*)

(sonify-quaero #P"0144b - The Beatles - A Hard Days Night - 01 A Hard Day s Night.xml"
	       :annotations-directory *rhythm-data-directory*
	       :audio-directory *rhythm-data-directory*)

(sonify-quaero #P"0051b - Buenavista social club - Buenavista social club - Chan chan.xml"
	       :annotations-directory *rhythm-data-directory*
	       :audio-directory *rhythm-data-directory*)

;; (setf ircambeat-markers (mrr::read-ircambeat-markers #P"/Local/Users/leigh/Research/Sources/Rhythm/IRCAM/LeighsTests/res4_1.wav.markers.xml"))
;; (setf mrr-markers (make-narray '(0.1d0 0.69d0 1.26d0 1.8d0 2.325d0 2.865d0 3.445d0 4.155d0 4.855d0  5.52d0 6.165d0 6.795d0 7.425d0 8.025d0 8.58d0 9.125d0)))
;; (nplot (list (.diff ircambeat-markers) (.diff mrr-markers)) nil :styles '("linespoints" "linespoints") :aspect-ratio 0.66)

;; (setf res4_2-ircambeat-markers (read-ircambeat-markers #P"/Local/Users/leigh/Research/Sources/Rhythm/IRCAM/LeighsTests/res4_2.wav.markers.xml"))
;; (setf res4_2-mrr-markers (make-narray '(0.475d0 0.985d0 1.54d0 2.2d0 2.85d0 3.495d0 4.145d0 4.825d0 5.56d0
;;         6.285d0 6.985d0 7.695d0 8.38d0 9.04d0 9.69d0 10.325d0 10.93d0 11.51d0)))
;; (nplot (list (.diff res4_2-ircambeat-markers) (.diff res4_2-mrr-markers)) nil :styles '("linespoints" "linespoints") :aspect-ratio 0.66)

(setf rwc95-times (.load #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc95_times.txt" :format :text))
(setf sample-rate (/ 1 (mean (.diff (.column rwc95-times 0)))))
(setf rwc95-odf (.load #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_95.odf" :format :text))
(setf rwc95-onsets (.load #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_95.onsets" :format :text))
(plot (.column rwc95-odf 0) nil :aspect-ratio 0.2)
(plot (make-double-array (.length rwc95-onsets) :initial-element 1.0d0) (.column rwc95-onsets 0) :style "impulses" :aspect-ratio 0.2)

|#
