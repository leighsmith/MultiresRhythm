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

;;; The location of the audio files that were annotated.
(defparameter *quaero-audio-directory* #P"/Volumes/Quaerodb/annotation/wav-m22k/")

;;; Holds the ground truth for the anacrusis (number of beats to shift to find the
;;; downbeat) or each example.
(defparameter *ircam-downbeats*
  '(("rwc_p_81_excerpt" :anacrusis 1)
    ("rwc_p_82_excerpt" :anacrusis 0) ; There is an anacrusis which IRCAM beat skips
    ("rwc_p_83_excerpt" :anacrusis 0)
    ("rwc_p_84_excerpt" :anacrusis 0)
;    ("rwc_p_85_excerpt" :anacrusis 0) ; This isn't correct, off by a quaver
    ("rwc_p_86_excerpt" :anacrusis 0)
    ("rwc_p_87_excerpt" :anacrusis 0)
    ("rwc_p_88_excerpt" :anacrusis 3)
    ("rwc_p_89_excerpt" :anacrusis 3)
    ("rwc_p_90_excerpt" :anacrusis 1)
    ("rwc_p_91_excerpt" :anacrusis 0)
    ("rwc_p_92_excerpt" :anacrusis 2)
    ("rwc_p_93_excerpt" :anacrusis 0)
    ("rwc_p_94_excerpt" :anacrusis 0)
;    ("rwc_p_95_excerpt" :anacrusis 2) ;This is hard to identify, it seems to change.
    ("rwc_p_96_excerpt" :anacrusis 0)
    ("rwc_p_97_excerpt" :anacrusis 0)
    ("rwc_p_98_excerpt" :anacrusis 1)
    ("rwc_p_99_excerpt" :anacrusis 1)
    ("rwc_p_100_excerpt" :anacrusis 3)))

(defun rhythm-from-ircam-odf (&rest params)
  "IRCAMbeat ODF has 100mS silence appended onto the audio file before calculating the
  ODF, which is then compensated for when calculating the markers. We remove it."
  (let* ((leading-silence-seconds 0.1d0)
	 (raw-odf (apply #'mrr::rhythm-from-odf params)))
    (mrr::subset-of-rhythm raw-odf (list (round (* leading-silence-seconds (sample-rate raw-odf))) t))))

(defun beat-marker-filepath (original-sound-path &key (analysis-directory *rhythm-data-directory*))
  "Returns the filepath of the beat marker file associated with the given sound file"
  (merge-pathnames
   (make-pathname :directory '(:relative "Analysis")
		  :name (concatenate 'string (file-namestring original-sound-path) ".markers")
		  :type "xml")
   analysis-directory))

(defun bpm-filepath (original-sound-path &key (analysis-directory *rhythm-data-directory*))
  "Returns the filepath of the tempo file associated with the given sound file"
  (merge-pathnames
   (make-pathname :directory '(:relative "Analysis")
		  :name (concatenate 'string (file-namestring original-sound-path) ".bpm")
		  :type "xml")
   analysis-directory))

(defun odf-filepath (original-sound-path &key (analysis-directory *rhythm-data-directory*))
  "Returns the filepath of the onset detection function file associated with the given sound file"
  (merge-pathnames
   (make-pathname :directory '(:relative "Analysis")
		  :name (pathname-name original-sound-path)
		  :type "odf")
   analysis-directory))

(defun read-ircam-marker-times (beat-marker-filepath)
  (let* ((clap-times-in-seconds (multires-rhythm::read-ircambeat-markers beat-marker-filepath)))
    ;; Remove the clap times that are negative (!)
   (.arefs clap-times-in-seconds (.find (.> clap-times-in-seconds 0.0d0)))))

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
    (multires-rhythm::save-rhythm-mix accompaniment-sound-path 
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

(defun ircam-find-downbeat (odf-filepath bpm-filepath &key (start-from 0.0d0) (sample-rate 172.27d0))
  "Given the files, compute the downbeat"
  (let* ((rhythm (rhythm-from-ircam-odf odf-filepath :sample-rate sample-rate :weighted nil))
	 (bpm (.aref (mrr::read-ircambeat-bpm bpm-filepath) 0))
	 (odf-subset (mrr::subset-of-rhythm rhythm (list (round (* sample-rate start-from)) t))))
    ;; TODO beats-per-measure is derived from time signature. hardwired to 4/4 for now.
    ;; (mrr::downbeat-estimation-fixed odf-subset bpm '(2 2 2) 4))) ; Test the null hypothesis
    (mrr::downbeat-estimation-phase odf-subset bpm '(2 2 2) 4)))
    ;; (mrr::downbeat-estimation-duration odf-subset bpm '(2 2 2) 4)))
    ;; (mrr::downbeat-estimation-amplitude odf-subset bpm '(2 2 2) 4)))
    ;; (mrr::downbeat-estimation odf-subset bpm '(2 2 2) 4)))

;;; (ircam-find-downbeat #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/Quaero_excerpts/Analysis/0186 - Dillinger_excerpt.odf" #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/Quaero_excerpts/Analysis/0186 - Dillinger_excerpt.wav.bpm.xml")

(defun ircambeat-marker-of-downbeat (original-sound-path &key (analysis-directory *rhythm-data-directory*))
  "Given the original sound file, determine the downbeat, and find the marker produced by
  IRCAMbeat that matches it in time"
  (let* ((ircambeat-markers (read-ircam-marker-times (beat-marker-filepath original-sound-path 
						      :analysis-directory analysis-directory)))
	 (odf-file (odf-filepath original-sound-path :analysis-directory analysis-directory))
	 (bpm-file (bpm-filepath original-sound-path :analysis-directory analysis-directory))
	 (bpm (.aref (mrr::read-ircambeat-bpm bpm-file) 0))
	 (first-ircambeat-time (.aref ircambeat-markers 0))
	 (found-downbeat (ircam-find-downbeat odf-file bpm-file :start-from first-ircambeat-time))
	 ;; Since the downbeat is determined from an ODF that begins on the first IRCAMbeat
	 ;; marker, we assume the beat matches.
	 (nearest-marker (nearest-beat-to-downbeat ircambeat-markers found-downbeat bpm)))
    (format t "Found downbeat shifted by ~d beats, nearest ircambeat marker ~d, starts at ~,3f seconds~%" 
	    found-downbeat nearest-marker (.aref ircambeat-markers nearest-marker))
    found-downbeat))

;;; (ircambeat-marker-of-downbeat #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/Quaero_excerpts/Audio/0186 - Dillinger_excerpt.wav"  :analysis-directory #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/Quaero_excerpts/Analysis")

;;; take the ground truth downbeat locations and print out where they would be.
(defun groundtruth-downbeat-locations (original-sound-path annotation-path)
  (let* ((sample-rate 172.27d0)
	 (ircambeat-markers (read-ircam-marker-times (beat-marker-filepath original-sound-path)))
	 (first-ircambeat-time (.aref ircambeat-markers 0))
	 (last-ircambeat-time (nlisp::.last ircambeat-markers))
	 (downbeat-times (annotated-downbeats annotation-path))
	 (excerpt-downbeat-times (mrr::prune-to-limit downbeat-times last-ircambeat-time))
	 (downbeat-samples (.round (.* excerpt-downbeat-times sample-rate))))
    ;; Align to the measure windows
    (.- downbeat-samples (round (* first-ircambeat-time sample-rate)))))

;;; (groundtruth-downbeat-locations #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186 - Dillinger_excerpt.wav"
;;; #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186b - Dillinger - Cocaine - 01 Cocaine In My Brain.xml")
;;; Calculate the offset from the bar region
;;; (.- a (.* (.iseq 0 (.length a)) (* 106 4)))


(defun odf-markers-verify-first (original-sound-path)
  "Plots the first fragment of the ODF against the IRCAM beat marker positions"
  (let* ((fragment-duration 1000)
	 (odf-file (odf-filepath original-sound-path))
	 (odf (rhythm-from-ircam-odf odf-file :sample-rate 172.27))
	 (fragment (mrr::subset-of-rhythm odf (list 0 fragment-duration)))
	 (markers (read-ircam-marker-times (beat-marker-filepath original-sound-path)))
	 (marker-samples (.round (.* markers (sample-rate fragment))))
	 (fragment-marker-samples (.arefs marker-samples (.find (.< marker-samples fragment-duration)))))
    (window)
    (plot-rhythm fragment :time-in-seconds t)
    (close-window)
    (format t "markers in samples ~a~%" marker-samples)
    (window)
    (plot (list (mrr::time-signal fragment) 
		(.arefs (mrr::time-signal fragment) fragment-marker-samples))
	  (list (.iseq 0 (duration-in-samples fragment)) fragment-marker-samples)
	  :aspect-ratio 0.2
	  :styles '("lines" "points pointtype 2"))
    (close-window)))

(defun odf-annotation-verify-start (original-sound-path annotation-path)
  "Plots just the first fragment of the ODF against the annotated downbeat positions. Does this
  to verify we aren't doing something stupid with annotation times."
  (let* ((fragment-duration 1200)
	 (odf-file (odf-filepath original-sound-path))
	 (odf (rhythm-from-ircam-odf odf-file :sample-rate 172.27))
	 (fragment (mrr::subset-of-rhythm odf (list 0 fragment-duration)))
	 ;; These are the hand annotated downbeats
	 (annotated-downbeat-times (annotated-downbeats annotation-path))
	 (downbeat-samples (.round (.* annotated-downbeat-times (sample-rate odf))))
	 (fragment-downbeat-samples (mrr::prune-to-limit downbeat-samples fragment-duration)))
    (format t "annotated downbeats in samples ~a~%" fragment-downbeat-samples)
    (window)
    (plot-rhythm fragment :time-in-seconds t)
    (close-window)
    ;; (format t "Amplitude computed Onsets in seconds ~a~%" (mrr::onsets-in-seconds fragment))
    (window)
    (plot (list (mrr::time-signal fragment) 
		(.arefs (mrr::time-signal fragment) fragment-downbeat-samples))
	  (list (.iseq 0 (duration-in-samples fragment)) fragment-downbeat-samples)
	  :aspect-ratio 0.2
	  :styles '("lines" "points pointtype 3 linecolor 2")
	  :title (format nil "First fragment of ~a with annotated downbeats" (mrr::name fragment)))
    (close-window)))

;;;(odf-annotation-verify-start #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186 - Dillinger_excerpt.wav"  #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186b - Dillinger - Cocaine - 01 Cocaine In My Brain.xml")

;;; (odf-annotation-verify-start #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0144 - The Beatles - A Hard Days Night - 01 A Hard Day s Night.wav" #P"0144b - The Beatles - A Hard Days Night - 01 A Hard Day s Night.xml"

(defun verify-annotated-downbeats (original-sound-path annotation-path &key (odf-type "odf") (bar-search 2))
  "Plots 2 bar fragment of the ODF against the annotated downbeat positions"
  (let* ((beats-per-measure 4)
	 (tempo-in-bpm (.aref (mrr::read-ircambeat-bpm (bpm-filepath original-sound-path)) 0))
	 (odf-file (odf-filepath original-sound-path))
	 (odf (rhythm-from-ircam-odf odf-file :sample-rate 172.27))
	 (beat-duration (round (* (/ 60.0 tempo-in-bpm) (sample-rate odf)))) ; in samples
	 (bar-duration (* beats-per-measure beat-duration)) ; in samples
	 (search-duration (* bar-search bar-duration)) ; in samples
	 (number-of-measures (1- (* (floor (duration-in-samples odf) bar-duration))))
	 ;; These are the hand annotated downbeats
	 (annotated-downbeat-times (annotated-downbeats annotation-path))
	 (excerpt-downbeat-times (mrr::prune-to-limit annotated-downbeat-times (duration-in-samples odf)))
	 (downbeat-samples (.round (.* excerpt-downbeat-times (sample-rate odf)))))
    (format t "Beat duration ~d, bar duration ~d~%" beat-duration bar-duration)
    (loop
       for measure-index from 0 below (min number-of-measures 10)
       for start-sample from 0 below (duration-in-samples odf) by bar-duration
       for search-region = (list start-sample (1- (min (duration-in-samples odf) (+ start-sample search-duration))))
       for fragment-of-ODF = (mrr::subset-of-rhythm odf search-region)
       ;;    (fragment-marker-samples (.arefs marker-samples (.find (.< marker-samples fragment-duration)))))
       do
	 (format t "Downbeat for measure ~d, sample ~d, shift by ~d samples~%" measure-index 
				  (.aref downbeat-samples measure-index)
				  (- (.aref downbeat-samples measure-index) start-sample))
	 (window) 
	 (mrr::visualise-downbeat '(2 2 2) tempo-in-bpm fragment-of-ODF 
				  (- (.aref downbeat-samples measure-index) start-sample)
				  (1- bar-search))
	 (close-window))))

;;(verify-annotated-downbeats #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186 - Dillinger_excerpt.wav"  #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186b - Dillinger - Cocaine - 01 Cocaine In My Brain.xml")

;;; Examine LP version
;;(verify-annotated-downbeats #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186 - Dillinger_excerpt.wav"  #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186b - Dillinger - Cocaine - 01 Cocaine In My Brain.xml" :odf-type "lp.odf")

;;(verify-annotated-downbeats #P"/Volumes/Quaerodb/annotation/wav-22km/0245 - The Beatles - Magical Mystery Tour - 10 Baby Youre A Rich Man.wav" #P"/Volumes/Quaerodb/annotation//0245 - The Beatles - Magical Mystery Tour - 10 Baby Youre A Rich Man.b_q.xml")

;; (make-pathname :defaults *quaero-annotations-directory* 
;; 	       :name (concatenate 'string (first (track-named "0245" full-quaero)) ".b_q")
;; 	       :type "xml")

(defun verify-ircambeat-downbeat-markers (original-sound-path anacrusis)
  "Plots 2 bar fragment of the ODF against the IRCAM beat _generated_ marker positions"
  (let* ((beats-per-measure 4)
	 (odf-file (odf-filepath original-sound-path))
	 (tempo-in-bpm (.aref (mrr::read-ircambeat-bpm (bpm-filepath original-sound-path)) 0))
	 (odf (rhythm-from-ircam-odf odf-file :sample-rate 172.27))
	 (beat-duration (round (* (/ 60.0 tempo-in-bpm) (sample-rate odf)))) ; in samples
	 (bar-duration (* beats-per-measure beat-duration)) ; in samples
	 (bar-search 2) ; Number of bars to search over.
	 (search-duration (* bar-search bar-duration)) ; in samples
	 (number-of-measures (1- (* (floor (duration-in-samples odf) bar-duration))))
 	 (markers (read-ircam-marker-times (beat-marker-filepath original-sound-path)))
 	 (downbeat-marker-indices (.+ (.* (.iseq 0 (1- number-of-measures)) beats-per-measure) anacrusis))
 	 (downbeat-samples (.round (.* (.arefs markers downbeat-marker-indices) (sample-rate odf)))))
    (format t "Beat duration ~d, bar duration ~d~%" beat-duration bar-duration)
    (loop
       for measure-index from 0 below number-of-measures
       for start-sample from 0 below (duration-in-samples odf) by bar-duration
       for search-region = (list start-sample (1- (min (duration-in-samples odf) (+ start-sample search-duration))))
       for fragment-of-ODF = (mrr::subset-of-rhythm odf search-region)
       ;;    (fragment-marker-samples (.arefs marker-samples (.find (.< marker-samples fragment-duration)))))
       do
	 (format t "Downbeat for measure ~d, sample ~d, shift by ~d samples~%" measure-index 
				  (.aref downbeat-samples measure-index)
				  (- (.aref downbeat-samples measure-index) start-sample))
	 (window) 
	 (mrr::visualise-downbeat '(2 2 2) tempo-in-bpm fragment-of-ODF 
				  (- (.aref downbeat-samples measure-index) start-sample))
	 (close-window))))

;;(verify-ircambeat-downbeat-markers #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186 - Dillinger_excerpt.wav" 1)
;; (verify-ircambeat-downbeat-markers #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_89_excerpt.wav" 3)

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
			   :analysis-directory (merge-pathnames (make-pathname :directory '(:relative "Quaero"))
								*rhythm-data-directory*)
			   :audio-directory *quaero-audio-directory*))

(defun annotated-anacrusis (ircam-annotation-path ircambeat-marker-path)
  "Returns the beat location of the ircambeat marker nearest the first annotated downbeat"
  (let* ((annotated-downbeat-times (annotated-downbeats ircam-annotation-path))
	 (ircambeat-markers (read-ircam-marker-times ircambeat-marker-path)))
;;    (format t "downbeat times ~a~%first ircambeat markers ~a~%" 
;;	    (.subseq annotated-downbeat-times 0 4) (.subseq ircambeat-markers 0 6))
    (nearest-beat-to-time (.aref annotated-downbeat-times 0) ircambeat-markers)))

(defun make-quaero-dataset (max every)
  "Generate a list of entries suitables for testing evaluate-quaero-downbeat with"
  (loop
     with annotation-files = (cl-fad:list-directory *quaero-annotations-directory*)
     with analysis-directory = (merge-pathnames (make-pathname :directory '(:relative "Quaero")) *rhythm-data-directory*)
     with anacrusis
     for song-index from 0 below max by every
     for annotation-pathname = (nth song-index annotation-files)
     for annotation-name = (pathname-name (pathname-name annotation-pathname))
     for ircambeat-marker-pathname = (beat-marker-filepath (make-pathname :defaults annotation-name :type "wav")
							   :analysis-directory analysis-directory)
     do (format t "~a~%" (pathname-name annotation-pathname))
     when (cl-fad:file-exists-p ircambeat-marker-pathname)
     when (< (setf anacrusis (annotated-anacrusis annotation-pathname ircambeat-marker-pathname)) 6)
     collect (list annotation-name :anacrusis anacrusis)))

(defun remove-bad-anacruses (data-set)
  "Remove those anacruses which are excessively long because IRCAMbeat starts clapping
  well before the first downbeat"
  (remove-if (lambda (x) (> x 6)) data-set :key #'third))

;; (setf small-quaero (make-quaero-dataset 60 3))
;; (setf half-quaero (make-quaero-dataset 250 2))
;; (setf bad-examples (evaluate-with-music #'evaluate-quaero-downbeat :music-dataset half-quaero :music-name #'first))

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


