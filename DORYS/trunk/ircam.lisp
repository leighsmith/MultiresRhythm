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

(in-package :dorys)
(use-package :nlisp)
(use-package :multires-rhythm)

(defparameter *rwc-directory* "/Volumes/iDisk/Research/Data/IRCAM-Beat/")

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

(defun beat-marker-filepath (original-sound-path)
  "Returns the filepath of the beat marker file associated with the given sound file"
  (make-pathname :directory (pathname-directory original-sound-path)
		 :name (concatenate 'string (file-namestring original-sound-path) ".markers")
		 :type "xml"))

(defun bpm-filepath (original-sound-path)
  "Returns the filepath of the tempo file associated with the given sound file"
  (make-pathname :directory (pathname-directory original-sound-path)
		 :name (concatenate 'string (file-namestring original-sound-path) ".bpm")
		 :type "xml"))

(defun read-ircam-marker-times (beat-marker-filepath)
  (let* ((clap-times-in-seconds (multires-rhythm::read-ircam-beat-markers beat-marker-filepath)))
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

(defun sonify-ircam-beat (original-sound-path &key (directory "/Volumes/iDisk/Research/Data/IRCAM-Beat")
			  (anacrusis 0) (sound-every 1) 
			  (clap-sample-file #P"/Volumes/iDisk/Research/Data/Handclap Examples/cowbell.aiff"))
  "Write out a sound file mixed with downbeats"
  (let* ((accompaniment-sound-path (make-pathname :directory directory
						  :name (concatenate 'string (pathname-name original-sound-path) "_mixed")
						  :type "wav"))
	 (beat-marker-path (beat-marker-filepath original-sound-path)))
    (multires-rhythm::save-rhythm-mix accompaniment-sound-path 
				      original-sound-path 
				      (downbeat-times beat-marker-path :anacrusis anacrusis :sound-every sound-every)
				      :clap-sample-file clap-sample-file)
    (format t "Wrote mix as soundfile ~a~%" accompaniment-sound-path)))

;; (setf ircam-beat-markers (read-ircam-beat-markers #P"/Local/Users/leigh/Research/Sources/Rhythm/IRCAM/LeighsTests/res4_1.wav.markers.xml"))
;; (setf mrr-markers (make-narray '(0.1d0 0.69d0 1.26d0 1.8d0 2.325d0 2.865d0 3.445d0 4.155d0 4.855d0  5.52d0 6.165d0 6.795d0 7.425d0 8.025d0 8.58d0 9.125d0)))
;; (nplot (list (.diff ircam-beat-markers) (.diff mrr-markers)) nil :styles '("linespoints" "linespoints") :aspect-ratio 0.66)

;; (setf res4_2-ircam-beat-markers (read-ircam-beat-markers #P"/Local/Users/leigh/Research/Sources/Rhythm/IRCAM/LeighsTests/res4_2.wav.markers.xml"))
;; (setf res4_2-mrr-markers (make-narray '(0.475d0 0.985d0 1.54d0 2.2d0 2.85d0 3.495d0 4.145d0 4.825d0 5.56d0
;;         6.285d0 6.985d0 7.695d0 8.38d0 9.04d0 9.69d0 10.325d0 10.93d0 11.51d0)))
;; (nplot (list (.diff res4_2-ircam-beat-markers) (.diff res4_2-mrr-markers)) nil :styles '("linespoints" "linespoints") :aspect-ratio 0.66)

#|
(sonify-ircam-beat #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_95_excerpt.wav")

(setf rwc95-times (.load #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc95_times.txt" :format :text))
(setf sample-rate (/ 1 (mean (.diff (.column rwc95-times 0)))))
(setf rwc95-odf (.load #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_95.odf" :format :text))
(setf rwc95-onsets (.load #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_95.onsets" :format :text))
(plot (.column rwc95-odf 0) nil :aspect-ratio 0.2)
(plot (make-double-array (.length rwc95-onsets) :initial-element 1.0d0) (.column rwc95-onsets 0) :style "impulses" :aspect-ratio 0.2)


(pushnew 'cwt+skeleton  *plotting*)

(multires-rhythm::clap-to-salience-rhythm-files  
 #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_95.odf"
 #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_95.onsets" 
 #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_95_excerpt.wav"
 #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_95_excerpt_mixed_mrr.wav"
:sample-rate 172.27 :beat-multiple 1)
|#

(defun sonify-rwc-example (rwc-example)
  (sonify-ircam-beat (make-pathname :directory *rwc-directory* :name (first rwc-example) :type "wav") 
		     :anacrusis (third rwc-example) 
		     :sound-every 4)) ; TODO we've fixed this as 4/4, needs fixing.

(defun count-rwc-example (rwc-example)
  (.length (downbeat-times (make-pathname :directory *rwc-directory* :name (first rwc-example) :type "marker.xml") 
			   :anacrusis (third rwc-example) 
			   :sound-every 4))) ; TODO we've fixed this as 4/4, needs fixing.

;;; (mapcar #'sonify-rwc-example *ircam-downbeats*)
;;; (sonify-rwc-example (nth 0 *ircam-downbeats*))

;;; Find the average number of downbeats in each example
;;; (mean (.* (make-narray (mapcar #'count-rwc-example *ircam-downbeats*)) 1d0))
;;; Find the total number of downbeats.
;;; (.sum (make-narray (mapcar #'count-rwc-example *ircam-downbeats*)))


;;; (sonify-ircam-beat #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_99_excerpt.wav")

;;;
(defun annotated-downbeats (ircam-annotation-path)
  "Retrieve the times of nominated downbeats from an annotation file"
  (multiple-value-bind (times beats) 
      (mrr::read-ircam-annotation ircam-annotation-path)
    (let* ((downbeat-indices (.find (.= beats 1)))
	   (downbeats (.arefs times downbeat-indices)))
      (format t "downbeat indices ~a~%" downbeat-indices)
      downbeats)))


(defun sonify-ircam-annotation (ircam-annotation-path original-sound-path accompaniment-directory
				&key (clap-sample-file #P"/Volumes/iDisk/Research/Data/Handclap Examples/cowbell.aiff"))
  "Make the downbeat annotations audible"
  (let* ((downbeats (annotated-downbeats ircam-annotation-path))
	 (accompaniment-sound-path (make-pathname :directory accompaniment-directory
						  :name (concatenate 'string (pathname-name original-sound-path) "_mixed")
						  :type "wav")))
    (multires-rhythm:save-rhythm-mix accompaniment-sound-path 
				     original-sound-path 
				     downbeats
				     :clap-sample-file clap-sample-file)
    (format t "Wrote mix as soundfile ~a~%" accompaniment-sound-path)))


;;; The symlink seems to not work over AFP so that this path is unable to be retrieved by POSIX calls.
;;; annotations-directory #P"/Volumes/Quaerodb/annotation/annotation current state/"
;;; TODO need to check if the audio is 22KHz or 44.1KHz to select the correct version of the accompaniment.
(defun sonify-quaero (annotation-pathspec 
		      &key (annotations-directory #P"/Volumes/Quaerodb/doc/quaero_site/content/18_current/")
		      (audio-directory #P"/Volumes/Quaerodb/annotation-wav-m22k/"))
  "Creates a sonification from the annotation-pathspec, which can have a relative directory component"
  (let* ((annotation-name (pathname-name annotation-pathspec))
	 (first-b (position #\b annotation-name))
	 (audio-name (concatenate 'string 
				  (subseq annotation-name 0 first-b)
				  (subseq annotation-name (1+ first-b)))))
  (sonify-ircam-annotation 
   (merge-pathnames (make-pathname :defaults annotation-pathspec :type "xml") 
		    annotations-directory)
   (merge-pathnames audio-directory
		    (make-pathname :directory (pathname-directory annotation-pathspec)
				   :name audio-name
				   :type "wav"))
   "/Volumes/iDisk/Research/Data/IRCAM-Beat/Annotation"
   :clap-sample-file #P"/Volumes/iDisk/Research/Data/Handclap Examples/cowbell_22kHz_mono.aiff")))

#|
(defun nearest-beat-to-downbeat (beat-markers downbeat tempo-in-bpm &key (start-from 0))
  "Finds the beats computed by IRCAMbeat which are closest to the chosen downbeat location"
  (let* ((beat-duration (/ 60.0d0 tempo-in-bpm))
	 (downbeat-in-seconds (+ (* beat-duration downbeat) start-from))
	 (distance-from-markers (.abs (.- beat-markers downbeat-in-seconds)))
	 (nearest (argmin distance-from-markers)))
    (format t "Downbeat estimated to be located at ~,3f seconds into the sound for ~,2f bpm~%"
	    downbeat-in-seconds tempo-in-bpm)
    ;; (format t "distance from beat-markers ~a~%" distance-from-markers)
    ;; check the markers are too far away from the downbeat.
    (if (< (.aref distance-from-markers nearest) beat-duration)
	(format t "ERROR: beat marker too far away from estimated downbeat position~%"))
    nearest))
|#

;;; Since the downbeat is determined from an ODF that begins on the first IRCAMbeat
;;; marker, we assume the beat matches.
(defun nearest-beat-to-downbeat (beat-markers downbeat tempo-in-bpm)
  "Finds the beats computed by IRCAMbeat which are closest to the chosen downbeat location"
  (declare (ignore beat-markers tempo-in-bpm))
  downbeat)

(defun ircam-find-downbeat (odf-filepath bpm-filepath &key (start-from 0.0d0) (sample-rate 172.27d0))
  "Given the files, compute the downbeat"
  (let* ((rhythm (rhythm-from-ircam-odf odf-filepath :sample-rate sample-rate :weighted nil))
	 (bpm (.aref (mrr::read-ircam-beat-bpm bpm-filepath) 0))
	 (odf-subset (mrr::subset-of-rhythm rhythm (list (round (* sample-rate start-from)) t))))
    ;; beats-per-measure is derived from time signature.
    ;; TODO hardwired to 4/4 for now.
    (mrr::downbeat-estimation odf-subset bpm '(2 2 2) 4)))

;;; (ircam-find-downbeat #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186 - Dillinger_excerpt.odf" #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186 - Dillinger_excerpt.wav.bpm.xml")

(defun ircam-beat-marker-of-downbeat (original-sound-path)
  "Given the original sound file, determine the downbeat, and find the marker produced by
  IRCAMbeat that matches it in time"
  (let* ((ircam-beat-markers (read-ircam-marker-times (beat-marker-filepath original-sound-path)))
	 (odf-file (make-pathname :defaults original-sound-path :type "odf"))
	 (bpm-file (bpm-filepath original-sound-path))
	 (bpm (.aref (mrr::read-ircam-beat-bpm bpm-file) 0))
	 (first-ircam-beat-time (.aref ircam-beat-markers 0))
	 (found-downbeat (ircam-find-downbeat odf-file bpm-file :start-from first-ircam-beat-time))
	 ;; (found-downbeat (ircam-find-downbeat odf-file bpm-file :start-from 0))
	 (nearest-marker (nearest-beat-to-downbeat ircam-beat-markers found-downbeat bpm)))
    (format t "Found downbeat shifted by ~d beats, nearest ircam-beat marker ~d~%" found-downbeat nearest-marker)
    (format t "From BPM, should be ~,3f seconds~%" (+ (* found-downbeat (/ 60.0 bpm)) first-ircam-beat-time))
    (format t "From ircambeat markers (search from first):~%~,3a~%" (.subseq ircam-beat-markers 0 5))
    (format t "Starting at time ~,3f seconds~%" (.aref ircam-beat-markers nearest-marker))
    nearest-marker))


;;; take the ground truth downbeat locations and print out where they would be.
(defun groundtruth-downbeat-locations (original-sound-path annotation-path)
  (let* ((sample-rate 172.27d0)
	 (ircam-beat-markers (read-ircam-marker-times (beat-marker-filepath original-sound-path)))
	 (first-ircam-beat-time (.aref ircam-beat-markers 0))
	 (last-ircam-beat-time (nlisp::.last ircam-beat-markers))
	 (downbeat-times (annotated-downbeats annotation-path))
	 (excerpt-downbeat-times (mrr::prune-to-limit downbeat-times last-ircam-beat-time))
	 (downbeat-samples (.round (.* excerpt-downbeat-times sample-rate))))
    ;; Align to the measure windows
    (.- downbeat-samples (round (* first-ircam-beat-time sample-rate)))))

;;; (ircam-beat-marker-of-downbeat #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186 - Dillinger_excerpt.wav")

;;; (ircam-beat-marker-of-downbeat #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_81_excerpt.wav")
;;; (ircam-beat-marker-of-downbeat #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_89_excerpt.wav")

;;; (groundtruth-downbeat-locations #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186 - Dillinger_excerpt.wav"
;;; #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186b - Dillinger - Cocaine - 01 Cocaine In My Brain.xml")

(defun odf-markers-verify-first (original-sound-path)
  "Plots the first fragment of the ODF against the IRCAM beat marker positions"
  (let* ((fragment-duration 1000)
	 (odf-file (make-pathname :defaults original-sound-path :type "odf"))
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
	 (odf-file (make-pathname :defaults original-sound-path :type "odf"))
	 (odf (rhythm-from-ircam-odf odf-file :sample-rate 172.27))
	 (fragment (mrr::subset-of-rhythm odf (list 0 fragment-duration)))
	 ;; These are the hand annotated downbeats
	 (annotated-downbeat-times (annotated-downbeats annotation-path))
	 (downbeat-samples (.round (.* annotated-downbeat-times (sample-rate odf))))
	 (fragment-downbeat-samples (mrr::prune-to-limit downbeat-samples fragment-duration)))
    (format t "annotated downbeats in samples ~a~%" fragment-downbeat-samples)
    (window)
    (plot (list (mrr::time-signal fragment) 
		(.arefs (mrr::time-signal fragment) fragment-downbeat-samples))
	  (list (.iseq 0 (duration-in-samples fragment)) fragment-downbeat-samples)
	  :aspect-ratio 0.2
	  :styles '("lines" "points pointtype 3 linecolor 2"))
    (close-window)))

;;;(odf-annotation-verify-start #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186 - Dillinger_excerpt.wav"  #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186b - Dillinger - Cocaine - 01 Cocaine In My Brain.xml")


(defun odf-annotation-verify (original-sound-path annotation-path)
  "Plots the first fragment of the ODF against the IRCAM beat marker positions"
  (let* ((beats-per-measure 4)
	 (odf-file (make-pathname :defaults original-sound-path :type "odf"))
	 (tempo-in-bpm (.aref (mrr::read-ircam-beat-bpm (bpm-filepath original-sound-path)) 0))
	 (odf (rhythm-from-ircam-odf odf-file :sample-rate 172.27))
	 (beat-duration (round (* (/ 60.0 tempo-in-bpm) (sample-rate odf)))) ; in samples
	 (bar-duration (* beats-per-measure beat-duration)) ; in samples
	 (bar-search 2) ; Number of bars to search over.
	 (search-duration (* bar-search bar-duration)) ; in samples
	 (number-of-measures (1- (* (floor (duration-in-samples odf) bar-duration))))
	 ;; These are the hand annotated downbeats
	 (annotated-downbeat-times (annotated-downbeats annotation-path))
	 (excerpt-downbeat-times (mrr::prune-to-limit annotated-downbeat-times (duration-in-samples odf)))
	 (downbeat-samples (.round (.* excerpt-downbeat-times (sample-rate odf)))))
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

;; (defun verify-ircambeat-downbeat-markers (original-sound-path anacrusis)
;;   ;; These are the IRCAMbeat _generated_ markers.
;;   (let* ((beats-per-measure 4)
;; 	 (markers (read-ircam-marker-times (beat-marker-filepath original-sound-path)))
;; 	 (downbeat-marker-indices (.+ (.* (.iseq 0 (1- number-of-measures)) beats-per-measure) anacrusis))
;; 	 (downbeat-samples (.round (.* (.arefs markers downbeat-marker-indices) (mrr::sample-rate odf))))
;;   )))

;; (defun verify-annotated-downbeats (original-sound-path)
;;   )

;;(odf-annotation-verify #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186 - Dillinger_excerpt.wav"  #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186b - Dillinger - Cocaine - 01 Cocaine In My Brain.xml")

(defun sonify-downbeat-estimate (original-sound-file)
  "Sonify just the downbeats as estimated by the downbeat finder"
  (sonify-ircam-beat original-sound-file
		     :anacrusis (ircam-beat-marker-of-downbeat original-sound-file)
		     :sound-every 4  ; TODO we've fixed this as 4/4, needs correcting.
		     :clap-sample-file 
		     (case (nlisp::audio-parameters original-sound-file)
		       (22050 #P"/Volumes/iDisk/Research/Data/Handclap Examples/cowbell_22kHz_mono.aiff")
		       (44100 #P"/Volumes/iDisk/Research/Data/Handclap Examples/cowbell.aiff"))))

(defun sonify-rwc-evaluation (rwc-example)
  (sonify-downbeat-estimate (make-pathname :directory *rwc-directory* :name (first rwc-example) :type "wav")))

;;;
;;; Evaluate the downbeat finder.
;;;

(defun evaluate-ircam-downbeat (ircam-example &key (directory *rwc-directory*))
  "Evaluates the downbeat finder against the annotated RWC excerpt"
  (let* ((rwc-filename (first ircam-example))
	 (rwc-anacrusis (third ircam-example))
	 (sound-file (make-pathname :directory directory :name rwc-filename :type "wav"))
	 (found-downbeat (ircam-beat-marker-of-downbeat sound-file)))
    (format t "For ~a Found downbeat ~a vs. ground truth ~a~%" rwc-filename found-downbeat rwc-anacrusis)
    (= found-downbeat rwc-anacrusis)))

#|


(setf bad-examples (evaluate-with-music #'evaluate-ircam-downbeat :music-dataset *ircam-downbeats* :music-name #'first))

(sonify-downbeat-estimate #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186 - Dillinger_excerpt.wav")

(sonify-downbeat-estimate #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0051 - Buenavista_excerpt.wav")

(sonify-downbeat-estimate #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_81_excerpt.wav")

(sonify-ircam-beat #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186 - Dillinger_excerpt.wav"
		   :clap-sample-file #P"/Volumes/iDisk/Research/Data/Handclap Examples/cowbell_22kHz_mono.aiff")

(sonify-ircam-beat #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0051 - Buenavista_excerpt.wav"
		   :clap-sample-file #P"/Volumes/iDisk/Research/Data/Handclap Examples/cowbell_22kHz_mono.aiff")


(sonify-quaero #P"beat_XML_from_QIMAv1/0144b - The Beatles - A Hard Days Night - 01 A Hard Day s Night")

(sonify-quaero #P"beat_XML_from_logic/0186b - Dillinger - Cocaine - 01 Cocaine In My Brain")

(sonify-quaero #P"beat_XML_from_protools/0051b - Buenavista social club - Buenavista social club - Chan chan")

(sonify-quaero #P"beat_XML_from_protools/0043b - Ali Farka Toure - Ali Farka Toure - Amandrai")

(sonify-quaero #P"beat_XML_from_protools/0202b - Squarepusher - Hard Normal Daddy - Beep Street")

;; Sonification of Local files
(sonify-quaero #P"0186b - Dillinger - Cocaine - 01 Cocaine In My Brain" 
	       :annotations-directory #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/"
	       :audio-directory #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/")

(sonify-quaero #P"0144b - The Beatles - A Hard Days Night - 01 A Hard Day s Night.xml"
	       :annotations-directory #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/"
	       :audio-directory #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/")

(sonify-quaero #P"0051b - Buenavista social club - Buenavista social club - Chan chan.xml"
	       :annotations-directory #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/"
	       :audio-directory #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/")

|#

