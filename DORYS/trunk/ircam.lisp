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

(defparameter *ircam-downbeats*
  '(("rwc_p_81_excerpt" :anacrusis 1)
;    ("rwc_p_82_excerpt" :anacrusis 0) ; There is an anacrusis which IRCAM beat skips
    ("rwc_p_83_excerpt" :anacrusis 0)
    ("rwc_p_84_excerpt" :anacrusis 0)
;    ("rwc_p_85_excerpt" :anacrusis 0) ; THis isn't correct, off by a quaver
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

(defun read-ircam-marker-times (original-sound-path)
  (let* ((beat-marker-filepath (make-pathname :directory (pathname-directory original-sound-path)
 					      :name (concatenate 'string (file-namestring original-sound-path) ".markers")
 					      :type "xml"))
	 (clap-times-in-seconds (multires-rhythm::read-ircam-beat-markers beat-marker-filepath)))
    ;; Remove the clap times that are negative (!)
   (.arefs clap-times-in-seconds (.find (.> clap-times-in-seconds 0.0d0)))))

(defun downbeats-of-times (times anacrusis beats-per-measure)
  "Returns those times which occur on the downbeat, i.e once every measure, with an
  anacrusis (in notes). The anacrusis indicates the start location, not the number of preceding notes."
  (let ((downbeat-indices (.+ (.* (.iseq 0 (floor (- (.length times) anacrusis 1) beats-per-measure)) beats-per-measure) anacrusis)))
    (format t "downbeat indices ~a~%" downbeat-indices)
    (.arefs times downbeat-indices)))

(defun sonify-ircam-beat (original-sound-path &key (directory "/Volumes/iDisk/Research/Data/IRCAM-Beat")
			  (anacrusis 0) (sound-every 1))
  (let* ((accompaniment-sound-path (make-pathname :directory directory
						  :name (concatenate 'string (pathname-name original-sound-path) "_mixed")
						  :type "wav"))
	 (clap-times-in-seconds (read-ircam-marker-times original-sound-path)))
    (multires-rhythm::save-rhythm-mix accompaniment-sound-path 
				      original-sound-path 
				      (downbeats-of-times clap-times-in-seconds anacrusis sound-every)
				      :clap-sample-file #P"/Volumes/iDisk/Research/Data/Handclap Examples/cowbell.aiff")
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

(defun ircam-find-downbeat (odf-filepath bpm-filepath)
  (let ((rwc-rhythm (multires-rhythm::rhythm-from-odf odf-filepath :sample-rate 172.27 :weighted nil))
	(rwc-bpm (.aref (mrr::read-ircam-beat-bpm bpm-filepath) 0)))
    ;; beats-per-measure is derived from time signature.
    (mrr::downbeat-estimation rwc-rhythm rwc-bpm '(2 2 2) 4)))

(defun evaluate-ircam-downbeat (ircam-example)
  "Evaluates the downbeat finder against the annotated RWC excerpt"
  (let* ((filename (first ircam-example))
	 (anacrusis (third ircam-example))
	 (rwc-rhythm-odf (make-pathname :directory *rwc-directory* :name filename :type "odf"))
	 (rwc-bpm (make-pathname :directory *rwc-directory* 
				 :name (format nil "~a.wav.bpm" filename)
				 :type "xml"))
	 (found-downbeat (ircam-find-downbeat rwc-rhythm-odf rwc-bpm)))
    (format t "For ~a Found downbeat ~a vs. ground truth ~a~%" filename found-downbeat anacrusis)
    (= found-downbeat anacrusis)))

(defun sonify-rwc-example (rwc-example)
  (sonify-ircam-beat (make-pathname :directory *rwc-directory* :name (first rwc-example) :type "wav") 
		     :anacrusis (third rwc-example) 
		     :sound-every 4)) ; TODO we've fixed this as 4/4, needs fixing.

;;; (setf bad-examples (evaluate-with-music #'evaluate-ircam-downbeat :music-dataset *ircam-downbeats* :music-name #'first))
;;; (mapcar #'sonify-rwc-example *ircam-downbeats*)
;;; (sonify-rwc-example (nth 0 *ircam-downbeats*))

;;; (sonify-ircam-beat #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_99_excerpt.wav")

(defun sonify-ircam-annotation (ircam-annotation-path original-sound-path accompaniment-directory
				&key (clap-sample-file #P"/Volumes/iDisk/Research/Data/Handclap Examples/cowbell.aiff"))
  (multiple-value-bind (times beats) 
      (mrr::read-ircam-annotation ircam-annotation-path)
    ;; (downbeats-of-times times (position 1 (val beats)) (.max beats))
    (let ((downbeats (.arefs times (.find (.= beats 1))))
	  (accompaniment-sound-path (make-pathname :directory accompaniment-directory
						   :name (concatenate 'string (pathname-name original-sound-path) "_mixed")
						   :type "wav")))
      (multires-rhythm:save-rhythm-mix accompaniment-sound-path 
				       original-sound-path 
				       downbeats
				       :clap-sample-file clap-sample-file)
      (format t "Wrote mix as soundfile ~a~%" accompaniment-sound-path))))


;;; The symlink seems to not work over AFP so that this path is unable to be retrieved by POSIX calls.
;;;(defparameter *ircam-annotations-directory* "/Volumes/Quaerodb/annotation/annotation current state/beat_XML_from_QIMAv1")

(defparameter *ircam-annotations-directory* "/Volumes/Quaerodb/doc/quaero_site/content/18_current/beat_XML_from_QIMAv1")

(defparameter *ircam-audio-directory* "/Volumes/Quaerodb/annotation-wav")

(defun sonify-quaero (annotation-name audio-name)
  (sonify-ircam-annotation 
   (make-pathname :directory *ircam-annotations-directory* 
		  :name annotation-name
		  :type "xml" )
   (make-pathname :directory *ircam-audio-directory*
		  :name audio-name
		  :type "wav")
   "/Volumes/iDisk/Research/Data/IRCAM-Beat/Annotation"))

#|
(sonify-quaero "0144b - The Beatles - A Hard Days Night - 01 A Hard Day s Night" 
	       "0144 - The Beatles - A Hard Days Night - 01 A Hard Day s Night")
|#

;; Sonification of Local files
;; (sonify-ircam-annotation 
;; #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186b - Dillinger - Cocaine - 01 Cocaine In My Brain.xml" 
;; #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0186 - Dillinger - Cocaine - 01 Cocaine In My Brain.wav"  
;; "/Volumes/iDisk/Research/Data/IRCAM-Beat/Annotation")

(sonify-ircam-annotation 
 #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0144b - The Beatles - A Hard Days Night - 01 A Hard Day s Night.xml"
 #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/0144 - The Beatles - A Hard Days Night - 01 A Hard Day s Night.wav" 
 "/Volumes/iDisk/Research/Data/IRCAM-Beat/Annotation" 
 :clap-sample-file #P"/Volumes/iDisk/Research/Data/Handclap Examples/cowbell_22kHz_mono.aiff")

