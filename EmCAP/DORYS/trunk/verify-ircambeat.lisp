
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

;;; (groundtruth-downbeat-locations #P"/Users/lsmith/Research/Data/IRCAM-Beat/0186 - Dillinger_excerpt.wav"
;;; #P"/Users/lsmith/Research/Data/IRCAM-Beat/0186b - Dillinger - Cocaine - 01 Cocaine In My Brain.xml")
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
	 (tempo-in-bpm (ircambeat-computed-bpm (bpm-filepath original-sound-path)))
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
	 (tempo-in-bpm (ircambeat-computed-bpm (bpm-filepath original-sound-path)))
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

;;(verify-ircambeat-downbeat-markers #P"/Users/lsmith/Research/Data/IRCAM-Beat/Quaero_Excerpts/0186 - Dillinger_excerpt.wav" 1)
;;(verify-ircambeat-downbeat-markers #P"/Users/lsmith/Research/Data/IRCAM-Beat/rwc_p_89_excerpt.wav" 3)

