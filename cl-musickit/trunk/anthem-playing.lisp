;; (require 'cl-musickit)
;; (require 'multiresrhythm)

(defun note-list-of-rhythm (rhythm-to-play)
  (let* ((play-times-ms (.floor (.* (onsets-in-seconds rhythm-to-play) 1000)))
	 (note-tag 0))
    (map 'list (lambda (onset-time) 
		 (cl-musickit::make-midi-note onset-time 
					      (incf note-tag) 
					      :key-number cl-musickit::*low-woodblock*
					      :midi-channel 9))
	 (val play-times-ms))))

;; (cl-musickit:play-timed-notes (note-list-of-rhythm (anthem-rhythm (anthem-named 'dorys::america))))

(defun note-list-of-anthem (anthem-to-play &key (tempo-in-bpm 60))
  (let* ((shortest-interval-milliseconds (/ 60000 tempo-in-bpm (anthem-beat-duration anthem-to-play)))
	 (intervals-ms (mapcar (lambda (x) (* shortest-interval-milliseconds x)) (second anthem-to-play)))
	 (play-times-ms (iois-to-onsets intervals-ms))
	 (note-tag 0))
    (mapcar (lambda (onset-time) 
	      (cl-musickit::make-midi-note onset-time 
					   (incf note-tag) 
					   :key-number cl-musickit::*low-woodblock*
					   :midi-channel 9))
	    play-times-ms)))

;; (cl-musickit:play-timed-notes (note-list-of-anthem (anthem-named 'dorys::america) :tempo-in-bpm 120))
;; (cl-musickit:play-timed-notes (note-list-of-anthem (anthem-named 'dorys::australia) :tempo-in-bpm 120))

