;;;; -*- Lisp -*-
;;;; $Id: playing.lisp 236 2007-03-09 14:21:51Z leigh $
;;;; Minimal rhythm playing functions interfacing to portmidi.
;;;;
;;;; Leigh M. Smith <lsmith@science.uva.nl>
;;;;

(in-package :cl-musickit)

;; Passing in the play-time-datum allows decoupling the creation of the event buffer from the time of
;; playing, so the creation can be slow.
(defmethod set-pm-event (play-time-datum (note-to-play note) pm-note-on-event pm-note-off-event)
  "Convert select parameters of a note into portmidi elements for playing"
  (let ((param (parameters note-to-play)))
    (case (note-type note-to-play)
      (:note-duration
       (pm:Event.message pm-note-on-event (pm:message (+ #b10010000 (gethash 'midi-channel param))
						   (gethash 'key-number param)
						   (gethash 'velocity param)))
       (pm:Event.timestamp pm-note-on-event (+ play-time-datum (play-time note-to-play)))
       (pm:Event.message pm-note-off-event (pm:message (+ #b10000000 (gethash 'midi-channel param))
						    (gethash 'key-number param)
						    0))
       (pm:Event.timestamp pm-note-off-event (+ play-time-datum (play-time note-to-play) (gethash 'duration param))))
      (:note-on
       (pm:Event.message pm-note-on-event (pm:message (+ #b10010000 (gethash 'midi-channel param))
						   (gethash 'key-number param)
						   (gethash 'velocity param)))
       (pm:Event.timestamp pm-note-on-event (+ play-time-datum (play-time note-to-play))))
      (:note-off			; Since this is a separate note-off, we modify the pm-note-on-event.
       (pm:Event.message pm-note-on-event (pm:message (+ #b10000000 (gethash 'midi-channel param))
						   (gethash 'key-number param)
						   0))
       (pm:Event.timestamp pm-note-on-event (+ play-time-datum (play-time note-to-play)))))))

(defun number-of-events (notes)
  "Returns the number of events given the types of notes"
  ;; on-off-pairs + single-events = double for note-on/off pairs.
  (+ (count :note-duration notes :key #'note-type) (length notes)))

;;TODO (event-times (iois-to-onsets intervals (pm:time)))
(defun create-event-buffer (notes)
  "Given a time ordered list of note objects, create and return a portmidi event buffer"
  (let* ((event-count (number-of-events notes)) 
	 (event-buffer (pm:EventBufferNew event-count))
	 (play-now-time (pm:time)))
    (loop				; assign the buffer's note on and off events.
       for note in notes
       for event-index = 0 then (+ event-index 2)
       do
	 (set-pm-event play-now-time 
		       note
		       (pm:EventBufferElt event-buffer event-index)
		       (pm:EventBufferElt event-buffer (1+ event-index))))
    (values event-buffer event-count)))

(let ((output-device-id nil))

  ;; initialize portmidi lib
  (defun enable-playing ()
    (pm:portmidi)
    (setf output-device-id (pm:GetDefaultOutputDeviceID))
    (pm:GetDeviceInfo output-device-id))

  ;; to get available playing devices
  ;; (pm:CountDevices)
  ;; (pm:GetDeviceInfo))

  (defun play-timed-notes (midi-notes)
    "Sends the note objects (which are assumed to have MIDI parameters) at times in milliseconds"
    (if (null output-device-id)
	(enable-playing))
    (multiple-value-bind (event-buffer event-count) (create-event-buffer midi-notes)
      (let* ((output-device (pm:OpenOutput output-device-id 100 1000)))
	(pm:Write output-device event-buffer event-count)
	(pm:EventBufferFree event-buffer)
	(pm:Close output-device)))))

;;; TODO this isn't strictly playing using portmidi, it could be elsewhere.
;;; If a tempo parameter is not specified, a
;;; default of 60BPM is used, assuming a beat is an interval of 1.
;;; If an IOI is instead not specified, a default of 1 second is used.
(defun make-rhythm (intervals &key ((:tempo tempo-in-bpm) 60)
		    (ioi 1.0 interval-supplied-p)
		    (pitch 60))
  "Plays a given rhythm (in relative interval values, 1.0 = the shortest interval) to the MIDI device."
  (let* ((shortest-interval-milliseconds (if interval-supplied-p ioi
					     (/ 60000 tempo-in-bpm)))
	 (intervals-milliseconds (mapcar #'(lambda (x) (truncate (* shortest-interval-milliseconds x))) intervals)))
    (loop 
       with start-note = (make-midi-note 0
					 0
					 :key-number pitch
					 :velocity 127
					 :duration (truncate shortest-interval-milliseconds 2))
       for interval in intervals-milliseconds
       for abs-time = interval then (+ abs-time interval)
       for note-tag = 1 then (1+ note-tag)
       collect (make-midi-note abs-time
			       note-tag
			       :key-number pitch
			       :velocity 127
			       :duration (truncate shortest-interval-milliseconds 2)) into note-list
       finally (return (cons start-note note-list)))))

; (play-timed-notes (make-rhythm '(1 1 2 1 3 1) :ioi 250))
; (play-timed-notes (make-rhythm '(1 1 1 3 1 2) :ioi 250))
