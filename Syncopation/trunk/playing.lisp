;;; $Id$
;;; Minimal rhythm playing functions interfacing to portmidi.
;;;
;;; Leigh M. Smith <lsmith@science.uva.nl>
;;;

(in-package :syncopation)

(defparameter *output-device-id* nil)

;; initialize portmidi lib
(defun enable-playing ()
  (pm:portmidi)
  (setf *output-device-id* (pm:GetDefaultOutputDeviceID))
  (pm:GetDeviceInfo *output-device-id*))

;; to get available playing devices
;; (pm:CountDevices)
;; (pm:GetDeviceInfo))

(defun iois-to-onsets (iois &optional (onset 0))
  (if iois
    (cons onset (iois-to-onsets (rest iois) (+ onset (first iois))))
    (list onset)))

(defun send-multiple-timed-notes (intervals pitch velocity duration &key (channel 0))
  "Sends the same pitch and duration at multiple times in milliseconds"
  (let* ((output-device (pm:OpenOutput *output-device-id* 100 1000))
	 (event-times (iois-to-onsets intervals (pm:time)))
	 (event-count (* (length event-times) 2)) ; double for note-on/off pairs.
	 (event-buffer (pm:EventBufferNew event-count)))
    (if (null *output-device-id*) (enable-playing))
    (loop				; assign the buffer's note on and off events.
       for next-time in event-times
       for event-index = 0 then (+ event-index 2)
       for note-on-event = (pm:EventBufferElt event-buffer event-index)
       for note-off-event = (pm:EventBufferElt event-buffer (1+ event-index))
       do
	 (pm:Event.message note-on-event (pm:message (+ #b10010000 channel) pitch velocity))
	 (pm:Event.timestamp note-on-event next-time)
	 (pm:Event.message note-off-event (pm:message (+ #b10000000 channel) pitch 0))
	 (pm:Event.timestamp note-off-event (+ next-time duration)))
    (pm:Write output-device event-buffer event-count)
    (pm:EventBufferFree event-buffer)
    (pm:Close output-device)))

;; (send-multiple-timed-notes '(250 250 250 250 250 250) 60 127 120)

;;; If a tempo parameter is not specified, a
;;; default of 60BPM is used, assuming a beat is an interval of 1.
;;; If an IOI is instead not specified, a default of 1 second is used.
(defun play-rhythm (intervals &key ((:tempo tempo-in-bpm) 60)
		    (ioi 1.0 interval-supplied-p)
		    (pitch 60))
  "Plays a given rhythm (in relative interval values, 1.0 = the shortest interval) to the MIDI device."
  (let* ((shortest-interval-milliseconds (if interval-supplied-p ioi
					     (/ 60000 tempo-in-bpm)))
	 (intervals-milliseconds (mapcar #'(lambda (x) (truncate (* shortest-interval-milliseconds x))) intervals)))
    (send-multiple-timed-notes intervals-milliseconds
			       pitch
			       127
			       (truncate shortest-interval-milliseconds 2))))

; (play-rhythm '(1 1 2 1 3 1) :ioi 250)
