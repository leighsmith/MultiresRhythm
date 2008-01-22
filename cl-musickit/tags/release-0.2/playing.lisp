;;;; -*- Lisp -*-
;;;; $Id$
;;;; Minimal note playing functions interfacing to portmidi.
;;;;
;;;; Leigh M. Smith <lsmith@science.uva.nl>
;;;;

(in-package :cl-musickit)

;; Passing in the play-time-datum allows decoupling the creation of the event buffer from the time of
;; playing, so the creation can be slow.
(defmethod set-pm-event (play-time-datum (note-to-play note) pm-note-on-event)
  "Convert select parameters of a note into portmidi elements for playing"
  (let ((param (parameters note-to-play)))
    (case (note-type note-to-play)
      ;;  & :note-duration
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

;;; This allows sorting all simultaneously occurring note-ons together.
(defun split-notes (notes)
  "Returns a note list with all note-duration notes split into their respective note-on and
  note-off note events"
  (loop
     for note in notes
     when (eq (note-type note) :note-duration)
     append (list (make-instance 'note 
				 :note-type :note-on 
				 :note-tag (note-tag note)
				 :play-time (play-time note)
				 ;; should remove the duration parameter
				 :parameters (parameters note)) 
		  (make-instance 'note 
				 :note-type :note-off 
				 :note-tag (note-tag note)
				 :play-time (+ (play-time note) (note-duration note))
				 ;; should remove the duration parameter
				 :parameters (parameters note))) into split-notes
     unless (eq (note-type note) :note-duration)
     collect note into split-notes
     finally (return split-notes)))

(defun create-event-buffer (notes)
  "Given a time ordered list of note objects, create and return a portmidi event buffer"
  (let* ((event-count (number-of-events notes)) 
	 (event-buffer (pm:EventBufferNew event-count))
	 (play-now-time (pm:time)))
    (loop				; assign the buffer's note on and off events.
       for note in notes
       for event-index = 0 then (1+ event-index)
       do
	 (set-pm-event play-now-time 
		       note
		       (pm:EventBufferElt event-buffer event-index)))
    (values event-buffer event-count)))

(let ((output-device-id nil))

  ;; initialize portmidi lib
  (defun enable-playing ()
    "Opens the portmidi library and retrieves the default output device"
    (pm:portmidi)
    (setf output-device-id (pm:GetDefaultOutputDeviceID))
    (pm:GetDeviceInfo output-device-id))

  ;; to get available playing devices
  ;; (pm:CountDevices)
  ;; (pm:GetDeviceInfo)

  (defun play-timed-notes (midi-notes)
    "Sends the note objects (which are assumed to have MIDI parameters) at times in milliseconds"
    (let ((playable-events (sort (split-notes midi-notes) #'compare)))
      (if (null output-device-id)
	  (enable-playing))
      (multiple-value-bind (event-buffer event-count) (create-event-buffer playable-events)
	(let* ((output-device (pm:OpenOutput output-device-id 100 1000)))
	  (pm:Write output-device event-buffer event-count)
	  (pm:EventBufferFree event-buffer)
	  (pm:Close output-device))))))

