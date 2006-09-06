;;; $Id: playing.lisp 4721 2006-03-23 21:00:53Z leigh $
;;; Minimal rhythm playing functions interfacing to midishare.
;;;
;;; Leigh M. Smith <lsmith@science.uva.nl>
;;;
;;; Nb: Make sure MidiShare has .ini files installed in ~/Library/Preferences/MidiShare

; #+mcl (load "Macintosh HD:Applications:MidiShare:Developer:Lisp:interface:MCL:MidiShare-Interface.lisp")
; Or use CFFI
; #+openmcl (load "/Applications/MidiShare/Developer/Lisp/interface/openmcl/MidiShare-Interface.lisp")

(require 'asdf)
(require 'asdf-install)
; #+sbcl (require :cffi) 
(asdf:operate 'asdf:load-op :cffi)

;; For midishare
(push "/Users/leigh/SysDev/midishare_cffi/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :midishare)


(use-package :ms)
;; Must check returns t if installed.
(midishare)

(midigetversion)

(defparameter *refnum* (midiopen "POCO"))

;;; Shamelessly hacked from the MidiShare tutorial example.
(defun send-multiple-timed-notes (intervals pitch velocity duration)
  "Sends the same pitch and duration at multiple times in milliseconds"
  (MidiConnect *refnum* 0 -1)
  (let ((date (MidiGetTime)))		; remember the current time
    (dolist (next-time (iois-to-onsets intervals date))
      (let ((event (MidiNewEv typeNote)))	; ask for a new note event
	;; if the allocation was succesful
	(unless #+mcl (ccl:%null-ptr-p event)	
		#+sbcl (cffi:null-pointer-p event)
	  (chan event 0)	       	; set the midi channel to 0 (means channel 1)
	  (midishare:port event 0)	       	; set the destination port to 0
	  (midishare:field event 0 pitch)		; set the pitch field
	  (midishare:field event 1 velocity)     	; set the velocity field
	  (midishare:field event 2 duration)      ; set the duration field
	  (MidiSendAt *refnum* 		; send the note event in the future.
                    event	 
                    next-time)
	  ;; (format t "play at ~d ~%" next-time)
	  ;; we need not dispose the original note since it has already been freed.
	  ;; (MidiFreeEv event) 
	  ))))
  (MidiConnect 0 *refnum* 0))            ; break the connection from MidiShare to Lisp 	


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
