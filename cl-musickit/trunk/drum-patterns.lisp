;;;; -*- Lisp -*-
;;;; $Id: rhythm-parts.lisp 377 2007-11-14 13:37:26Z leigh $
;;;; Shades of TR-303 programming!
;;;;
;;;; Leigh M. Smith <lsmith@science.uva.nl>
;;;;

(in-package :cl-musickit)

;;; General MIDI percussion map key numbers:
(defparameter *cowbell* 56)		; co
(defparameter *claves* 75)		; cl
(defparameter *hi-woodblock* 76)	; hw
(defparameter *low-woodblock* 77)	; lw
(defparameter *closed-hi-hat* 42)	; hh
(defparameter *bass-drum* 36)		; bd
(defparameter *snare-drum* 38)		; sd

#|
(defun drum-machine (filename grids &key (tempo 60) (beats-per-measure 4))
  (save-scorefile (concatenate 'string 
			       "/Volumes/iDisk/Music Compositions/Scorefiles/Drum Patterns/" filename ".score")
		  (mapcar (lambda (grid) (grid-to-onsets (append grid '(1)))) grids)
		  :description filename
		  :instrument "midi"
		  :midi-channel 10
		  :tempo 480
		  :key-numbers (list *bass-drum* *snare-drum* *closed-hi-hat*)))

(defun drum-machine (filename grids &key (tempo 60) (beats-per-measure 4))
  (save-scorefile (concatenate 'string 
			       "/Volumes/iDisk/Music Compositions/Scorefiles/Drum Patterns/" filename ".score")
		  (mapcar (lambda (grid) (grid-to-onsets (append grid '(1)))) grids)
		  :description filename
		  :instrument "midi"
		  :midi-channel 10
		  :tempo 480
		  :key-numbers (list *bass-drum* *snare-drum* *closed-hi-hat*)))
|#

(defun drum-machine (name grids &key (tempo 60) 
		     (beats-per-measure 4) 
		     (instruments (list *bass-drum* *snare-drum* *closed-hi-hat*)))
;;  (let* ((drum-note-lists (mapcar (lambda (grid) (note-list-of-rhythm-grid grid :tempo tempo)) grids))
  (let* ((drum-note-lists (mapcar (lambda (grid) (note-list-of-rhythm-grid grid :ioi 300)) grids))
	 (orchestrated-parts (loop
				for instrument-note-list in drum-note-lists 
				for key-number in instruments
				collect (set-drum-instrument instrument-note-list key-number))))
    (mix-note-lists orchestrated-parts)))

(drum-machine "rock-backbeat"
	      '((1 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0)
		(0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0)
		(1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)))

(drum-machine "sixteenth-backbeat"
	      '((1 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0)
		(0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0)
		(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
			     
