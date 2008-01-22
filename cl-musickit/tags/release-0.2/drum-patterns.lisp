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
|#

(defun drum-machine (grids &key (tempo 60) (ioi 300 ioi-supplied-p)
		     (instruments (list *closed-hi-hat* *snare-drum* *bass-drum*)))
  (let* ((drum-note-lists (mapcar (lambda (grid) (note-list-of-rhythm-grid grid :ioi ioi)) grids))
;;  (let* ((drum-note-lists (mapcar (lambda (grid) (note-list-of-rhythm-grid grid :tempo tempo)) grids))
	 (orchestrated-parts (loop
				for instrument-note-list in drum-note-lists 
				for key-number in instruments
				collect (set-drum-instrument instrument-note-list key-number))))
    (mix-note-lists orchestrated-parts)))

(defparameter rock-backbeat 
  (drum-machine '((1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
		  (0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0)
		  (1 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0))
		:tempo 155))

(defparameter sixteenth-backbeat 
  (drum-machine '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
		  (0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0)
		  (1 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0))
		:tempo 140))

;;; S pattern: "Regular rock pattern without syncopation"
(defparameter s-pattern 
  (drum-machine '((1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1)
		  (0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0)
		  (1 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1))
		:ioi 150))

;; (play-timed-notes s-pattern)

;;; Experiment A D1 pattern: "Regular rock pattern with strong syncopation (towards beat pos 0)"
(defparameter sync-d1-pattern 
  (drum-machine '((1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1)
		  (0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0)
		  (1 0 0 0 0 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1))
		:ioi 150))

;;; Experiment A D2 pattern: "Regular rock pattern with weak syncopation (towards beat pos 5)"
(defparameter sync-d2-pattern 
  (drum-machine '((1 0 1 0 1 0 1 0 1 1 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1)
		  (0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0)
		  (1 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1))
		:ioi 150))

;;; Experiment B D1 pattern: "Regular rock pattern with strong shift (towards beat pos 0)"
(defparameter shift-d1-pattern 
  (time-cut (drum-machine '((1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1)
			    (0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0)
			    (1 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1))
			  :ioi 150)
	    2100 300))

;;; Experiment B D2 pattern: "Regular rock pattern with weak shift (towards beat pos 5)"
(defparameter shift-d2-pattern 
  (time-cut (drum-machine '((1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1)
			    (0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0)
			    (1 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1))
			  :ioi 150)
	    1350 1050))

;; (play-timed-notes shift-d2-pattern)
