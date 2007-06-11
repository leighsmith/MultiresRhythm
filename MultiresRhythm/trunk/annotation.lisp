;;;;
;;;; $Id$
;;;; 
;;;; Routines to annotate the given rhythm according to an identified metrical structure.
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2006, 2007 All Rights Reserved.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; See multiresrhythm.asd for further info.
;;;;

(defmethod ratios-to-tactus ((rhythm-to-annotate rhythm) (rhythm-skeleton skeleton) (tactus ridge) &key (voices-per-octave 16))
  "Given the rhythm, skeleton of ridges and tactus, produces the ratios to the tactus of each note."
  (let ((note-locations (onsets-in-samples rhythm-to-annotate))) ; Get sample indexes of each onset
    (loop		    ; We proceed causally thorough the rhythm, one note at a time.
       for note-time across (val note-locations)
       for ridges-for-note = (ridges-at-time rhythm-skeleton note-time)
       for tactus-time-support = (time-support (scale-at-time tactus note-time) voices-per-octave)
       for ratios-to-tactus = (mapcar (lambda (ridge-of-note) 
					(/ (time-support (scale-at-time ridge-of-note note-time) voices-per-octave) tactus-time-support))
				      ridges-for-note)
       do (format t "note time ~d:~%~a~%tactus-time-support ~f~%" note-time ridges-for-note tactus-time-support)
       collect (list note-time (sort ratios-to-tactus #'<)))))

(defmethod annotate-rhythm ((rhythm-to-annotate rhythm) (rhythm-skeleton skeleton) (tactus ridge))
  "Given the rhythm and the skeleton of ridges, produce an annotation of each note with
  respect to the tactus."
  ;; We could use Desain & Honing categorisation data to unify or prune the ratio
  ;; lists. This should be implemented as a separate function, of course.
  ;; Need also an underlying measure of ratio uncertainty due the voices-per-octave resolution.
  (ratios-to-tactus rhythm-to-annotate rhythm-skeleton tactus))

(defmethod annotate-rhythm ((rhythm-to-annotate rhythm))
  "Given the rhythm, produce an annotation of each note with respect to the tactus"
  (let ((skeleton (skeleton-of-rhythm rhythm-to-annotate))
	(tactus (select-longest-lowest-tactus skeleton))) ; This is the weak link in the chain...
    (annotate-rhythm rhythm-to-annotate skeleton tactus)))
