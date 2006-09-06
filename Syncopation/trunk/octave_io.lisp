;;; $Id: octave_io.lisp 4704 2006-02-09 23:05:28Z leigh $
;;; Reads and writes Octave text matrix files in Common Lisp.

;; write out the file
(defun save-iois-to-file (filename iois &key (tempo 80))
  "Writes out IOIs as an octave file"
  (.save-to-octave-file filename 
		    (onsets-to-grid (iois-to-onsets (intervals-in-ms iois
								     :tempo 80)))))

(defun save-grid-to-file (filename grid &key (tempo 80))
  "Writes out grid as an octave file"
  (save-iois-to-file filename 
		     (onsets-to-iois (grid-to-onsets grid)) :tempo tempo))

;; Analyse rhythm
; (run-program "octave")
