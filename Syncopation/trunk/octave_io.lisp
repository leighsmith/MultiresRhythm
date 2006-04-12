;;; $Id: octave_io.lisp 4704 2006-02-09 23:05:28Z leigh $
;;; Reads and writes Octave text matrix files in Common Lisp.

(defun octave-param-value (line)
  "Read non-space word until ':' returning it as a parameter name, that following the colon as it's value."
  (let* ((separator-pos (position #\: line :test #'equal))
	 (name-start (position-if #'alphanumericp line :start 1))
	 (value-start (position-if #'alphanumericp line :start separator-pos)))
    (list (subseq line name-start separator-pos) ; retrieve name
	  (subseq line value-start (length line))))) ; retrieve value

;;; read first line throw away
;;; read next line throw away "# ", examine the field upto ":"
;;; Return a list of new variables assigned, or just assign them and
;;; return them?
(defun load-octave-file (filename)
  "Loads any variables in the given Octave (Matlab) matrix file. 
   Returns a vector and the name of the variable as saved in the matrix file."
  (with-open-file (octave-file filename :direction :input)
    (let ((file-parameters (make-hash-table :test 'equal))
	  rows
	  new-array
	  data-line)
      (setf data-line 
	    (loop
	       for next-line = (read-line octave-file)
	       while (equalp (char next-line 0) #\#)
	       do (let ((param-and-value (octave-param-value next-line)))
		    (setf (gethash (first param-and-value) file-parameters)
			  (second param-and-value)))
	       finally 
		 (setf rows (parse-integer (gethash "rows" file-parameters)))
		 (return next-line)))
      (loop
	 initially (setf new-array (make-array rows)) ; '(list rows columns)
	 for row-index from 0 below rows
	 while data-line
	 do 
	   ;; Attempt to read as a floating point number
	   (setf (elt new-array row-index) (read-from-string data-line))
	   ;; get the next line from the file at the end of the loop
	   ;; so we use the first read version.
	   (setf data-line (read-line octave-file nil)))
      (values new-array (string-upcase (gethash "name" file-parameters))))))

;; (intern name)?
;; (set name (load-octave-file "filename")) ; '(list rows columns)

;; (Not needed to be defined in SBCL)
(defconstant *day-names* '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defun time-of-day ()
  "Returns a formatted date and time string, because there isn't anything standard...sigh"
  (multiple-value-bind
        (second minute hour day month year day-of-week dst-p tz)
      (get-decoded-time)
    (format t "~a ~2,'0d/~2,'0d/~2,'0d ~d:~2,'0d:~0d (GMT~@d)"
	    (nth day-of-week *day-names*)
	    month
	    day
	    year
	    hour
	    minute
	    second
	    (- tz))))

(defun save-octave-file (filename variable)
  "Writes out to filename the supplied list or vector in a format
  suitable for Octave or Matlab to read"
;;  (dolist (variable variables)
    (with-open-file (octave-text-file filename :direction :output :if-exists :new-version)
      (loop 
	 with rows = (length variable)
	 for row-index from 0 below rows
	 initially 
	   (format octave-text-file 
		    "# Created by ~a ~a, ~a <~a@~a>~%" 
		    (lisp-implementation-type)
		    (lisp-implementation-version)
		    (time-of-day)
		    "leigh"
		    (long-site-name))
	    (format octave-text-file "# name: ~a~%" (quote variable))
	    (format octave-text-file "# type: ~a~%" "matrix")
	    (format octave-text-file "# rows: ~d~%# columns: ~d~%" rows 1)
	 do (format octave-text-file "~f~%" (elt variable row-index)))))

;; write out the file
(defun save-iois-to-file (filename iois &key (tempo 80))
  "Writes out IOIs as an octave file"
  (save-octave-file filename 
		    (onsets-to-grid (iois-to-onsets (intervals-in-ms iois
								     :tempo 80)))))

(defun save-grid-to-file (filename grid &key (tempo 80))
  "Writes out grid as an octave file"
  (save-iois-to-file filename 
		     (onsets-to-iois (grid-to-onsets grid)) :tempo tempo))

;; Analyse rhythm
; (run-program "octave")
