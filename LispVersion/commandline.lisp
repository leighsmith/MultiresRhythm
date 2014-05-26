;;;; -*- Lisp -*-
;;;;
;;;; Functions for operating as a command line tool.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2007
;;;;

(in-package :multires-rhythm)
(use-package :cli-parser)

(defparameter *expectancy-options*
  (list (make-instance 'cli-option
                       :abbr "t"
                       :full "onset-times"
                       :requires-arguments :optional
                       :description "Whether the input format consists of onset times"
                       :example "--onset-times=music.onsets")
        (make-instance 'cli-option
                       :abbr "s"
                       :full "saliency-trace"
                       :requires-arguments :optional
                       :description "The input file in a format consisting of sampled saliency traces"
                       :example "--saliency-trace=music.saliency")
        (make-instance 'cli-option
                       :abbr "e"
                       :full "ending-peaks"
                       :requires-arguments nil
                       :description "Calculates expectancy from the ending peaks, not the ridge persistency"
                       :example "--ending-peaks")
        (make-instance 'cli-option
                       :abbr "r"
                       :full "sample-rate"
                       :requires-arguments t
                       :description "The sample rate (in Hz) used for sampled saliency traces (see option saliency)"
                       :example "--sample-rate=[200]")
        (make-instance 'cli-option
                       :abbr "p"
                       :full "plotting"
                       :requires-arguments t
                       :description "Enable plotting of specific diagnostic plots"
                       :example "--plotting=\"PHASE-HISTOGRAM TEMPO-RIDGE-PERSISTENCY\"")
        (make-instance 'cli-option
                       :abbr "o"
                       :full "output-file"
                       :requires-arguments :optional
                       :description "The name of the file for expectancy times."
                       :example "--output-file=expectancy")))

(defparameter *beat-tracking-options*
  (list (make-instance 'cli-option
                       :abbr "t"
                       :full "onset-times"
                       :requires-arguments :optional
                       :description "Whether the input format consists of onset times"
                       :example "--onset-times=music.onsets")
        (make-instance 'cli-option
                       :abbr "o"
                       :full "odf"
                       :requires-arguments t
                       :description "The input file in a format consisting of a sampled onset detection function"
                       :example "--odf=music.saliency.txt")
        (make-instance 'cli-option
                       :abbr "r"
                       :full "sample-rate"
                       :requires-arguments t
                       :description "The sample rate (in Hz) used for sampled saliency traces (see option saliency)"
                       :example "--sample-rate=[200]")
        (make-instance 'cli-option
                       :abbr "p"
                       :full "plotting"
                       :requires-arguments t
                       :description "Enable plotting of specific diagnostic plots"
                       :example "--plotting=\"PHASE-HISTOGRAM TEMPO-RIDGE-PERSISTENCY\"")
        (make-instance 'cli-option
                       :abbr "c"
                       :full "clap-times"
                       :requires-arguments t
                       :description "The name of the file for clap times."
                       :example "--clap-times=clap-times.txt")
        (make-instance 'cli-option
                       :abbr "i"
                       :full "input"
                       :requires-arguments t
                       :description "The name of the audio input file."
                       :example "--input=audiofile.wav")
        (make-instance 'cli-option
                       :abbr "m"
                       :full "mix"
                       :requires-arguments t
                       :description "The name of the audio file mixing original input audio file with beat track accompaniment."
                       :example "--mix=audiofile.wav")))

(defun version-number (system)
  (asdf:component-version (asdf:find-system system)))

(defun info-banner ()
  (let ((system (asdf:find-system 'multiresrhythm)))
    (format t "MultiresRhythm Version ~a, ~a ~a.~%" 
	    (version-number 'multiresrhythm) (asdf:system-license system) (asdf:system-author system))
    (format t "~a.~%~%" (asdf:system-description system))
    (format t "NLISP Version ~a.~%" (version-number 'nlisp))
    (format t "~a Lisp Version ~a.~%" (lisp-implementation-type) (lisp-implementation-version))))

;;; This is generated as the top-level interpreter from a SBCL not run within SLIME with (generate-executable)
(defun expectancy-command-line-parser ()
  "Function which is run instead of the top-level interpreter for standalone operation"
  (let* ((argv #+sbcl sb-ext:*posix-argv*
	       #+ecl '(""))
	 (parsed-cli (cli-parser:cli-parse argv *expectancy-options*))
	 (sample-rate (if (null (gethash "sample-rate" parsed-cli)) 
			  200.0d0 
			  (read-from-string (first (gethash "sample-rate" parsed-cli))))))
    ;; (format t "argv ~a~%" argv)
    ;; The hash of plotting will return a list of strings, so we read them first.
    (setf *plotting* (mapcar (lambda (x) (intern (string-upcase x) "MULTIRES-RHYTHM"))
			     (gethash "plotting" parsed-cli)))
    (unless (null *plotting*) (format t "Plotting ~a~%" *plotting*))
    (cond ((< (length argv) 2)
	   (info-banner) 
	   (cli-parser:cli-usage (first argv) *expectancy-options*))
	  ((gethash "saliency-trace" parsed-cli)
	   (last-expectancy-of-salience (first (gethash "saliency-file" parsed-cli)) 
					(first (gethash "onset-times" parsed-cli))
					(first (gethash "output-file" parsed-cli))
					:sample-rate sample-rate))
	  ((gethash "onset-times" parsed-cli)
	   (last-expectancy-of-file (first (gethash "onset-times" parsed-cli))
				    (first (gethash "output-file" parsed-cli))
				    :expectancies-generator (if (nth-value 1 (gethash "ending-peaks" parsed-cli))
								#'expectancies-of-rhythm
								#'expectancies-of-rhythm-ridge-persistency)
				    :sample-rate sample-rate))
	  (t
	   (last-expectancy-of-file (second argv))))
    0))

;;; This must be run using an SBCL not run within SLIME.
(defun generate-expectancy-executable ()
  "Call this to generate an executable and die"
  #+sbcl (sb-ext:save-lisp-and-die "emem-expect-mrr" :executable t :toplevel #'expectancy-command-line-parser))

;;; For beat tracking command line tool
(defun beat-tracking-command-line-parser ()
  "Function which is run instead of the top-level interpreter for standalone operation"
  (let* ((argv #+sbcl sb-ext:*posix-argv*
	       #+ecl '(""))
	 (parsed-cli (cli-parser:cli-parse argv *beat-tracking-options*))
	 (sample-rate (if (null (gethash "sample-rate" parsed-cli)) 
			  200.0d0 
			  (read-from-string (first (gethash "sample-rate" parsed-cli))))))
    ;; (format t "argv ~a~%" argv)
    ;; The hash of plotting will return a list of strings, so we read them first.
    (setf *plotting* (mapcar (lambda (x) (intern (string-upcase x) "MULTIRES-RHYTHM"))
			     (gethash "plotting" parsed-cli)))
    (unless (null *plotting*) (format t "Plotting ~a~%" *plotting*))
    (cond ((< (length argv) 2)
	   (info-banner) 
	   (cli-parser:cli-usage (first argv) *beat-tracking-options*))
	  ((gethash "odf" parsed-cli)
	   (clap-to-odf-file (pathname (first (gethash "odf" parsed-cli)))
			     (pathname (first (gethash "clap-times" parsed-cli)))
			     (pathname (first (gethash "input" parsed-cli)))
			     (pathname (first (gethash "mix" parsed-cli)))
			     :sample-rate sample-rate))
	  ((gethash "onset-times" parsed-cli)
	   nil)
	  (t
	   nil))
    0))

(defun generate-beat-track-executable ()
  "Call this to generate an executable and die"
  #+sbcl (sb-ext:save-lisp-and-die "mrr-beat-track" :executable t :toplevel #'beat-tracking-command-line-parser))
