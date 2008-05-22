;;;; -*- Lisp -*-
;;;;
;;;; $Id$
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
                       :abbr "r"
                       :full "sample-rate"
                       :requires-arguments t
                       :description "The sample rate (in Hz) used for sampled saliency traces (see option saliency)"
                       :example "--sample-rate=[200]")
        (make-instance 'cli-option
                       :abbr "o"
                       :full "output-file"
                       :requires-arguments :optional
                       :description "The name of the file for expectancy times."
                       :example "--output-file=expectancy")))

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
				    :sample-rate sample-rate))
	  (t
	   (last-expectancy-of-file (second argv))))
    0))

;;; This must be run using an SBCL not run within SLIME.
(defun generate-executable ()
  "Call this to generate an executable and die"
  #+sbcl (sb-ext:save-lisp-and-die "emem-expect-mrr" :executable t :toplevel #'expectancy-command-line-parser))
