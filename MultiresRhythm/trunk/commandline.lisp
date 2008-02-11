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

(defun usage (command-name)
  "Print the command line usage"
  (format t "Usage: ~a ~a~%" command-name ""))

;;; This is generated as the top-level interpreter from a SBCL not run within SLIME with:
;;; 
(defun expectancy-command-line-parser ()
  "Function which is run instead of the top-level interpreter for standalone operation"
  ;; (format t "argv ~a~%" sb-ext:*posix-argv*)
  (let ((parsed-cli (cli-parser:cli-parse sb-ext:*posix-argv* *expectancy-options*)))
    (cond ((< (length sb-ext:*posix-argv*) 2)
	   (info-banner))
	  ((gethash "saliency-trace" parsed-cli)
	   (format t "yet to implement saliency-trace file I/O~%"))
	   #|
	  (last-expectancy-of-trace (first (gethash "saliency-file" parsed-cli)) 
	  (first (gethash "sample-rate" parsed-cli))
	  (first (gethash "output-file" parsed-cli)))
	   |#
	  ((gethash "onset-times" parsed-cli)
	   (last-expectancy-of-file (first (gethash "onset-times" parsed-cli))
				    (first (gethash "output-file" parsed-cli))))
	  (t
	   (last-expectancy-of-file (second sb-ext:*posix-argv*))))
    0))

(defun generate-executable ()
  "Call this to generate an executable and die"
  (sb-ext:save-lisp-and-die "emem-expect-mrr" :executable t :toplevel #'expectancy-command-line-parser))
