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

(defun version-number (system)
  (asdf:component-version (asdf:find-system system)))

(defun info-banner ()
  (let ((system (asdf:find-system 'multiresrhythm)))
    (format t "MultiresRhythm Version ~a, ~a ~a.~%" 
	    (version-number 'multiresrhythm) (asdf:system-license system) (asdf:system-author system))
    (format t "~a.~%~%" (asdf:system-description system))
    (format t "NLISP Version ~a.~%" (version-number 'nlisp))
    (format t "~a Lisp Version ~a.~%" (lisp-implementation-type) (lisp-implementation-version))))

;;; This is generated as the top-level interpreter from a SBCL not run within SLIME with:
;;; (sb-ext:save-lisp-and-die "mrr-expectancy" :executable t :toplevel #'multires-rhythm::expectancy-command-line-parser)
(defun expectancy-command-line-parser ()
  "Function which is run instead of the top-level interpreter for standalone operation"
  ;; (format t "argv ~a~%" sb-ext:*posix-argv*)
  (cond ((< (length sb-ext:*posix-argv*) 2)
	 (info-banner))
	(t
	 (last-expectancy-of-file (second sb-ext:*posix-argv*))))
  0)

