;;;; -*- Lisp -*-
;;;;
;;;; $Id: cl-musickit.asd 5456 2009-03-13 22:34:36Z leighsmi $
;;;;
;;;; ASDF definition file for Common Lisp MusicKit routines.
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2007-2009
;;;;

;; (require 'cl-objc)

(defsystem :cl-musickit
  :description "Common Lisp interface to the MusicKit"
  :version "1.0"
  :author "Leigh M. Smith"
  :depends-on (:cl-objc)
  :serial t
  :components ((:file "package")
	       (:file "musickit" :depends-on ("package"))
	       ;; (:file "note") ; Lisp only version of Note class
	       ;; (:file "parts") ; Lisp only version of Part class
	       ;; (:file "playing") ; TODO use MusicKit for all I/O.
	       ;; (:file "rhythm-parts")
	       ;; (:file "drum-patterns")))
))

;;; Check if the MusicKit framework bindings have been compiled, if not, force them.
;; TODO we should check these and attempt to compile them if they don't exist. That should
;; be done by ASDF, really.
;; "MKPerformSndMIDI" "SndKit" MKDSP MusicKit)

;; Ideally check if framework-name has been loaded, rather than checking if the file exists.
;; T to load the CLOS bindings.
;; TODO check that import-framework returns T or nil depending on whether it can load the framework.
;; (objc-cffi:import-framework framework-name t) 
;; otherwise (objc-cffi:compile-framework (framework-name))

