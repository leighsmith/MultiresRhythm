;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Routines to read (and one day write) ircam-beat XML files.
;;;; Uses the CXML package and the accompanying DOM package.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <Leigh.Smith@ircam.fr> 
;;;;
;;;; Copyright (c) 2008
;;;;

(require 'cxml)

(in-package :multires-rhythm)
(use-package :nlisp)

#|
(setf bpm-doc (cxml:parse
	       #P"/Local/Users/leigh/Research/Data/IRCAM-Beat/res4_1.wav.bpm.xml"
	       (cxml-dom:make-dom-builder)))
(dom:document-element bpm-doc)
(dom:tag-name (dom:document-element bpm-doc))
(dom:child-nodes (dom:document-element bpm-doc))

(dom:length (dom:child-nodes (dom:document-element bpm-doc)))
(dom:item (dom:child-nodes (dom:document-element bpm-doc)) 3)

;;; Retrieve the elements
(dom:has-child-nodes (dom:item (dom:child-nodes (dom:document-element bpm-doc)) 3))
(dom:child-nodes (dom:item (dom:child-nodes (dom:document-element bpm-doc)) 3))

(dom:map-node-list (dom:tag-name (dom:child-nodes (dom:document-element bpm-doc))))

(setf a (dom:get-elements-by-tag-name (dom:document-element bpm-doc) "bpm"))
(dom:length a)
(dom:item a 0)
(dom:child-nodes (dom:item a 0))
(dom:node-value (dom:item (dom:child-nodes (dom:item a 0)) 0))

;;; Creation
(dom:create-document (dom:implementation bpm-doc) nil nil nil)
|#

(defun read-ircam-beat-markers (filepath)
  "Read the given file using CXML and DOM to return an narray of beat markers"
  (let* ((marker-document (cxml:parse filepath (cxml-dom:make-dom-builder)))
	 (markers (dom:get-elements-by-tag-name (dom:document-element marker-document) "marker")))
    ;; (format t "~a~%" markers)
    (loop
       for marker-index from 0 below (dom:length markers)
       for marker-node = (dom:item markers marker-index)
       collect (read-from-string (dom:node-value (dom:item (dom:child-nodes marker-node) 0))) into marker-list
       finally (return (make-narray marker-list)))))

;;;; To serialize a DOM document, use a SAX serialization sink as the argument to dom:map-document, which generates SAX events for the DOM tree
;;; Will have to fake out the metrical descriptors.
(defun create-ircam-beat-marker-document (media-description clap-times-in-seconds)
  (let* ((document (dom:create-document (dom:implementation bpm-doc) nil nil nil))
	 (beat-description (dom:create-element document "beatdescription"))
	 (media (dom:create-element document "media"))
	 (marker-DS (dom:create-element document "markerDS")))
    (loop
       for time across (val clap-times-in-seconds)
       do (dom:create-element document "marker"))
    (dom:create-attribute document )
    document))

(defun write-ircam-beat-marker-stream (stream document)
  (dom:map-document (cxml:make-octet-stream-sink stream :indentation 2 :canonical nil)
		    document))

(defun write-ircam-beat-marker-document (filepath document)
  (with-open-file (out filepath :direction :output :element-type '(unsigned-byte 8))
    (write-ircam-beat-marker-stream out document)))

(defun write-ircam-beat-marker-document (filepath media-description clap-times-in-seconds)
  (write-ircam-beat-marker-document filepath 
				    (create-ircam-beat-marker-document media-description clap-times-in-seconds)))

(defun write-ircam-beat-marker-stream (stream media-description clap-times-in-seconds meter last-time)
  (cxml:with-xml-output (cxml:make-octet-stream-sink stream :indentation 2 :canonical nil)
    (cxml:with-element "beatdescription"
      (cxml:with-element "media"
	(cxml:text media-description))
      (cxml:with-element "segment"
	(cxml:attribute "start" "0.0") 
	(cxml:attribute "stop" (format nil "~f" last-time))
	(cxml:with-element "markerDS"
	  (loop
	     for time across (val clap-times-in-seconds)
	     for beat-index = 0 then (1+ beat-index)
	     do (cxml:with-element "marker"
		  (cxml:attribute "id" (format nil "~a" beat-index))
		  (cxml:attribute "num" (format nil "~a" (1+ (mod beat-index meter))))
		  (cxml:text (format nil "~f" time)))))))))

(defun write-ircam-beat-marker-document (filepath media-description clap-times-in-seconds last-time)
  (with-open-file (out filepath :direction :output :element-type '(unsigned-byte 8))
    (write-ircam-beat-marker-stream out media-description clap-times-in-seconds 4 last-time)))

;;(write-ircam-beat-marker-document #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/test.xml"
;; "testfile.wav" (make-narray '(1.0d0 2.0d0 2.5d0 3.0d0)) 9.0d0)
