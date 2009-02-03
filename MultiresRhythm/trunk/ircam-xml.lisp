;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Routines to read and write ircam-beat XML files.
;;;; Uses the CXML package and the accompanying DOM package.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <Leigh.Smith@ircam.fr> 
;;;;
;;;; Copyright (c) 2008
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

(defun read-ircambeat-bpm (filepath)
  (let* ((bpm-document (cxml:parse filepath (cxml-dom:make-dom-builder)))
	 (bpm (dom:get-elements-by-tag-name (dom:document-element bpm-document) "bpm")))
    ;; (format t "~a~%" bpm)
    (loop
       for bpm-index from 0 below (dom:length bpm)
       for bpm-node = (dom:item bpm bpm-index)
       collect (read-from-string (dom:node-value (dom:item (dom:child-nodes bpm-node) 0))) into bpm-list
       finally (return (make-narray bpm-list)))))

;; (dom:document-element bpm-doc)
;; (dom:tag-name (dom:document-element bpm-doc))
;; (dom:child-nodes (dom:document-element bpm-doc))

;; (dom:length (dom:child-nodes (dom:document-element bpm-doc)))
;; (dom:item (dom:child-nodes (dom:document-element bpm-doc)) 3)

;; ;;; Retrieve the elements
;; (dom:has-child-nodes (dom:item (dom:child-nodes (dom:document-element bpm-doc)) 3))
;; (dom:child-nodes (dom:item (dom:child-nodes (dom:document-element bpm-doc)) 3))

;; (dom:map-node-list (dom:tag-name (dom:child-nodes (dom:document-element bpm-doc))))

;; (dom:length a)
;; (dom:item a 0)
;; (dom:child-nodes (dom:item a 0))
;; (dom:node-value (dom:item (dom:child-nodes (dom:item a 0)) 0))

;;; Creation
;; (dom:create-document (dom:implementation bpm-doc) nil nil nil)


(defun read-ircambeat-markers (filepath)
  "Read the given file using CXML and DOM to return an narray of beat markers"
  (let* ((marker-document (cxml:parse filepath (cxml-dom:make-dom-builder)))
	 (markers (dom:get-elements-by-tag-name (dom:document-element marker-document) "marker")))
    ;; (format t "~a~%" markers)
    (loop
       for marker-index from 0 below (dom:length markers)
       for marker-node = (dom:item markers marker-index)
       collect (read-from-string (dom:node-value (dom:item (dom:child-nodes marker-node) 0))) into marker-list
       finally (return (make-narray marker-list)))))

#|
(defun create-beat-markers-element (document clap-times-in-seconds meter)
  (let* ((marker-DS (dom:create-element document "markerDS")))
    (loop
       for time across (val clap-times-in-seconds)
       for beat-index = 0 then (1+ beat-index)
       for marker-node = (dom:create-element document "marker")
       do
	 (dom:set-attribute marker-node "id" (format nil "~a" beat-index))
	 (dom:set-attribute marker-node "num" (format nil "~a" (1+ (mod beat-index meter))))
	 (dom:append-child marker-node (dom:create-text-node document (format nil "~f" time)))
	 (dom:append-child marker-DS marker-node))
    marker-DS))

;;; Will have to fake out the metrical descriptors.
(defun create-ircam-beat-marker-document (media-description clap-times-in-seconds)
  (let* ((document (dom:create-document (dom:implementation bpm-doc) nil nil nil))
	 (beat-description (dom:create-element document "beatdescription"))
	 (media (dom:create-element document "media"))
	 (segment (dom:create-element document "segment")))
    (dom:set-attribute segment "start" "0.0")
    (dom:set-attribute segment "stop" (format nil "~f" last-time))
    (dom:append-child segment (create-beat-markers-element document clap-times-in-seconds 4))
    (dom:append-child media (dom:create-text-node document media-description))
    (dom:append-child beat-description media)
    (dom:append-child beat-description segment)
    (dom:append-child document beat-description)
    document))

(defun write-ircambeat-marker-stream (stream document)
  ;; To serialize a DOM document, use a SAX serialization sink as the argument to
  ;; dom:map-document, which generates SAX events for the DOM tree.
  (dom:map-document (cxml:make-octet-stream-sink stream :indentation 2 :canonical nil) document))

(defun write-ircambeat-marker-document (filepath document)
  (with-open-file (out filepath :direction :output :element-type '(unsigned-byte 8))
    (write-ircam-beat-marker-stream out document)))

(defun write-ircambeat-marker-document (filepath media-description clap-times-in-seconds)
  (write-ircam-beat-marker-document filepath 
				    (create-ircam-beat-marker-document media-description clap-times-in-seconds)))
|#

(defun write-ircambeat-marker-stream (stream media-description clap-times-in-seconds meter last-time)
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

(defun write-ircambeat-marker-document (filepath media-description clap-times-in-seconds last-time)
  (with-open-file (out filepath :direction :output :element-type '(unsigned-byte 8))
    (write-ircam-beat-marker-stream out media-description clap-times-in-seconds 4 last-time)))

;;(write-ircam-beat-marker-document #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/test.xml"
;; "testfile.wav" (make-narray '(1.0d0 2.0d0 2.5d0 3.0d0)) 9.0d0)

(defun read-annotation-time (marker-node)
  (read-from-string (dom:get-attribute marker-node "time") 0))

(defun read-annotation-beat (marker-node)
  "Return a list of the beat descriptors, or nil if it is ill-formed"
  (let ((beat-type (dom:item (dom:get-elements-by-tag-name marker-node "beattype") 0)))
    (if beat-type
	(read-from-string (dom:get-attribute beat-type "b"))
	nil)))

(defun read-ircam-annotation (filepath &key (marker-name "marker"))
  "Read the given file using CXML and DOM to return an narray of beat markers"
  (let* ((marker-document (cxml:parse filepath (cxml-dom:make-dom-builder)))
	 (markers (dom:get-elements-by-tag-name (dom:document-element marker-document) marker-name)))
    ;; (format t "~a~%" markers)
    (loop
       for marker-index from 0 below (dom:length markers)
       for marker-node = (dom:item markers marker-index)
       for beat = (read-annotation-beat marker-node)
       when beat
       collect beat into marker-beat-list
       and collect (read-annotation-time marker-node) into marker-time-list
       finally (return (values (make-narray marker-time-list) (make-narray marker-beat-list))))))
