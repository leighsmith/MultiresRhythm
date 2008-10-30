;;;; -*- Lisp -*-
;;;; Routines to read (and one day write) ircam-beat XML files.
;;;; Uses the CXML package and the accompanying DOM package.

#|
(setf bpm-doc (cxml:parse
	       #P"/Local/Users/leigh/Research/Sources/Rhythm/IRCAM/LeighsTests/res4_1.wav.bpm.xml"
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


;; (setf ircam-beat-markers (read-ircam-beat-markers #P"/Local/Users/leigh/Research/Sources/Rhythm/IRCAM/LeighsTests/res4_1.wav.markers.xml"))
;; (setf mrr-markers (make-narray '(0.1d0 0.69d0 1.26d0 1.8d0 2.325d0 2.865d0 3.445d0 4.155d0 4.855d0  5.52d0 6.165d0 6.795d0 7.425d0 8.025d0 8.58d0 9.125d0)))
;; (nplot (list (.diff ircam-beat-markers) (.diff mrr-markers)) nil :styles '("linespoints" "linespoints") :aspect-ratio 0.66)

;; (setf res4_2-ircam-beat-markers (read-ircam-beat-markers #P"/Local/Users/leigh/Research/Sources/Rhythm/IRCAM/LeighsTests/res4_2.wav.markers.xml"))
;; (setf res4_2-mrr-markers (make-narray '(0.475d0 0.985d0 1.54d0 2.2d0 2.85d0 3.495d0 4.145d0 4.825d0 5.56d0
;;         6.285d0 6.985d0 7.695d0 8.38d0 9.04d0 9.69d0 10.325d0 10.93d0 11.51d0)))
;; (nplot (list (.diff res4_2-ircam-beat-markers) (.diff res4_2-mrr-markers)) nil :styles '("linespoints" "linespoints") :aspect-ratio 0.66)

;;;; To serialize a DOM document, use a SAX serialization sink as the argument to dom:map-document, which generates SAX events for the DOM tree
;;; Create a "beatdescription", holding:
;;; "media" node
;;; Multiple "segments"
;;; "markerDS" holding:
;;; "marker"
;;; Have to fake out the metrical descriptors.
;;; (defun write-ircam-beat-marker (media-description clap-times-in-seconds)
