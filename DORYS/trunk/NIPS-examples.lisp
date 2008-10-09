;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Examples used in NIPS workshop paper.
;;;;

(in-package :dorys)
(use-package :nlisp)
(use-package :multires-rhythm)

(defun compute-perceptual-versions (salience-filename onsets-filename original-sound-filename 
				    &key 
				    (start-from-beat 0) 
				    (beat-multiple 1)
				    (data-directory "/Volumes/iDisk/Research/Data/PerceptualOnsets/"))
  "Just a convenience function to set up path names"
  (let* ((original-sound-path (make-pathname :directory (list :absolute data-directory)
					     :name original-sound-filename
					     :type "wav"))
	 (accompaniment-sound-path (make-pathname :directory "/Volumes/iDisk/Research/Data/Handclap Examples"
						  :name (concatenate 'string original-sound-filename "_mixed")
						  :type "wav"))
	 (saliency-path (make-pathname :directory (list :absolute data-directory) 
				       :name salience-filename
				       :type "saliency"))
	 (onsets-path (make-pathname :directory (list :absolute data-directory) 
				     :name onsets-filename
				     :type "onsets")))
    (multires-rhythm::clap-to-salience-rhythm-files saliency-path onsets-path original-sound-path accompaniment-sound-path
						    :start-from-beat start-from-beat
						    :beat-multiple beat-multiple)))

;;; Drums
(defun nips-drums ()
  (compute-perceptual-versions "res3_1_resp_text"  "res3_1_pOnsets_text"  "res3_1")
  (compute-perceptual-versions "res3_2_resp_text"  "res3_2_pOnsets_text"  "res3_2")
  (compute-perceptual-versions "res3_7_resp_text"  "res3_7_pOnsets_text"  "results3_7")
  (compute-perceptual-versions "res3_9_resp_text"  "res3_9_pOnsets_text"  "results3_9")
  (compute-perceptual-versions "res3_11_resp_text" "res3_11_pOnsets_text" "results3_1" :start-from-beat 1)
  (compute-perceptual-versions "res3_20_resp_text" "res3_20_pOnsets_text" "res3_20")
  (compute-perceptual-versions "res3_38_resp_text" "res3_38_pOnsets_text" "res3_38"))

;;; Sung Saxophone lines
(defun nips-sung-saxophone ()
  (compute-perceptual-versions "res2/res2_20_resp_text"  "res2/res2_20_pOnsets_text"  "res2/res2_20"))

;;; Freely Sung examples.
;; TODO Because we are clapping to a melody, we need to manually set the start-from-beat to get
;; the phase right.
(defun nips-freely-sung ()
  (compute-perceptual-versions "res1/res1_1_resp_text" "res1/res1_1_pOnsets_text" "res1/res1_1" :start-from-beat 1)
  ;; There is a glitch at the beginning of the recording so that clapping from 0 creates a phase problem with res1_2
  ;; So for now we use a start from 1st beat. Because we are clapping on every second beat,
  ;; we actually need to start on beat 2, ie. the third beat.
  (compute-perceptual-versions "res1/res1_2_resp_text" "res1/res1_2_pOnsets_text" "res1/res1_2" :start-from-beat 2)
  (compute-perceptual-versions "res1/res1_3_resp_text" "res1/res1_3_pOnsets_text" "res1/res1_3")
  (compute-perceptual-versions "res4/res4_1_resp_text" "res4/res4_1_pOnsets_text" "res4/res4_1")
  (compute-perceptual-versions "res4/res4_2_resp_text" "res4/res4_2_pOnsets_text" "res4/res4_2")
  ;; The ridge selected by the weighted version is too slow.
  (compute-perceptual-versions "res4/res4_3_resp_text" "res4/res4_3_pOnsets_text" "res4/res4_3")
  (compute-perceptual-versions "res4/res4_3_resp_text" "res4/res4_3_pOnsets_text" "res4/res4_3" :start-from-beat 5))

;; Doing this with a beat-multiple of 3 correctly claps, the unweighted version
;; matches the beat rate.
;; (compute-perceptual-versions "res4/res4_3_resp_text" "res4/res4_3_pOnsets_text" "res4/res4_3" :start-from-beat 5 :beat-multiple 3)
;; (compute-perceptual-versions "res4/res4_3_resp_text" "res4/res4_3_pOnsets_text" "res4/res4_3" :beat-multiple 3)

