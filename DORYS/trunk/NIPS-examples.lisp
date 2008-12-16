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
  ;; res4_1 produces claps at:
  ;; Beat times of res4/res4_1_resp_text in seconds:
  ;; NLISP #(0.1d0 0.69d0 1.26d0 1.8d0 2.325d0 2.865d0 3.445d0 4.155d0 4.855d0
  ;;         5.52d0 6.165d0 6.795d0 7.425d0 8.025d0 8.58d0 9.125d0)
  ;; Tempo estimate: 101.69 BPM
  (compute-perceptual-versions "res4/res4_2_resp_text" "res4/res4_2_pOnsets_text" "res4/res4_2")
  ;; Beat times of res4/res4_2_resp_text in seconds:
  ;; NLISP #(0.475d0 0.985d0 1.54d0 2.2d0 2.85d0 3.495d0 4.145d0 4.825d0 5.56d0
  ;;         6.285d0 6.985d0 7.695d0 8.38d0 9.04d0 9.69d0 10.325d0 10.93d0 11.51d0)
  ;; Tempo estimate: 92.31 BPM
  ;; The ridge selected by the weighted version is too slow.
  (compute-perceptual-versions "res4/res4_3_resp_text" "res4/res4_3_pOnsets_text" "res4/res4_3")
  (compute-perceptual-versions "res4/res4_3_resp_text" "res4/res4_3_pOnsets_text" "res4/res4_3" :start-from-beat 5))

;; Doing this with a beat-multiple of 3 correctly claps, the unweighted version
;; matches the beat rate.
;; (compute-perceptual-versions "res4/res4_3_resp_text" "res4/res4_3_pOnsets_text" "res4/res4_3" :start-from-beat 5 :beat-multiple 3)
;; (compute-perceptual-versions "res4/res4_3_resp_text" "res4/res4_3_pOnsets_text" "res4/res4_3" :beat-multiple 3)

