;;;; -*- Lisp -*-
;;;;
;;;; $Id: sound.lisp 5372 2008-11-07 10:20:32Z leigh $
;;;;
;;;; Functions for detecting onsets.
;;;;
;;;; In nlisp (Matlab-like Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <Leigh.Smith@ircam.fr> 
;;;;
;;;; Copyright (c) 2008 All Rights Reserved.
;;;;

(in-package :multires-rhythm)
(use-package :nlisp)

(setf rwc95 (multires-rhythm::perceptual-salience-rhythm
	#P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_95.odf"
	#P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_95_new.onsets" :sample-rate 172.27d0
	:weighted nil))

;; To inspect the onsets computed by ircambeat.

(plot (list (./ 1.0d0 (.diff onset-times)) (.diff (.arefs (onset-time-signal rwc95) onset-times))) nil)

(defun sonify-onset-detection (rhythm)
  (save-rhythm-mix #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_95_onsets_mixed.wav"
		   #P"/Volumes/iDisk/Research/Data/IRCAM-Beat/rwc_p_95_excerpt.wav" 
		   (onsets-in-seconds rhythm)
		   :clap-amplitudes (.arefs (onset-time-signal rhythm) (onsets-in-samples rhythm))
		   :clap-sample-file #P"/Volumes/iDisk/Research/Data/Handclap Examples/cowbell.aiff"))

;; (sonify-onset-detection rwc95)

;; (plot (.diff (.arefs (onset-time-signal rwc95) within-window)) nil)

#|
With ircambeat onset detector:
MULTIRES-RHYTHM> (onsets-in-samples rwc95)
#<N-INTEGER-ARRAY {11EF0651}> (222)
MULTIRES-RHYTHM> 
NLISP #(44 65 80 98 136 161 180 191 224 251)
MULTIRES-RHYTHM> (.arefs (onset-time-signal rwc95) (.subseq (.+ (onsets-in-samples rwc95) 1) 0 10))
NLISP #(5.832200050354004d0 1.502750039100647d0 1.322100043296814d0
        2.8261799812316895d0 4.151619911193848d0 0.8986390233039856d0
        1.0815999507904053d0 3.1086199283599854d0 4.288750171661377d0
        1.0596100091934204d0)

With my onset detector:
MULTIRES-RHYTHM> (onsets-in-samples rwc95)
NLISP #(45 99 137 192 225 290 318 343 382 411 507 545 556 600 634 693 724 733
        794 825 863 930 1062 1114 1154 1205 1245 1307 1338 1368 1410 1437 1531
        1567 1620 1651 1812 1869 1878 1943 2070 2109 2158 2255 2312 2377 2437
        2528 2566 2638 2765 2810 2854 2946 3018 3053 3143 3175 3257 3308 3387
        3451 3506 3531 3630 3750 3848 3954 3981 4092 4179 4219 4251 4343 4464
        4493 4532 4573 4637 4661 4731 4763 4891 4953 5075 5162 5394 5454 5575
        5590)
MULTIRES-RHYTHM> (.arefs (onset-time-signal rwc95) (.subseq (onsets-in-samples rwc95) 0 10))
NLISP #(5.832200050354004d0 2.8261799812316895d0 4.151619911193848d0
        3.1086199283599854d0 4.288750171661377d0 2.652899980545044d0
        2.3698201179504395d0 3.7452900409698486d0 2.186150074005127d0
        3.940850019454956d0)

;; So the threshold as a fraction of standard deviation still produces the same onsets as
ircam-beat. So the dynamic threshold is going wrong.

(setf decay 0.05) ; half-life (time to decay to 50%)
(setf alpha (- (/ (log 0.5) decay)))
(setf temporal-masking (.exp (.* (- alpha) (.rseq 0 2 50))))
(setf masking-value (.* reliability temporal-masking)
(setf masking-value temporal-masking)
;; compare next reliability (onset amplitude) value against the current reliabilty using masking-value
(plot masking-value (.rseq 0 2 50))

;; Assign the onsets from the computed times derived from the salience trace.
;; (setf (onset-time-signal rwc95) (impulses-at (onsets-of-salience rwc95) (duration-in-samples rwc95)))
|#

;;; Attempt to find peaks by rapid change in energy integration
(setf ts (odf (mrr::subset-of-rhythm p41-odf '(0 1000))))
(setf (.row tsa 0) ts)
(setf tsa (make-double-array (list 1 (.column-count ts))))
(setf int-ts (.row (mrr::cumsum tsa) 0))
(setf int-ts (.row (mrr::window-integration tsa 15) 0))
(setf norm-ts (./ ts (.max ts)))
(plot (list (./ int-ts (.max int-ts)) norm-ts) nil :aspect-ratio 0.66)
;; Remove the increment from the integration
(setf no-cumulative-energy (./ int-ts (.iseq 1 (.length ts))))
(plot (list no-cumulative-energy norm-ts) nil :aspect-ratio 0.66)
;; Probably should be dividing by the moving average (LP filter).
(plot (list (mrr::extrema-points-vector no-cumulative-energy :extrema :min) norm-ts) nil :aspect-ratio 0.66)
