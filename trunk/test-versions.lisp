;; Can run this on V2.1 and V2.6 to see how they differ.
(setf ir (rhythm-of-grid "iso rhythm" '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) :shortest-ioi 256))
(setf vpo 12) ; 
(setf sor (scaleogram-of-rhythm ir :voices-per-octave vpo))

;; Length of rhythm "iso rhythm" is 20.48 seconds
;; CWT input signal length 4096 samples
;; Maximum time period analysed = 1024 samples, dyadic length = 4096 samples
;; Finished CWT, 128 scales, last time period = 1024 samples
;; #<SCALEOGRAM {11E01AB9}> 128 scales x 4096 samples, 16 VPO

(setf m (scaleogram-magnitude sor))
(setf rp (scale-peaks-of-scaleogram sor (sample-rate ir)))
(plot (list (.column m 2048) (.column rp 2048)) nil :aspect-ratio 0.66 :styles '("lines" "impulses"))
(format t "peak scales ~a time support ~a~%" 
	(.find (.column rp 2048))
	(time-support (.find (.column rp 2048)) vpo))
(plot-scale-energy+peaks-at-time sor 2048 rp)

;;; For V1.0 
(pushnew "/Volumes/iDisk/Research/Sources/Rhythm/MultiresRhythm_1.0/" asdf:*central-registry*)
(require 'nlisp)
(require 'multiresrhythm)
(in-package :multires-rhythm)

(defun rhythm-of-grid (name grid &key (tempo 80 tempo-supplied-p)
				(shortest-ioi 1.0)
				(sample-rate 200))
  "Given a rhythmic grid list, returns a rhythm instance"
  (if tempo-supplied-p (setf shortest-ioi (/ (* sample-rate 60) tempo)))
  (iois-to-rhythm name 
		  ;; Append an impulse to ensure trailing silences are counted.
		  (onsets-to-iois (grid-to-onsets (append grid '(1))))
		  :shortest-ioi shortest-ioi))

(plot-cwt (cwt (fm-test) 16) :title "fm")



(setf ir   :shortest-ioi 256))
	 (make-instance 'rhythm 
			:name  "iso rhythm"
			:onset-time-signal (rhythmic-grid-to-signal '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) :tempo 200)
			:sample-rate 200)))




