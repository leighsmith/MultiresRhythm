#|
;; zero valued last point to stretch the rhythm
(setf jongsma-ternary-meter-rhythms 
      (create-probe-rhythms "ternary-meter" '((0.0   1.0) 
					      (0.333 0.43) 
					      (0.666 0.43) 
					      (1.0   1.0)
					      (1.333 0.43) 
					      (1.666 0.43) 
					      (2.0 1.0) 
					      (3.0 1.0) 
					      (4.0 0.0))))

(setf jongsma-binary-meter-rhythms 
      (create-probe-rhythms "binary-meter" '((0.0 1.0) 
					     (0.5 0.43) 
					     (1.0 1.0) 
					     (1.5 0.43)
					     (2.0 1.0) 
					     (3.0 1.0) 
					     (4.0 0.0))))


(setf binary-meter (rhythm-of-part "binary-meter" '(( 0.0 0.350 1.0) 
						    ( 2.0 0.255 1.0) 
						    ( 4.0 0.350 1.0) 
						    ( 6.0 0.255 1.0)
						    ( 8.0 0.350 1.0) 
						    (10.0 0.255 1.0) 
						    (12.0 0.350 1.0) 
						    (14.0 0.255 0.5) 
						    (16.0 0.350 1.0) 
						    (18.0 0.255 1.0)
						    (20.0 0.350 1.0) 
						    (22.0 0.255 1.0)
						    (24.0 0.350 1.0) 
						    (26.0 0.255 1.0)
						    (28.0 0.350 1.0) 
						    (30.0 0.255 1.0))))

(plot-rhythm binary-meter)

(setf binary-analysis (analysis-of-rhythm binary-meter))
(plot-cwt-of-rhythm (scaleogram binary-analysis) binary-meter)
(setf binary-ridges (ridges-at-time (skeleton binary-analysis) 3026))
(setf binary-duration (time-support (make-narray (mapcar (lambda (ridge) (scale-at-time ridge 3026)) binary-ridges)) 16))


(setf ternary-meter (rhythm-of-part "ternary-meter" '(( 0.0 0.350 1.0) 
						      ( 2.0 0.255 1.0) 
						      ( 4.0 0.255 1.0) 
						      ( 6.0 0.350 1.0)
						      ( 8.0 0.255 1.0) 
						      (10.0 0.255 1.0) 
						      (12.0 0.350 1.0) 
						      (14.0 0.255 0.5) 
						      (16.0 0.255 1.0) 
						      (18.0 0.350 1.0)
						      (20.0 0.255 1.0) 
						      (22.0 0.255 1.0)
						      (24.0 0.350 1.0) 
						      (26.0 0.255 1.0)
						      (28.0 0.255 1.0) 
						      (30.0 0.350 1.0))))


(plot-rhythm ternary-meter)

(setf ternary-analysis (analysis-of-rhythm ternary-meter))
(plot-cwt (scaleogram ternary-analysis))
;; Would seem to be the same period corresponding to the IOI rate (400 samples) & 
;; no meter is induced

(setf binary-meter-amplitude (rhythm-of-weighted-onsets "binary-meter" '(( 0.0 1.0) 
									 ( 2.0 0.43) 
									 ( 4.0 1.0) 
									 ( 6.0 0.43)
									 ( 8.0 1.0) 
									 (10.0 0.43) 
									 (12.0 1.0) 
									 (14.0 0.43) 
									 (16.0 1.0) 
									 (18.0 0.43)
									 (20.0 1.0) 
									 (22.0 0.43)
									 (24.0 1.0) 
									 (26.0 0.43)
									 (28.0 1.0) 
									 (30.0 0.43))))

(setf ternary-meter-amplitude (rhythm-of-weighted-onsets "ternary-meter" '(( 0.0 1.0) 
									   ( 2.0 0.43) 
									   ( 4.0 0.43) 
									   ( 6.0 1.0)
									   ( 8.0 0.43) 
									   (10.0 0.43) 
									   (12.0 1.0) 
									   (14.0 0.0) 
									   (16.0 0.43) 
									   (18.0 1.0)
									   (20.0 0.43) 
									   (22.0 0.43)
									   (24.0 1.0) 
									   (26.0 0.43)
									   (28.0 0.43) 
									   (30.0 1.0))))

(plot-rhythm jongsma-binary-meter)
;;; Test with causal padding.
(setf jongsma-binary-analysis (analysis-of-rhythm jongsma-binary-meter))
(plot-cwt-of-rhythm (scaleogram jongsma-binary-analysis) jongsma-binary-meter)

(setf jongsma-ternary-meter (rhythm-of-weighted-onsets "ternary-meter" '((0.0   1.0) 
									 (0.333 0.43) 
									 (0.666 0.43) 
									 (1.0   1.0)
									 (1.333 0.43) 
									 (1.666 0.43) 
									 (2.0 1.0) 
									 (3.0 1.0) 
									 (4.0 0.0))))

(setf jongsma-ternary-analysis (analysis-of-rhythm jongsma-ternary-meter))
(plot-cwt-of-rhythm (scaleogram jongsma-ternary-analysis) jongsma-ternary-meter)

(write-as-audio (random-metrical-rhythm-of-meter '(2 2 2 2))
		#P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/test_sound.wav"
		#P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff")

(write-as-audio jongsma-ternary-meter
		#P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/ternary_sound.wav"
		#P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff")

(write-as-audio (random-metrical-rhythm-of-meter '(3 2))
		#P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/test_sound.wav"
		#P"/Volumes/iDisk/Research/Data/Handclap Examples/hihat_closed.aiff")

(setf short-ternary-meter (rhythm-of-weighted-onsets "ternary-meter" '((0.0   1.0) 
								       (0.333 0.43) 
								       (0.666 0.43) 
								       (1.0   1.0)
								       (1.333 0.43) 
								       (1.666 0.43) 
								       (2.0   1.0) 
								       (3.0   0.0))))

(setf short-binary-meter (rhythm-of-weighted-onsets "binary-meter" '((0.0 1.0) 
								     (0.5 0.43) 
								     (1.0 1.0) 
								     (1.5 0.43)
								     (2.0 1.0) 
								     (3.0 0.0))))

;;;;
