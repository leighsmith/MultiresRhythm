(push 'cwt+skeleton *plotting*)
(push 'odf *plotting*)
(push 'tap-phase *plotting*)
(push 'weighted-beat-ridge *plotting*)
(push 'scaleogram-probabilities *plotting*)
(push 'transition-probabilities *plotting*)
(push 'scaleogram-probability-profile *plotting*)

(mrr:clap-to-odf-file #P"/tmp/hum-65_cropped_lognovelty.txt"
		      #P"/Users/leigh/clap_times.txt"
		      #P"/Users/leigh/Sources/Contracting/Humtap/data/cache/60.wav"
		      #P"/Users/leigh/60_mix.wav" :sample-rate 86.132812)

(mrr:clap-to-odf-file #P"/Users/leigh/hum-68_cropped_lognovelty.txt" 
		      #P"/Users/leigh/hum-68_cropped_beats.txt"
		      #P"/Users/leigh/Sources/Contracting/Humtap/data/cache/63.wav"
		      #P"/Users/leigh/hum-68_cropped_mix.wav"
		      :sample-rate 86.132812)

;;; Log novelty
;;; Examine first
;;;
(setf hum68odf (rhythm-from-odf #P"/tmp/hum-68_cropped_lognovelty.txt" :sample-rate 86.132812))
(setf rhythm-analysis (analysis-of-rhythm hum68odf :voices-per-octave 16))
(setf scaleogram (scaleogram rhythm-analysis))
(setf scaleogram-magnitudes (scaleogram-magnitude scaleogram))
(setf chosen-tactus (select-probable-tactus hum68odf rhythm-analysis))
(setf chosen-tactus (create-weighted-beat-ridge hum68odf rhythm-analysis))
(setf chosen-tactus-list (select-longest-lowest-tactus hum68odf rhythm-analysis))
(scales-in-ridge chosen-tactus)
(plot-cwt+skeleton-of rhythm-analysis (list chosen-tactus) hum68odf)
(plot-cwt+skeleton-of rhythm-analysis chosen-tactus-list hum68odf)

(setf clap-times (clap-to-rhythm hum68odf :start-from-beat 0 :tactus-selector #'select-longest-lowest-tactus))
(setf clap-times (clap-to-rhythm hum68odf :start-from-beat 0 :tactus-selector #'create-weighted-beat-ridge))
(setf clap-times-in-seconds (./ clap-times (* 1d0 (sample-rate hum68odf))))

(save-rhythm-mix #P"/Users/leigh/hum-68_lowest.wav"
		 #P"/Users/leigh/Sources/Contracting/Humtap/data/cache/63.wav"
		 clap-times-in-seconds
		 :clap-sample-file #P"/Users/leigh/Research/Data/Handclap Examples/cowbell.aiff")

(setf downbeat (find-downbeat hum68odf beat-period :strategy #'is-greater-rhythmic-period))

;;;
;;; examine the ground truth version
;;;
(setf claps-gt (.column (.load #P"/Users/leigh/Sources/Contracting/Humtap/data/cache/88.txt" :format :text) 0))
(setf hum68-gt (rhythm-of-onsets "hum68GT" claps-gt :sample-rate 86.132812))
(save-rhythm-mix #P"/Users/leigh/hum-68_gt.wav"
		 #P"/Users/leigh/Sources/Contracting/Humtap/data/cache/63.wav"
		 claps-gt
		 :clap-sample-file #P"/Users/leigh/Research/Data/Handclap Examples/cowbell.aiff")

(setf rhythm-analysis (analysis-of-rhythm hum68-gt :voices-per-octave 16))
(setf chosen-tactus-list (select-longest-lowest-tactus hum68-gt rhythm-analysis))
(setf chosen-tactus (create-weighted-beat-ridge hum68-gt rhythm-analysis))

(plot-cwt (scaleogram rhythm-analysis))
(plot-cwt+skeleton-of rhythm-analysis chosen-tactus-list hum68-gt)

;;;
;;; Normalized novelty
;;;
(setf hum68odf (rhythm-from-odf #P"/Users/leigh/novelty68.txt" :sample-rate 86.132812))
(setf rhythm-analysis (analysis-of-rhythm hum68odf :voices-per-octave 16))
(setf chosen-tactus (select-probable-tactus hum68odf rhythm-analysis))
(setf clap-times (clap-to-rhythm hum68odf :start-from-beat 0 :tactus-selector #'select-probable-tactus))
(setf clap-times-in-seconds (./ clap-times (* 1d0 (sample-rate hum68odf))))

(save-rhythm-mix #P"/Users/leigh/hum-68_selection.wav"
		 #P"/Users/leigh/Sources/Contracting/Humtap/data/cache/63.wav"
		 clap-times-in-seconds
		 :clap-sample-file #P"/Users/leigh/Research/Data/Handclap Examples/cowbell.aiff")


(mrr:clap-to-odf-file #P"/Users/leigh/mynormnovelty68.txt"
		      #P"/Users/leigh/hum-68_cropped_beats.txt"
		      #P"/Users/leigh/Sources/Contracting/Humtap/data/cache/63.wav"
		      #P"/Users/leigh/hum-68_cropped_mix.wav" :sample-rate 86.132812)


;;;
;;; The original routines
;;;
(mrr:clap-to-odf-file #P"/tmp/hum-65_cropped_novelty.txt"
		      #P"/tmp/hum-65_cropped_beats.txt"
		      #P"/Users/leigh/Sources/Contracting/Humtap/data/cache/60.wav"
		      #P"/tmp/hum-65_cropped_mix.wav"
		      :sample-rate 86.132812)

(mrr:clap-to-odf-file #P"/tmp/hum-66_cropped_novelty.txt"
		      #P"/tmp/hum-66_cropped_beats.txt"
		      #P"/Users/leigh/Sources/Contracting/Humtap/data/cache/61.wav"
		      #P"/tmp/hum-66_cropped_mix.wav"
		      :sample-rate 86.132812)

(mrr:clap-to-odf-file #P"/tmp/hum-67_cropped_novelty.txt"
		      #P"/tmp/hum-67_cropped_beats.txt"
		      #P"/Users/leigh/Sources/Contracting/Humtap/data/cache/62.wav"
		      #P"/tmp/hum-67_cropped_mix.wav"
		      :sample-rate 86.132812)

(mrr:clap-to-odf-file #P"/tmp/hum-68_cropped_novelty.txt"
		      #P"/tmp/hum-68_cropped_beats.txt"
		      #P"/Users/leigh/Sources/Contracting/Humtap/data/cache/63.wav"
		      #P"/tmp/hum-68_cropped_mix.wav"
		      :sample-rate 86.132812)

(mrr:clap-to-odf-file #P"/tmp/hum-69_cropped_novelty.txt"
		      #P"/tmp/hum-69_cropped_beats.txt"
		      #P"/Users/leigh/Sources/Contracting/Humtap/data/cache/64.wav"
		      #P"/tmp/hum-69_cropped_mix.wav"
		      :sample-rate 86.132812)

