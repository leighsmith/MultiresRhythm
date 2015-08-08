(in-package :multires-rhythm)
(use-package :nlisp)

(compute-perceptual-versions "res4/res4_1_resp_text" "res4/res4_1_pOnsets_text" "res4/res4_1")

(setf r (perceptual-salience-rhythm #P"/Volumes/iDisk/Research/Data/PerceptualOnsets/res4/res4_1_resp_text.saliency"
				    #P"/Volumes/iDisk/Research/Data/PerceptualOnsets/res4/res4_1_pOnsets_text.onsets"
				    :weighted nil))

(multiple-value-setq (tactus analysis) (tactus-for-rhythm r :tactus-selector
							  #'create-weighted-beat-ridge))
(plot-cwt-of-rhythm (scaleogram analysis) r)
(plot-cwt+skeleton-of analysis tactus r :title "res4_1")
(plot-cwt+skeleton-of analysis nil r :title "res4_1")
(.save (ridge-peaks analysis) "/Volumes/iDisk/Research/Data/PerceptualOnsets/res4/res4_1.skeleton" :format :octave)

;;;

(compute-perceptual-versions "BessieSmithShort" "BessieSmithShort" "BessieSmithShort"
			     :data-directory "/Volumes/iDisk/Research/Data/PerceptualOnsets/Audio Examples/")

(setf r (perceptual-salience-rhythm #P"/Volumes/iDisk/Research/Data/PerceptualOnsets/Audio Examples/BessieSmithShort.saliency"
				    #P"/Volumes/iDisk/Research/Data/PerceptualOnsets/Audio Examples/BessieSmithShort.onsets"
				    :weighted nil))

(multiple-value-setq (tactus analysis) (tactus-for-rhythm r :tactus-selector
							  #'create-weighted-beat-ridge))
(plot-cwt-of-rhythm (scaleogram analysis) r)
(plot-cwt+skeleton-of analysis tactus r :title "BessieSmithShort")
(plot-cwt+skeleton-of analysis nil r :title "BessieSmithShort")
(.save (ridge-peaks analysis) "/Volumes/iDisk/Research/Data/PerceptualOnsets/Audio Examples/BessieSmithShort.skeleton" :format :octave)

(compute-perceptual-versions "AguaDeBeberExcerpt" "AguaDeBeberExcerpt" "AguaDeBeberExcerpt"
			     :data-directory "/Volumes/iDisk/Research/Data/PerceptualOnsets/Audio Examples/")

(compute-perceptual-versions "JWPrelude" "JWPrelude" "John Williams Prelude #3"
			     :data-directory "/Volumes/iDisk/Research/Data/PerceptualOnsets/Audio Examples/")
