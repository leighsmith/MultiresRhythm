;;; $Id$
;;; Routines to test the Klapuri onset detector with the multiresrhythm model.

;; (clap-to-salience-rhythm-files saliency-path
;; 			       onsets-path
;; 			       original-sound-path
;; 			       accompaniment-sound-path
;; 			       :start-from-beat 
;; 			       :beat-multiple beat-multiple)))

(setf x (perceptual-salience-rhythm 
	 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/df/emcap5_annotated.wav.df" 
	 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/df/emcap5_annotated.wav_Th4.500.onsets"
	 :sample-rate 245 :weighted nil))

;;; res4_1
(clap-to-salience-rhythm-files
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/df/OGL08202_annotated.wav.df" 
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/df/OGL08202_annotated.wav_Th4.500.onsets"
 #P"/Volumes/iDisk/Research/Data/PerceptualOnsets/res4/res4_1.wav"
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/handclapping/res4_1_mixed.wav"
 :sample-rate 245)

;; res4_2
(clap-to-salience-rhythm-files
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/df/OGL18103_annotated.wav.df" 
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/df/OGL18103_annotated.wav_Th4.500.onsets"
 #P"/Volumes/iDisk/Research/Data/PerceptualOnsets/res4/res4_2.wav"
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/handclapping/res4_2_mixed.wav"
 :sample-rate 245)

;; res4_3
(clap-to-salience-rhythm-files
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/df/OGL36012_annotated.wav.df" 
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/df/OGL36012_annotated.wav_Th4.500.onsets"
 #P"/Volumes/iDisk/Research/Data/PerceptualOnsets/res4/res4_3.wav"
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/handclapping/res4_3_mixed.wav"
 :start-from-beat 5
 :sample-rate 245)

;; res1_1
(clap-to-salience-rhythm-files
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/df/emcap1_annotated.wav.df" 
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/df/emcap1_annotated.wav_Th4.500.onsets"
 #P"/Volumes/iDisk/Research/Data/PerceptualOnsets/res1/res1_1.wav"
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/handclapping/res1_1_mixed.wav"
 :sample-rate 245)

;; res1_2
(clap-to-salience-rhythm-files
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/df/emcap4_annotated.wav.df" 
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/df/emcap4_annotated.wav_Th4.500.onsets"
 #P"/Volumes/iDisk/Research/Data/PerceptualOnsets/res1/res1_2.wav"
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/handclapping/res1_2_mixed.wav"
 :sample-rate 245)

;; res1_3
(clap-to-salience-rhythm-files
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/df/emcap5_annotated.wav.df" 
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/df/emcap5_annotated.wav_Th4.500.onsets"
 #P"/Volumes/iDisk/Research/Data/PerceptualOnsets/res1/res1_3.wav"
 #P"/Volumes/iDisk/Research/Data/EvaluationOfKlapuriOnsetDetector/handclapping/res1_3_mixed.wav"
 :sample-rate 245)
