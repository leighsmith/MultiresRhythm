;;;; $Id$
;;;;

(in-package :dorys)
(use-package :nlisp)
(use-package :multires-rhythm)

;; (defun tempo-change-times (base-tempo acceleration)
;;   (let ((number-of-tones 10)
;; 	(number-of-fixed-tones 7))
;;     (.* (.+ (.rseq 0 (1- number-of-tones) number-of-tones)
;; 	    (.concatenate (make-double-array number-of-fixed-tones) 
;; 			  (.rseq acceleration acceleration (- number-of-tones number-of-fixed-tones)))) base-tempo)))

(defun tempo-change-times (base-tempo acceleration)
  (let ((number-of-tones 10)
	(number-of-fixed-tones 7))
    (make-narray 
     (multires-rhythm::iois-to-onsets (append
				       (make-list number-of-fixed-tones :initial-element base-tempo)
				       (make-list (- number-of-tones number-of-fixed-tones)
						  :initial-element (+ base-tempo acceleration)))))))
												      

(defun tempo-change-stimulus (base-tempo acceleration &key (sample-rate 44100))
  (let* ((beep-duration 0.020) ; mSec.
	 (enveloped-square-wave (make-instance 'multires-rhythm::sound
					       :description "enveloped square wave"
					       :sound-signal (.* (multires-rhythm::make-envelope 0.9 
										:attack 0.001 
										:release 0.001 
										:sustain (- beep-duration 0.001 0.001)
										:sample-rate sample-rate)
								 (multires-rhythm::square-wave 440 beep-duration sample-rate))
					       :sample-rate sample-rate))
	 (beep-times (tempo-change-times base-tempo acceleration))
	 (sound-duration (round (* sample-rate (+ (nlisp::.last beep-times) beep-duration)))))
     (multires-rhythm::sample-at-times sound-duration enveloped-square-wave beep-times)))

;; (plot (tempo-change-stimulus ) nil :style "lines linewidth 2")


;;; Reproduces Vos, van Assen and Franek (1997)
(defun create-vos-data ()
  (loop for tempo in '(0.250 0.500 1.0) ; in seconds
     do (loop for acceleration across (val (.rseq -0.10 0.10 11))
	   do (multires-rhythm::save-to-file (tempo-change-stimulus tempo acceleration)
			    (make-pathname :directory "/Volumes/iDisk/Research/Data/TempoChange/"
					   :name (format nil "tempo_change_~,3f_~,3f" tempo acceleration)
					   :type "wav")))))


