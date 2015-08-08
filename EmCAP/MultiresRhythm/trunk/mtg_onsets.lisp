(in-package :multires-rhythm)

(defun rhythm-of-aubio-onsets (filename)
  (with-open-file (f "/Users/leigh/Research/Data/NewAnalysedRhythms/MTG_pat1.onsets" :direction :input)
    (let* ((onsets (read f nil))
	   ;; Returns the onset times in seconds, convert them to sample rate.
	   (iois-in-samples (.floor (.* 200 (make-narray (onsets-to-iois onsets))))))
      (iois-to-rhythm "pat1" (make-narray iois-in-samples) :sample-rate 200))))

(setf pat1-rhythm (rhythm-of-aubio-onsets "MTG_pat1.onsets"))
(onset-time-signal pat1-rhythm)
(plot-rhythm pat1-rhythm)

(setf pat1-analysis (analysis-of-rhythm pat1-rhythm))
(format t "pat1-skeleton ~a~%" (ridges (skeleton pat1-analysis)))
(plot-scale-energy-at-times (scaleogram pat1-analysis) '(100) :sample-rate 200)
(plot-scale-energy-at-times (scaleogram pat1-analysis) '(100))
(setf ridge-set (ridges-at-time (skeleton pat1-analysis) 100))
(mapcar (lambda (ridge) (time-support-seconds (scale-at-time ridge 100) 16 200)) ridge-set)
(mapcar (lambda (ridge) (scale-at-time ridge 100)) ridge-set)


(setf pat1-clapping (clap-to-rhythm pat1-rhythm :tactus-selector (lambda (skeleton)
								   (ridge-containing-scale-and-time
								    (skeleton
								     pat1-analysis) 104 100))))
(save-rhythm-and-claps pat1-rhythm pat1-clapping)
