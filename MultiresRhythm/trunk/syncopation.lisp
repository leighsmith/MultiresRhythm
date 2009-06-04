;;;; For calculating the syncopation

(in-package :syncopation)
(use-package :multires-rhythm)
(use-package :nlisp)
(use-package :prob-downbeat)

(defun normalise-syncopation (syncopation-measures meter)
  "Divide through by the maximum syncopation value capable of being generated for this meter"
  (./ syncopation-measures (abs (apply #'min (syncopation::lh-metric-salience meter)))))

(defun syncopation-measures (onset-observations meter)
  (loop
     with hierarchy = (hierarchy meter)
     for measure-index from 0 below (.column-count onset-observations)
     for measure-onsets = (.column onset-observations measure-index)
     collect (syncopation::calculate-syncopations-on-grid
				     (nlisp::array-to-list measure-onsets) hierarchy) 
     into syncopation-measures
     ;; Transpose to return the measures to the same orientation as the input data.
     finally (return (normalise-syncopation (.transpose (make-narray syncopation-measures)) hierarchy))))

(defmethod eval-syncopation-measures ((analysed-rhythm prob-downbeat:rhythm-description))
  (let* ((rhythm-name (name (prob-downbeat:rhythm analysed-rhythm)))
	 (onset-observations (prob-downbeat:observe-onsets analysed-rhythm))
	 (metric-profile (./ (mrr::.partial-sum (.transpose onset-observations)) (.column-count onset-observations)))
	 (syncopation-measures (syncopation-measures onset-observations (meter analysed-rhythm)))
	 ;; syncopation measures is returned with measures as columns, tatums as rows,
	 ;; matching orientation of onset-observations.
	 (num-of-tatums (.row-count syncopation-measures))
	 (num-of-measures (.column-count syncopation-measures))
	 (syncopation-profile (./ (mrr::.partial-sum (.transpose syncopation-measures)) num-of-measures))
	 (syncopation-variation (./ (mrr::.partial-sum syncopation-measures) num-of-tatums)))
    (diag-plot 'metric-profile
      ;;(format t "onset-observations ~a~%synopation measures ~a~%metric profile ~a~%syncopation-profile ~a~%" 
      ;;    onset-observations syncopation-measures metric-profile syncopation-profile)
      (plot-histogram (make-narray (list metric-profile syncopation-profile)) nil 
		      :title (format nil "Metric profile of ~a" rhythm-name)
		      :legends '("Beat Occurrence" "Syncopation Intensity")
		      :xlabel "Metric Location"
		      :ylabel "Relative Occurrence in Piece"))
    (diag-plot 'syncopation-intensity
      (image syncopation-measures nil nil 
	     :aspect-ratio 0.66
	     :title (format nil "Syncopation intensity for ~a" rhythm-name)))
    (diag-plot 'syncopation-variation
      (plot syncopation-variation nil 
	    :aspect-ratio 0.66
	    :styles '("impulses")
	    :title (format nil "Evolution of syncopation of ~a" rhythm-name)))
    syncopation-profile))

