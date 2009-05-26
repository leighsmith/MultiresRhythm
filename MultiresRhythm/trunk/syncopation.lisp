;;;; For calculating the syncopation

(in-package :multires-rhythm)
(use-package :nlisp)

(defun metric-grid-from-probabilities (tatum-probabilities grid-length)
  "Returns a binary grid from probabilities of relative silence in each tatum position"
  (let* ((threshold (/ 1.0d0 grid-length)))
    (format t "grid-length ~a, threshold ~a~%" grid-length threshold)
    (.< tatum-probabilities threshold)))

(defmethod observe-onsets ((analysed-rhythm rhythm-description))
  (let* ((rhythm (rhythm analysed-rhythm))
	 (silence-observations (observe-downbeat-of analysed-rhythm
						  4 ; fixed subdivisions-of-beat to 16ths.
						  #'silence-evidence))
	 (onset-observations (metric-grid-from-probabilities silence-observations
							     (.row-count silence-observations))))
    (format t "silence-observations ~a~%" (.column silence-observations 0))
    (window)
    (image onset-observations nil nil
	   :title (format nil "~a observations of ~a" "Onset" (name rhythm))
	   :xlabel "Time (measures)"
	   :ylabel "Tatum location (beat)"
	   :aspect-ratio 0.666)
    (close-window)
    (window)
    (image silence-observations nil nil
	   :title (format nil "~a observations of ~a" "Silence" (name rhythm))
	   :xlabel "Time (measures)"
	   :ylabel "Tatum location (beat)"
	   :aspect-ratio 0.666)
    (close-window)
    onset-observations))

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

(defmethod eval-syncopation-measures ((analysed-rhythm rhythm-description))
  (let* ((rhythm-name (name (rhythm analysed-rhythm)))
	 (onset-observations (observe-onsets analysed-rhythm))
	 (metric-profile (./ (.partial-sum (.transpose onset-observations)) (.column-count onset-observations)))
	 (syncopation-measures (syncopation-measures onset-observations (meter analysed-rhythm)))
	 ;; syncopation measures is returned with measures as columns, tatums as rows,
	 ;; matching orientation of onset-observations.
	 (num-of-tatums (.row-count syncopation-measures))
	 (num-of-measures (.column-count syncopation-measures))
	 (syncopation-profile (./ (.partial-sum (.transpose syncopation-measures)) num-of-measures))
	 (syncopation-variation (./ (.partial-sum syncopation-measures) num-of-tatums)))
    (window)
    ;;(format t "onset-observations ~a~%synopation measures ~a~%metric profile ~a~%syncopation-profile ~a~%" 
    ;;    onset-observations syncopation-measures metric-profile syncopation-profile)
    (plot-histogram (make-narray (list metric-profile syncopation-profile)) nil 
		    :title (format nil "Metric profile of ~a" rhythm-name)
		    :legends '("Beat Occurrence" "Syncopation Intensity")
		    :xlabel "Metric Location"
		    :ylabel "Relative Occurrence in Piece")
    (close-window)
    (window)
    (image syncopation-measures nil nil 
	   :aspect-ratio 0.66
	   :title (format nil "Syncopation intensity for ~a" rhythm-name))
    (close-window)
    (window)
    (plot syncopation-variation nil 
	  :aspect-ratio 0.66
	  :styles '("impulses")
	  :title (format nil "Evolution of syncopation of ~a" rhythm-name))
    (close-window)
    syncopation-profile))

;;; Should be in DORYS
(defun syncopation-for-corpus (corpus)
  (loop
     for piece in corpus
     for annotated-rhythm = (read-annotated-rhythm (first piece)
							:rhythm-directory-root dorys::*quaero-selection-directory*)
     for syncopation-measures = (eval-syncopation-measures annotated-rhythm)))
