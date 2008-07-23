;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Functions for generating expectations of metrical rhythms.
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <lsmith@science.uva.nl> 
;;;;
;;;; Copyright (c) 2007
;;;;

(in-package :dorys)
(use-package :nlisp)
(use-package :multires-rhythm)

#|
;;; Just needs the syncopation package loaded.
(defun metric-scale (meter &key (max-amp 1.0d0) (min-amp 0.12d0))
  "Derive from Longuet-Higgin's & Lee's metric salience measure (which is inverted in
   polarity) an amplitude weighting"
  (let* ((lhl-metric-salience (make-narray (syncopation::lh-metric-salience meter)))
	 (max-salience (.max lhl-metric-salience))
	 (min-salience (.min lhl-metric-salience))
	 (amp-scaling (/ (- max-salience min-salience) (- max-amp min-amp))))
    ;; TODO need to scale the minimum above 0.
    (.+ 1d0 (./ (.* 1d0 lhl-metric-salience) amp-scaling))))

(defun metrically-scaled-rhythm (meter bars tempo)
  "Returns a rhythm with weighted onsets matching the metrical structure"
  (let* ((metrical-weights (nlisp::array-to-list (metric-scale meter)))
	 (times (nlisp::array-to-list (.rseq 0 1.0d0 (1+ (reduce #'* meter))))))
    (rhythm-of-weighted-onsets "metrical scaling" (mapcar #'list times metrical-weights))))
|#

;;;; 

;;; Randomly generate metrical rhythms of a fixed meter, with no upbeats.
;;; TODO Need to specify tempo, either directly or with shortest-ioi.
(defun random-rhythm-of-meter (meter number-of-bars 
			       &key (quarter-note-IOI 0.600) 
			       (max-division-of-crotchet 4)
			       (sample-rate 200.0))
  (let* ((bar-length (reduce #'* meter))
	 (rhythm-name (format nil "~d bar random rhythm in ~a meter" number-of-bars meter))
	 (random-metrical-rhythm (shoe::pattern-from-grammar-long-segment 0 (* number-of-bars bar-length)
									  bar-length meter)))
    ;; Assume 16ths are the minimum division
    (values (multires-rhythm::iois-to-rhythm rhythm-name 
			    random-metrical-rhythm 
			    :shortest-ioi (/ (* sample-rate quarter-note-IOI) max-division-of-crotchet)
			    :sample-rate sample-rate)
	    random-metrical-rhythm)))

;; (setf random-44 (random-rhythm-of-meter '(2 2 2 2) 2))
;; (random-rhythm-of-meter '(3 2 2) 2)
;; (random-rhythm-of-meter '(3 2) 2)

(defun probed-random-rhythm-of-meter (meter)
  (let* ((metrical-rhythm (random-rhythm-of-meter meter 2))
	 (empty+probe-rhythms (make-probe-rhythms "empty+probe" '((0.0 1.0) 
								    (1.0 1.0)
								    (2.0 0.0)) :probe-base-time 1.0d0)))
    (mapcar (lambda (probe-rhythm) (append-rhythm metrical-rhythm probe-rhythm)) empty+probe-rhythms)))

;; (setf x (probed-random-rhythm-of-meter '(2 2 2 2)))

;; Used to ensure it is not the regularity of the analysis region which biases the ridge.
(defun random-offset (max-length)
  "Defines a rhythm of a random chunk of silence."
  (make-instance 'rhythm 
		 :name "offset"
		 :time-signal (make-double-array (random max-length)))) ; region-length

(defun make-metrical-set (candidate-meter size-of-rhythm-set number-of-bars)
  "Return a set of unique metrical rhythms"
  (loop ; repeat size-of-rhythm-set ;; needs to be 
     while (< (length metrical-interval-set) size-of-rhythm-set)
     for (rhythm intervals) = (multiple-value-list (random-rhythm-of-meter candidate-meter number-of-bars))
     when (not (member intervals metrical-interval-set :test #'equal))
     collect intervals into metrical-interval-set
     and collect rhythm into metrical-rhythm-set
     ;; and collect (append-rhythm rhythm (random-offset 100)) into metrical-rhythm-set
     ;; (append rhythm (rhythm-of-weighted-onsets "empty duration" '((0.0 1.0) (1.0 0.0))))
     finally (return (values metrical-rhythm-set metrical-interval-set))))

(defun random-metrical-rhythm-expectancies (candidate-meter &key (sample-size 40) (number-of-bars 2))
  "Given a meter, create a set of random metrical rhythms plot the confidences of each expectation"
  (let* ((random-metrical-rhythms (make-metrical-set candidate-meter sample-size number-of-bars))
	 (title (format nil "~a examples of ~a bars of ~a meter" sample-size number-of-bars candidate-meter)))
    (metrical-rhythm-expectancies random-metrical-rhythms candidate-meter number-of-bars title)))

;; (plot-expectancies all-expectations (format nil "Expectation Confidences for ~a examples of ~a bars of ~a meter"  sample-size number-of-bars candidate-meter))

;; Read Temperley's Melisma "notefiles". These are described as:
;;  in "notelist" format, "Note [ontime] [offtime] [pitch]", where ontime and offtime
;; are in milliseconds and pitch is an integer, middle C = 60.
;; There are however, other lines in the file, which need pruning.
(defun part-of-melisma-file (filepath)
  "Reads the data file, creates a note-list"
  (with-open-file (input-stream filepath)
    (loop
       for note-type = (read input-stream nil)
       while note-type
       if (string-equal note-type "Note")
       collect
	 (let ((on-time (read input-stream))
	       (off-time (read input-stream))
	       (midi-note (read input-stream)))
	   ;; (format t "~a, ~a, ~a, ~a~%" note-type on-time off-time midi-note)
	   (list (/ on-time 1000.0d0) (/ (- off-time on-time) 1000.0d0) 1.0 midi-note))
       else
       do (read-line input-stream nil))))

(defun rhythm-of-melisma-file (filepath)
  "Returns a rhythm object from the melisma file"
  (let ((melisma-note-list (part-of-melisma-file filepath)))
    (multires-rhythm::rhythm-of-part (pathname-name filepath) melisma-note-list)))

(defun load-essen-rhythms (essen-scores)
  "Reads in the files from the score descriptions passed in, returns a list of rhythm instances."
  (loop 
     for (filename m meter d description) in essen-scores
     for filepath = (make-pathname :directory "/Volumes/iDisk/Research/Data/Temperley/essen-perf" 
				   :name filename 
				   :type "notes")
     collect (rhythm-of-melisma-file filepath)))

;;; (setf rs (load-essen-rhythms (subseq dorys::*essen-perf-meters* 0 5)))
;;; (setf performed-44 (dorys::essen-of-meter "4/4"))
;;; (setf performed-34 (dorys::essen-of-meter "3/4"))
;;; (setf performed-68 (dorys::essen-of-meter "6/8"))
;;; (setf expectancy-set (mapcar #'last-expectations (load-essen-rhythms performed-34)))
;;; (setf all-expectations (reduce #'append expectancy-set))

(defun plot-profile-and-persistency (sample-size number-of-bars meter-name)
  "Display the combined metrical profile and combined ridge persistency measures"
  (let ((meter (meter-for-name meter-name)))
    (multiple-value-bind (random-rhythms random-iois) (make-metrical-set meter sample-size number-of-bars)
      ;; Need an analysis instance to pass in the scaleogram to label the axes, This is way
      ;; too wasteful!
      ;; TODO should produce the analysis of all the rhythms, then compute the arp &
      ;; expectancies from those.
      (let (;;(analysis (analysis-of-rhythm (first random-rhythms)))
	    ;; (arp (average-ridge-persistency random-rhythms))
	    (description (format nil "~a random rhythms in ~a meter of ~a measures"
				 (length random-rhythms) meter-name number-of-bars)))
	(window)
	(multires-rhythm::plot-metrical-profile random-iois meter 
			       (format nil "~a random rhythms in ~a meter" (length random-iois) meter-name))
	(close-window)
	(window)
	(multires-rhythm::plot-interval-histogram (multires-rhythm::interval-histogram random-iois) description)
	(close-window)
	;; (plot-time-histogram (make-narray random-iois) description :bin-size 1)
	;; (plot-ridge-persistency arp
	;; 				(scaleogram analysis)
	;; 				(format nil "Average ridge persistency of ~a" description))
	(multires-rhythm::metrical-rhythm-expectancies random-rhythms
				      meter
				      number-of-bars 
				      (format nil "~a (ending peaks)" description)
				      :expectation-generator #'multires-rhythm::expectancies-of-rhythm)
	(multires-rhythm::metrical-rhythm-expectancies random-rhythms
				      meter
				      number-of-bars 
				      (format nil "~a (integration)" description)
				      :expectation-generator #'multires-rhythm::expectancies-of-rhythm-integrator)
	(multires-rhythm::metrical-rhythm-expectancies random-rhythms
				      meter
				      number-of-bars 
				      (format nil "~a (RP)" description)
				      :expectation-generator #'multires-rhythm::expectancies-of-rhythm-ridge-persistency)))))

(defun time-limited-rhythm-named (name &key (sample-rate 200))
  (mapcar (lambda (x) (limit-rhythm x :maximum-samples (* 15 sample-rate)))
	  (load-essen-rhythms (list (essen-named name)))))

(defun time-limited-rhythms-of-meter (meter &key (sample-rate 200))
      (mapcar (lambda (x) (multires-rhythm::limit-rhythm x :maximum-samples (* 15 sample-rate)))
		      (load-essen-rhythms (essen-of-meter meter))))

;; (time-limited-rhythms-of-meter "3/4")

(defun performed-rhythms-of-meter (meter-name)
  (let ((rhythm-set (time-limited-rhythms-of-meter meter-name)))
    (multires-rhythm::performed-rhythm-expectancies rhythm-set (meter-for-name meter-name)
						    (format nil "~a Performed Rhythms in ~a Meter"
							    (length rhythm-set) meter-name))))

(defun one-rhythm (meter number-of-bars)
  (multiple-value-bind (one-rhythm one-iois) 
      (random-rhythm-of-meter meter number-of-bars)
    (multires-rhythm::plot-profile-and-persistency-of-rhythm one-rhythm one-iois meter number-of-bars)))

(defun test-phase-correction (r)
  (format t "rhythm ~a~%onsets at samples ~a~%" r (multires-rhythm::onsets-in-samples r))
  ;; trim the end of the rhythm
  (let ((trimmed-rhythm (multires-rhythm::limit-rhythm r :maximum-samples (- (multires-rhythm::duration-in-samples r) 
									     (random 100)))))
    (multires-rhythm::last-expectations r :expectancies-generator #'multires-rhythm::expectancies-of-rhythm-ridge-persistency) 
    ;; With a limited rhythm, the phase is the same for a highly persistent ridge upto the
    ;; region which is trimmed. Actually the phase zero points match even on low
    ;; persistency ridges. The projection from the zero phase value of the limited rhythm won't necessarily
    ;; match that of the longer rhythm since the phase warps in and out over the extra
    ;; region. Need to test what the projection point would be if we used the phase value,
    ;; not chasing.
    (multires-rhythm::last-expectations trimmed-rhythm)))

(defun test-phase-offset (r)
  "Tests where the expectancies fall with an inserted space at the start of the rhythm"
  (format t "rhythm ~a~%onsets at samples ~a~%" r (multires-rhythm::onsets-in-samples r))
  (multires-rhythm::last-expectations r :expectancies-generator #'multires-rhythm::expectancies-of-rhythm-ridge-persistency) 
  (multires-rhythm::last-expectations (multires-rhythm::append-rhythm (random-offset 100) r) ; shift the start of the rhythm
				      :expectancies-generator #'multires-rhythm::expectancies-of-rhythm-ridge-persistency))

;; (defun x (meter number-of-bars) (test-phase-correction (random-rhythm-of-meter meter number-of-bars)))
;; (test-phase-correction (rhythm-of-grid "Isochronous Rhythm" '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) :shortest-ioi 256))


;;;; ICMPC plots.
(defun icmpc-plots ()
  ;;; Generate the tempo profile.
  (pushnew 'tempo-beat-preference *plotting*)
  (one-rhythm '(3 2 2) 8)
  (pop *plotting*)

  ;; About 1 rhythm of 30 bars, or 5 rhythms of 6 bars each is minimally necessary to produce a
  ;; canonical metrical profile.
  (plot-profile-and-persistency 20 8 "3/4")
  (plot-profile-and-persistency 20 6 "4/4")
  
  (performed-rhythms-of-meter "4/4")
  (performed-rhythms-of-meter "3/4"))

;; (performed-rhythms-of-meter "6/8")
;; (performed-rhythms-of-meter "3/8")
;; (performed-rhythms-of-meter "2/4")
;; (performed-rhythms-of-meter "6/4")
;; (performed-rhythms-of-meter "3/2")

(defun test-meter-classifier (number-of-tests)
  (let* ((metrical-rhythms (make-metrical-set '(2 2 2 2) number-of-tests 16)))
    (loop
       for rhythm in metrical-rhythms
       for analysis = (analysis-of-rhythm rhythm :padding #'multires-rhythm::causal-pad)
       do (format t "meter is ~a~%" (multires-rhythm::meter-of-analysis-likely analysis)))))

#|

;;; Need minimum of 1 rhythm of 16 bars to get a good metrical profile
(setf one-34-rhythm (one-rhythm '(3 2 2) 16))
(one-rhythm '(3 2 2) 8)
(one-rhythm '(2 2 2 2) 6)

(defun plot-profile-and-persistency-new (sample-size number-of-bars meter)
  "Display the combined metrical profile and combined ridge persistency measures"
  (multiple-value-bind (random-rhythms random-iois) (make-metrical-set meter sample-size number-of-bars)
    ;; produce the analysis of all the rhythms, then compute the arp &
    ;; expectancies from those.
    (let* ((all-analyses (mapcar (lambda (r) (analysis-of-rhythm r :padding #'causal-pad)) random-rhythms))
	   (ridge-persistencies (mapcar #'unweighted-ridge-persistency-of all-analyses))
	   (arp (average-ridge-persistency-of ridge-persistencies)))
      (window)
      (plot-metrical-profile random-iois meter "")
      (close-window)
      (window)
      (plot-ridge-persistency arp
			      ;; Need to pass in the scaleogram to label the axes.
			      (scaleogram (first all-analyses))
			      (format nil "Average ridge persistency of ~a random rhythms in ~a meter of ~a measures"
				      (length random-rhythms) meter number-of-bars))
      
      (metrical-rhythm-expectancies random-rhythms all-analyses
				    meter 
				    number-of-bars 
				    (format nil "~a random rhythms in ~a meter" (length random-rhythms) meter)))))

(pushnew 'weighted-beat-ridge *plotting*)
(setf e (expectancies-of-rhythm-integrator random-44 :times-to-check (list (1- (duration-in-samples random-44)))))

(setf r '(2 1 1 2 6 1 1 1 1 1 1 1 1 1 1 2))
(setf m (iois-to-rhythm "blah" r :shortest-ioi (/ 200.0 6) :sample-rate 200))

;;; 4 bars of 3 crotchets each = 48 semiquavers
;;; 3 bars of 4 crotchets each = 48 semiquavers

(setf expected-times (last-expectations m44))
(setf expected-times-no-integration (last-expectations-no-integration m44))
(setf expected-times-from-onset (last-onset-expectations m44))
;; (mapcar (lambda (e) (/ (expected-time e) bar-duration-in-samples)) expected-times) 

;; Resolution is too fine?
(scale-from-period (.* bar-duration-in-samples (make-narray '(3.0 3.25 3.5 3.75 4))) 16) ; for 4/4.
;; In samples:
(setf note-durations-44 (.* bar-duration-in-samples (make-narray '(0.0 0.25 0.5 0.75 1.0))))
(scale-from-period note-durations-44 16)
;; In samples:
(setf note-durations-34 (.* bar-duration-in-samples (make-narray '(0.0 0.333 0.666 1.0))))
(scale-from-period note-durations-34 16)
;; In samples:
(setf projection-times (make-narray (mapcar (lambda (e) (- (expected-time e) rhythm-duration)) expected-times)))
(./ projection-times bar-duration-in-samples)

;; Compare integrator vs. last-onset magnitude & phase.
(last-expectations m)
(last-onset-expectations m)

;; (random-metrical-rhythm-expectancies '(3 2 2) :number-of-bars 4 :sample-size 40) ; 100
;; (random-metrical-rhythm-expectancies '(2 2 2 2) :number-of-bars 3 :sample-size 40) ; 100

;;; larger number of bars.
;; (random-metrical-rhythm-expectancies '(3 2 2) :number-of-bars 8 :sample-size 10)
;; (random-metrical-rhythm-expectancies '(2 2 2 2) :number-of-bars 6 :sample-size 10)

(setf mo (append-rhythm m (random-offset 100)))
(pushnew 'ridge-phase *plotting*)
(list (last-expectations mo :last-time (last-onset-time mo)) ; last-onset-expect-mo
      (last-expectations mo :last-time 799) ; last-meter-expect-mo
      (last-expectations mo)) ; last-moment-expect-mo
;; (plot-expectancies-histogram (last-expectations mo) (name mo))
(mapcar (lambda (e) (time-in-seconds e 200.0)) (last-expectations mo))

(setf iso-rhythm (rhythm-of-grid "Isochronous Rhythm" '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) :shortest-ioi 256))

(setf rhythm-offset (append-rhythm (random-offset 100) iso-rhythm))

(pushnew 'weighted-beat-ridge *plotting*)
(pushnew 'scale-energy-profile *plotting*)
(pushnew 'unweighted-profile *plotting*)
(pushnew 'tempo-preference *plotting*)
;; Do no phase correction when testing metrical expectancies.
(expectancies-of-rhythm-integrator iso-rhythm 
				   :times-to-check (list (1- (duration-in-samples iso-rhythm)))
				   :phase-correct-from nil)


(setf x (tempo-salience-weighting-vector (preferred-tempo-scale 16 200) 128 :octaves-per-stddev 1.0))

(setf m (scaleogram-magnitude (scaleogram (analysis-of-rhythm iso-rhythm))))
(plot (.subseq (.column m 2048) 75 86) (.iseq 75 85) :aspect-ratio 0.66)

;;; The average number of bars in an anthem is 18, so we create the same to match the waltz-anthem-16ths
;;; Produces a very nice profile.
(plot-profile-and-persistency 7 18 '(3 2 2))


;;; Test the meter determination code.
(dolist (rhythm random-34-rhythms)
  (format t "proposed meter ~a~%" (meter-of-analysis (analysis-of-rhythm rhythm :padding #'causal-pad))))

;;; TODO each scale should have it's own summation (leaky integration) constant when
;;; determining the influence and tracking of each peak at each moment. This
;;; should be the differentiation between expectancies-of-rhythm-integrator and
;;; expectancies-of-rhythm-ridge-persistency

(mapcar #'intervals-as-ratios (load-essen-rhythms performed-44))
(mapcar (lambda (x) (plot-time-histogram (intervals-as-ratios x) (name x))) (load-essen-rhythms performed-44))


|#

