;; Create a relatively isochronous rhythm with an interruption that should disturb the
;; phase.
(setf stutter-rhythm (iois-to-rhythm "interrupted" '(1.0 1.0 1.0 1.0 1.0 1.0 0.15 0.85 1.0 1.0 1.0) :shortest-ioi 100))
(setf stutter-rhythm (iois-to-rhythm "interrupted" '(1.0 1.0 1.0 1.0 1.0 1.0 0.15 0.85 1.0) :shortest-ioi 100))
(setf a (analysis-of-rhythm stutter-rhythm :padding #'causal-pad))
;; Retrieve a ridge and then all the phases for that ridge at each time point.

(setf phase (scaleogram-phase (scaleogram a)))
(setf last-time (1- (duration-in-samples stutter-rhythm)))
(setf skel (skeleton a))
(setf phases-at-onsets
      (loop
	 for ridge-of-interest in (ridges-at-time skel last-time) ;; all ridges at the last time
	 do (format t "ridge ~a~%" ridge-of-interest)
	 collect (make-narray (loop
				 for time across (val (onsets-in-samples stutter-rhythm))
				 for ridge-scale-at-onset = (scale-at-time ridge-of-interest time)
				 do (format t "time = ~a, ridge-scale-at-onset = ~a~%" time ridge-scale-at-onset)
				 when ridge-scale-at-onset
				 collect (.aref phase ridge-scale-at-onset time)))))

(plot (nth 1 phases-at-onsets) (onsets-in-samples stutter-rhythm) :styles '("linespoints"))
(plot (nth 2 phases-at-onsets) (onsets-in-samples stutter-rhythm) :styles '("linespoints"))

(setf sexpect (expectancies-of-rhythm stutter-rhythm))
(setf x (mapcar (lambda (x) (make-narray (mapcar #'expected-features (second x)))) sexpect))


;; Other options:
;; Phase congruency of subset of ridges?
;; Phase relationship to remaining ridges?
;; Weight confidence using phase congruency?
