;; foottapper
;; honing@uva.nl, jan 2007


(defun foot-tapper (model
                    pattern &rest args
                    &key 
                    (beat t)
                    (shoe t)
                    (r-key 60) (r-veloc 0.6) (r-channel 0) (r-pan -1) (r-duration .05) 
                    (b-key 45) (b-veloc 0.5) (b-channel 1) (b-pan -1) (b-duration .05) ; b-key was 56
                    (s-key 72) (s-veloc 1.0) (s-channel 2) (s-pan 1)  (s-duration .05) ; not used
                    (s-advance 0) ; in s
                    (unit 4)        ; counting unit
                    (tempo 120)     ; tempo (i.e. 4 = 120 MM)
                    (tap-function   ; function returning list of onset-times in relative time
                     'filled-in-t3s) ; was last-beat
                    (debug #'print-state-debug)
                    &allow-other-keys) 
  (format t "~%=================================================================")
  (when pattern 
    (let* ((trace (apply model pattern :trace #'now-rule-t1-beat :allow-other-keys t args))
           (beat-grid-onsets (when trace (funcall tap-function
                                                  trace 
                                                  (apply #'+ pattern)
                                                  2))))

      (play-grid-shoe (integrate pattern) beat-grid-onsets 
                      :beat beat
                     ; :shoe shoe
                      :r-key r-key :r-veloc r-veloc :r-channel r-channel :r-pan r-pan :r-duration r-duration
                      :b-key b-key :b-veloc b-veloc :b-channel b-channel :b-pan b-pan :b-duration b-duration
                      ;:s-key s-key :s-veloc s-veloc :s-channel s-channel :s-pan s-pan :s-duration s-duration
                     ; :s-advance s-advance
                      :unit unit  
                      :tempo tempo)
      )))


(defun play-grid-shoe (pattern-grid-onsets beat-grid-onsets
                                      &key (beat t)
                                      (shoe t) ;not used anymore, from here down
                                      (r-key 60) (r-veloc 0.6) (r-channel 0) (r-pan -1) (r-duration .05) ;in s
                                      (b-key 45) (b-veloc 0.5) (b-channel 1) (b-pan -1) (b-duration .05)
                                      (unit 4)        ; counting unit
                                      (tempo 100)     ; tempo (i.e. 4 = 100 MM)
                                      )
  (let ((beat-onsets (loop for onset in  beat-grid-onsets
                           collect (grid-to-s onset tempo unit)))          
        (pattern-onsets (loop for onset in  pattern-grid-onsets
                              collect (grid-to-s onset tempo unit))))
    (visualize-grid-shoe pattern-onsets  (when beat beat-onsets )) ;; new added 2007
    (play-patterns :rhythm pattern-onsets :beats  (when beat beat-onsets )
                   :r-key r-key :b-key b-key
                  ; :keys (list r-key b-key)
                  ; :velocities (list r-veloc b-veloc)
                  ; :channels (list r-channel b-channel)
                  ; :durations (list r-duration b-duration)
                   )))


(defun grid-to-ms (duration tempo unit)
  (unless (> tempo 0) (error "invalid tempo: ~A" tempo))
  (round (* (/ duration unit tempo) 60000)))

(defun grid-to-s (duration tempo unit)
  (unless (> tempo 0) (error "invalid tempo: ~A" tempo))
  (* (/ duration unit tempo) 60))

(defun ms-to-grid (duration tempo unit)
  (round (/ (* duration unit tempo) 60000)))

(defun s-to-grid (duration tempo unit)
  (round (/ (* duration unit tempo) 60)))

(defun s-to-ms (time)
  (round (* 1000 time)))

(defun ms-to-s (time)
  (/ time 1000.0))

;(grid-to-s 4 120 4)
;(grid-to-s 4 60 4)
;(grid-to-s 8 60 4)

(defun grid-advance (onsets advance tempo unit)
  (loop with old = 0
        for onset in onsets
        as point = (- (grid-to-s onset tempo unit) advance)
        when (and (>= point 0) (or (null old) (>= point (+ old advance))))
        collect point
        and do (setf old point)))

(defun grid-de-advance (onsets advance tempo unit)
  (loop for onset in onsets
        as point = (s-to-grid (+ onset advance) tempo unit)
        collect point))

(defun advance (onsets advance)
  (loop with old = 0
        for onset in onsets
        as point = (- onset advance)
        when (and (>= point 0) (or (null old) (>= point (+ old advance))))
        collect point
        and do (setf old point)))

;(grid-advance '(0 2 10 12 14 15 16) 1500 60 1)
;(grid-de-advance '(500 8500 10500 12500 14500) 1500 60 1)

(defun print-tap-warning (onsets taps &optional (stream t))
    (terpri stream)
    (print-grid stream (differentiate onsets) "foot taps" 27 1 "." "|" "|")
    (print-grid stream (differentiate (cons 0 taps)) "realised" 27 1 "." "|" "|"))


;*******************************************************************************************

(defun play-patterns (&key rhythm beats (r-key 60) (b-key 61)) 
  (let* ((rhythm-pairs (mapcar #'(lambda(x) (list (* x 1000) r-key)) rhythm))
         (beat-pairs (mapcar #'(lambda(x) (list (* x 1000) b-key)) beats))
         (onsets-pitches (merge 'list rhythm-pairs beat-pairs #'< :key #'first)))
   ; (pprint (list 'r rhythm 'b beats))
    (send-multiple-timed-notes-pitches (differentiate (mapcar #'first onsets-pitches))
                                (mapcar #'second onsets-pitches)
                                100 ;velocity
                                2 ;duration
                                )))




