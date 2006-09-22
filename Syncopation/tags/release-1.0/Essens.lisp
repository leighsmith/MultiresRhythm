;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $Id$
;;;
;;; Implementation of the Clock Model of Essens 1995
;;;
;;; taken from:
;;;    Essens, P. (1995). Structuring temporal sequences: Comparison of models and factors 
;;;    of complexity. Perception & Psychophysics, 57 (4), 519-532
;;;
;;; by Bas de Haas <bas.dehaas@phil.uu.nl>


;;; Taken from Essens 1995, Table 8, 24 patterns also used in a 
;;; complexity judgement experiment. Like Shumlevich data,
;;; a 1= simple to 5= complex rating was used.

(defparameter *essens-table-8*
  '((2.2 (1 1 1 0 1 1 1 0 1 1 1 0 1 1 0 0))
    (3.1 (1 1 1 0 1 0 1 1 1 0 1 1 1 1 0 0))
    (3.2 (1 0 1 1 1 0 1 1 1 0 0 1 1 1 0 0))
    (2.9 (1 0 1 1 1 0 0 1 1 0 1 1 1 1 0 0))
    (2.2 (1 1 1 0 1 1 1 0 1 1 0 1 1 0 0 0))
    (3.1 (1 1 1 0 1 0 0 1 1 0 1 0 1 1 0 0))
    (2.6 (1 1 1 1 1 1 1 0 1 1 1 0 1 1 1 0))
    (4.2 (1 1 1 0 1 1 1 1 1 0 0 1 1 1 0 0))
    (2.9 (1 1 1 1 1 1 0 1 1 0 1 1 1 0 0 0))
    (2.8 (1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 0))
    (3.1 (1 1 1 1 1 1 1 0 1 1 1 0 1 0 1 0))
    (2.5 (1 1 1 0 1 1 0 0 1 0 1 0 1 0 0 0))
    (3.5 (1 0 0 1 1 1 1 0 1 1 0 0 1 1 0 0))
    (2.5 (1 0 1 1 1 1 0 1 1 1 0 1 1 1 0 0))
    (2.4 (1 0 0 1 1 1 0 1 1 1 0 1 1 1 0 0))
    (3   (1 0 0 1 1 1 0 1 1 1 1 0 1 1 0 0))
    (3   (1 1 0 1 1 1 0 1 1 1 1 0 1 0 0 0))
    (3.1 (1 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0))
    (2.4 (1 0 1 0 1 1 0 1 1 1 0 1 1 1 0 0))
    (3.2 (1 1 0 1 1 1 1 0 1 1 0 1 1 0 0 0))
    (2.4 (1 1 0 1 1 1 0 1 1 1 1 1 1 0 0 0))
    (2.9 (1 1 0 0 1 1 0 1 1 1 1 1 1 0 0 0))
    (2.7 (1 0 1 0 1 1 0 1 1 1 1 1 1 1 0 0))
    (3.8 (1 1 0 1 1 1 1 1 1 0 1 0 1 1 0 0))))

;;; Taken from Essens 1995, Table 5,
;;; Using ratings derived from P&E model metrical context.
(defparameter *essens-table-5*
  '((6.3 (1 1 1 0 1 1 0 1 1 1 0 0))
    (4.1 (1 1 1 0 1 1 0 1 1 0 1 0))
    (7.0 (1 1 1 0 1 1 0 1 1 0 0 0))
    (7.6 (1 1 0 1 1 0 1 1 1 0 0 0))
    (5.5 (1 0 1 1 0 1 1 0 1 0 1 0))
    (7.4 (1 1 1 0 1 1 0 0 1 1 0 0))
    (7.2 (1 1 0 1 1 0 0 1 1 1 0 0))
    (8.9 (1 1 0 1 1 0 0 1 1 0 0 0))
    (4.1 (1 1 1 1 1 1 0 1 1 0 1 0))
    (5.1 (1 1 1 1 0 1 1 0 1 0 1 0))))

;; (setf res (syncopation-test (sort *essens-table-5* #'< :key #'first) '(2 3 2)))
;; (plot-syncopation-comparisons (sort *essens-table-5* #'< :key #'first) '(2 3 2))
;; (plot-syncopation-comparisons (sort *essens-table-5* #'< :key #'first) '(3 2 2))

; the example in table 1 of the paper of Essens (1995) p. 520
(defparameter *Etab1* '(1 1 0 1 0 1 1 1 0 1 0 0))

;; example 1 of table 6 of Essens (1995) p. 525
(defparameter *Etab6.1* '(1 1 1 1 1 0 1 1 1 0 0 0))

;; example 2 of table 6 of Essens (1995) p. 525
(defparameter *Etab6.2* '(1 0 0 1 1 1 1 0 1 1 0 0))


;; C-score: weighted combination of the number of clock ticks that coincide with unaccented 
;; events and with silence: 
;; C = W*s + u 
;; s number of clock ticks coinciding with silence.
;; u number of unaccented events.
(defun C-score (rhythm clock W)
  (+ (* W (nr-of-silent-beats rhythm clock)) 
     (nr-of-unaccented-beats (unaccented-beats rhythm (grid-to-accents rhythm))  clock)))

;; displays the accents and the rhtythm in a readable way. The optional argument
;; allows you to also print the IOIs. This shows all the accents of the shumelevitch patterns:
(defun display-accents (grid &optional (show-iois))
  (cond (show-iois 
         (format t "accents: ~{~a ~}~%rhythm:  ~{~a ~}~%IOIs:    ~{~a ~}~%~%" (grid-to-accents grid) grid (grid-to-iois grid)))
        (t (format t "accents: ~{~a ~}~%rhythm:  ~{~a ~}~%" (grid-to-accents grid) grid))))

;; displays the results of the clock model on a rhythm (essens 1995, which is the first part 
;; of the Povel and Shumelevich model (C-score)in the same fasion as table of essens 1995 (p. 520)
;; The default value of the C-score-weight is 4, becaus in essens '95 they use 4
(defun display-clock-model (rhythm c-score-weight &optional (max-unit 4))
  (display-accents rhythm)
  (format t "clock: ~45t0ev~50t-ev~55tC*~%")
  (let ((possible-clocks (generate-possible-clocks (length rhythm) max-unit)))
    (loop for clock in possible-clocks
          for i to (length possible-clocks)
          do(let ((-ev (nr-of-silent-beats rhythm clock)) 
                  (0ev (nr-of-unaccented-beats (unaccented-beats rhythm (grid-to-accents rhythm))  clock))
                  (c-score (c-score rhythm clock c-score-weight)))
              (format t "   ~a     ~{~a ~}~45t~a~50t~a~55t~a ~%" i clock 0ev -ev c-score))))
  (let ((best-clock (select-best-clock rhythm C-score-weight max-unit)))
    (format t "~%~{~a: ~a ~a: ~a~}" best-clock)))

;; display's the best c-scores in a fashionable way, better use ps-measure
(defun display-best-c-scores (list-with-rated-patterns c-score-weight)
  (let* ((patterns (mapcar #'second list-with-rated-patterns))
        (ratings (mapcar #'car list-with-rated-patterns))
        (c-scores (mapcar #'(lambda (x) (getf (select-best-clock x c-score-weight) :c-score)) patterns)))
    (format t "number:~8trhythm:~40trating:~50tC-score~%")
    (loop for pattern in patterns
          for rating in ratings
          for c-score in c-scores
          for i to (length patterns)
          do (format t "~a~8t~a~40t~a~50t~a~%" i pattern rating c-score ))))

(defun best-c-score-measures (rating-and-patterns c-score-weight)
  "Calculate and display c-score measures and return them as a list scaled from 1-5"
  (loop ; over all patterns
     for (rating pattern) in rating-and-patterns
     with c-score-measure-of-pattern
     do
       (setf c-score-measure-of-pattern (getf (select-best-clock pattern c-score-weight) :c-score))
       (format t "Rhythm ~A Rated ~,2f ~,2f~%" pattern rating c-score-measure-of-pattern)
     collect c-score-measure-of-pattern into all-c-score-measures
     finally (return (scale-to-shmulevich-ratings all-c-score-measures))))
	 
;; counts the number of clock beats that coincide with silence
(defun nr-of-silent-beats (rhythm clock &optional (score 0) )
  (cond ((or (eq (car rhythm) nil) (eq (car clock) nil)) score)
        ((and (eq (car rhythm) 0) (eq (car clock) 1 ))             ; if there is a clock beat coincide with silence
         (nr-of-silent-beats (cdr rhythm) (cdr clock) (+ score 1))); score++
        (t (nr-of-silent-beats (cdr rhythm) (cdr clock) score))))

; counts the number of clock beats that coincide with an unaccented note.
(defun nr-of-unaccented-beats (unaccented-beats clock &optional (score 0))
  (cond ((or (eq (car unaccented-beats) nil) (eq (car clock) nil)) score)
        ((and (eq (car unaccented-beats) 1) (eq (car clock) 1 )) 
         (nr-of-unaccented-beats  (cdr unaccented-beats) (cdr clock) (+ score 1)))
        (t (nr-of-unaccented-beats (cdr unaccented-beats) (cdr clock) score))))

;returns a list with (only) the notes that are unaccented.              
(defun unaccented-beats (rhythm accents &optional (unaccented-beats '()))
  (cond ((eq (car rhythm) nil) unaccented-beats)
        ((and (eq (car rhythm) 1) (eq (car accents) 0)) 
         (unaccented-beats (cdr rhythm) (cdr accents) (append unaccented-beats '(1))))
        (t (unaccented-beats (cdr rhythm) (cdr accents) (append unaccented-beats '(0))))))

;; this method gets a string and returns a string with (only) the accented beats
;; Povel and shumulevitch use the following accent rule: Tones that are 
;; preceded or followed by relatively long tone onset time intervals are perceived as more 
;; salient than others, except for the first of a cluster of two tones. In two-tone clusters, 
;; the second tone is perceived as more accented than the first (Povel & Okkerman, 1981).
(defun iois-to-accents (iois smu &optional (accents '()))
  (let ((ioi (car iois)))
        (cond ((eq ioi nil) (remove-two-tone-accents (reverse (cdr (reverse accents)))))
              ((> ioi smu) 
               (iois-to-accents (cdr iois) smu (last-1-overides accents (ioi-to-notes ioi))))
              (t (iois-to-accents (cdr iois) smu (last-1-overides accents (make-list (+ ioi 1) :initial-element 0)))))))

;; removes the two-tone cluster accents 
;; remove-two-tone-accents '(1 0 0 0 1 1 0 1 1 0 0 1 0)) -> (1 0 0 0 0 1 0 0 1 0 0 1 0)
(defun remove-two-tone-accents (accents &optional (without-two-tone '()))
  (cond ((eq (car accents) nil) without-two-tone)
        ((and (eq (car accents) 1) (eq (cadr accents) 1)) 
         (remove-two-tone-accents (cdr accents) (append without-two-tone '(0))))
        (t (remove-two-tone-accents (cdr accents) (reverse (cons (car accents) (reverse without-two-tone)))))))

;; transforms a inter onset interval to the two notes and the interval it represents.
;; (ioi-to-notes 1) -> (1 1)
;; (ioi-to-notes 3) -> (1 0 0 1)
(defun ioi-to-notes (ioi)
  (cons 1 (append (make-list (- ioi 1) :initial-element 0) '(1))))

;; This method appends to list and checks if the last bit of the fist OR 
;; the first bit of tha last is a 1. After all a beat is accented if 
;; it is preceded OR followed by a large onset interval.
;;(last-1-overides '(1 0 0 0) '(0 0 1)) -> (1 0 0 0 0 1)
;;(last-1-overides '(1 0 0 1) '(0 0 1)) -> (1 0 0 1 0 1)
;;(last-1-overides '(1 0 0 0) '(1 0 1)) -> (1 0 0 1 0 1)
;;(last-1-overides '(1 0 0 1) '(1 0 1)) -> (1 0 0 1 0 1)
(defun last-1-overides (accents to-be-appended)
  (cond ((eq (car accents) nil) to-be-appended)
        ((eq (car (last accents)) 1) (append accents (cdr  to-be-appended)))
        (t (reverse (append (reverse to-be-appended) (cdr (reverse accents)))))))

; calculates the inter-onset intervals (IOIs) from the onset grid
; (grid-to-iois '(1 1 0 1 0 1 1 1 0 1 0 0)) -> (1 2 2 1 1 2 3)
(defun grid-to-iois (grid &optional (ioi 1) (iois '()))
  (cond ((eq (cdr grid) nil) (reverse (cons ioi iois)))
        ((eq (car grid) (cadr grid)) (grid-to-iois (cdr grid) 1 (cons ioi iois))) 
        (t (grid-to-iois (cons (car grid) (cdr (cdr grid))) (+ ioi 1) iois))))

;; returns from an iois string the smallest metrical unit
;; (smallest-metrical-unit '(1 2 2 1 1 2 3)) -> 1
(defun smallest-metrical-unit (iois)
  (reduce #'min iois))

;; returns the list with accenst from the grid.
;;(grid-to-accents '(1 1 0 1 0 1 1 1 0 1 0 0)) -> (0 1 0 1 0 1 0 1 0 1 0 0)
(defun grid-to-accents (grid)
  (let ((iois (grid-to-iois grid)))
    (iois-to-accents iois (smallest-metrical-unit iois))))



;; generates a clock given the length of the list, the clock unit and the starting posistion
;; of the first beat. N.B. start-pos < unit
;; if the unit and the length are not competable, nil is returned instead
;; (generate-clock 4 16 2) -> (0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0)
(defun generate-clock (unit length start-pos)
  (cond ((not (equal (mod length unit) 0)) nil)
        (t (append (make-list start-pos :initial-element 0)
                   (loop repeat (- (/ length unit) 1)
                         nconc (cons 1 (make-list (- unit 1) :initial-element 0)))
                   (cons 1 (make-list (- unit (+ start-pos 1)) :initial-element 0))))))

;; generates a list with all the possible clocks given the desired length.
;; one can optional specify the maximum clock-unit size (default 4, this means
;; that 1 clock beat has a duration of 4 list positions: ( 1 0 0 0 1 0 0 0 1 0 0 0 ... ))
;; in competable unit length combinations are filtered out so:
;; (generate-possible-clocks 3 16 1) -> nil
(defun generate-possible-clocks (length &optional (max-unit 4))
  (loop for i from 2 to max-unit 
        nconc (loop for j from 0 to (- i 1)
                    collect (generate-clock i length j ) into clocks
                    finally (return (remove-if #'(lambda (x)(equal x nil)) clocks)))))

;; generates the list with possible clocks given the maximum unit size (default 4)
;; Afterwards it checks which of these clocks has the highest C-score and finally 
;; it will return the desired "best" clock.
;; N.B this method returns a PROPERTY LIST with the best clock and the best c-score!
;; options:
;; c-score-weight: You can give an other c-score-weight as an argument (default 
;; is the weight used in essens 1995: 4)
;; max-unit: You can also specify the maximum clock unit size (see generate possible clocks for details, default: 4)
(defun select-best-clock (rhythm c-score-weight &optional (max-unit 4))
  (let* ((possible-clocks (generate-possible-clocks (length rhythm) max-unit))
         (c-scores (mapcar #'(lambda (x) (c-score rhythm x c-score-weight)) possible-clocks))
         (best-c-score-index (get-best-index c-scores)))
    (loop for item in possible-clocks
          counting item into index
          until (eq index best-c-score-index)
          finally (return (list :best-clock item :c-score (c-score rhythm item c-score-weight))))))

;; returns the index of the element with the lowest (c-)score in the list (starting the index from 1)
;; (get-best-index '(1 8 5 9 2)) -> 1
(defun get-best-index (list)
  (let ((max-value (apply 'min list)))
     (loop for item in list 
           counting item into index
           until (eq max-value item)
           finally (return index))))

;;; Plotting routine
(defun plot-c-score-comparisons (listener-rated-patterns c-score-weight meter &optional (figure-number 1))
  ;; Re-sort into increasing rating judgements.
  (let ((syncopation-test-results (syncopation-test listener-rated-patterns meter))
	(c-score-measures (best-c-score-measures listener-rated-patterns c-score-weight))
	(listener-ratings (mapcar #'first listener-rated-patterns)))
    (plot-initialise)
    (plot-command "set key left")
    (plot (cons listener-ratings (cons c-score-measures syncopation-test-results))
	:labels '("Listener Ratings" "Essens C-score" "Longuet-Higgins & Lee Measure" "P&K Measure (Musicians)" "P&K (Non-musicians)")
	:title "Comparison of Psychological and Modelled C-Score Measures"
	:figure-number figure-number)))

; (plot-c-score-comparisons (sort *shmulevich-patterns* #'< :key #'first) 4.0 '(2 2 2 2))
