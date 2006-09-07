;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$
;;;
;;; Longuet-Higgins & Lee 1984 Syncopation Measure 
;;; as described in Longuet-Higgins & Lee (1984) Music Perception, pp 424-441.
;;; In Common Lisp 
;;;
;;; (c) 2002, honing@uva.nl / http://www.hum.uva.nl/mmm/
;;;
;;; Adapted to incorporate Palmer & Krumhansl Metrical Hierarchy model
;;; by Leigh M. Smith <lsmith@science.uva.nl> 2005 

(in-package :syncopation)

;;; toplevel function

(defun syncopation-LHL84 (rhythm meter)
  "Return syncopation strengths, or NIL if meter cannot accomodate rhythm.
   0=no syncopation, other values=LHL84 strengths + 1"
  (when (can-meter-accomodate-rhythm? meter rhythm)
    (calculate-syncopations (iois-to-onsets (scale-rhythm rhythm meter)) meter)))

;;; Examples in Figure 9
;(syncopation-lhl84 '(1 3) '(2 2))        -> (2),      lhl84: 1
;(syncopation-lhl84 '(2 1 3 2) '(2 2 2))  -> (0 3 0) , lhl84: 2
;(syncopation-lhl84 '(3 1) '(2 2))        -> (0) ,     lhl84: no sync
;(syncopation-lhl84 '(2 1) '(3))          -> (0) ,     lhl84: no sync
;(syncopation-lhl84 '(1 2) '(3))          -> (1) ,     lhl84: 0

;;; LMS examples
;(syncopation-lhl84 '(3 1 2) '(2 3))  -> (0 1) lhl84 Ex 4(b): 
;(syncopation-lhl84 '(1 2 1) '(2 2))  -> (2)
;(syncopation-lhl84 '(1 2 1 1 3) '(2 2 2)) -> (2 2)
;(syncopation-lhl84 '(1 4 3) '(2 2 2)) -> (3 2)
;(syncopation-lhl84 '(5 3) '(2 2 2)) -> (3 2)
;(syncopation-lhl84 '(1 5 3) '(3 2 2)) -> (3 2)
;(syncopation-lhl84 '(1 2 2 2 1) '(2 2 2)) -> (2 3 2)

;;; kacharpari in 2/4
;(syncopation-lhl84 '(1 2 1 2 2) '(2 2 2))
;(syncopation-lhl84 '(1 2 1 2 2 2 2 4) '(2 2 2 2))

;;; greensleeves in 6/8
;(syncopation-lhl84 '(4 2 3 1 2) '(2 3 2))

;(syncopation-lhl84 '(2 1 1 2 4 2 4) '(2 2 2 2))

;;; Examples in Figure 14
;(syncopation-lhl84 '(4 1 1 4 1 1 2 1 2 1 3 3) '(2 3)) ; example 17

;;; calculate syncopations

(defun calculate-syncopations-on-grid (rhythm meter &optional (metric-salience 'lh-metric-salience))
  "Return list of syncopation strengths"
  (loop with salience = (funcall metric-salience meter)
     for position from 0 below (length salience)
     when (and (note? (elt rhythm position))             ; note,
	       (not (note? (elt rhythm (1+ position))))) ; followed by rest
     collect (syncopation (elt salience position)        ; metric-salience note
			  (metric-salience-rest position rhythm salience))))

(defun calculate-syncopations (onsets meter &optional (metric-salience 'lh-metric-salience))
  "Return list of syncopation strengths"
  (calculate-syncopations-on-grid (onsets-to-grid onsets) meter metric-salience))

;; (calculate-syncopations '(0 2 3 6 8) '(2 2 2) 'lh-metric-salience) -> (0 3 0)

;;; calculating metric salience the Longuet-Higgins & Lee way.
(defun lh-metric-salience (meter)
  "Return list of metric weights; 0 highest level, -n lowest"
  (loop with grid-length = (apply #'* meter)
        with salience = (make-list grid-length :initial-element nil)
        initially (setf (elt salience 0) 0)
        for division in meter
        as level = -1 then (1- level)
        as interval = (/ grid-length division) then (/ interval division)
        do (loop for pos from 0 below grid-length by interval
                 do (unless (elt salience pos) (setf (elt salience pos) level)))
        finally (return salience)))
  
;;; now:
; 3/4 (lh-metric-salience '(3 2)) -> (0 -2 -1 -2 -1 -2)
; 4/4 (lh-metric-salience '(2 2 2)) -> (0 -3 -2 -3 -1 -3 -2 -3)
; 6/8 (lh-metric-salience '(2 3)) -> (0 -2 -2 -1 -2 -2)
; 9/8 (lh-metric-salience '(3 3)) -> (0 -2 -2 -1 -2 -2 -1 -2 -2)
;12/8 (lh-metric-salience '(2 2 3)) -> (0 -3 -3 -2 -3 -3 -1 -3 -3 -2 -3 -3)
; 3/2 (lh-metric-salience '(3 2 2)) -> (0 -3 -2 -3 -1 -3 -2 -3 -1 -3 -2 -3)

;;; Associate Palmer & Krumhansl's perceptual hierarchical data to
;;; Longuet-Higgins & Lee's representation of meters.
;;;
;;; Keys are tuples combining meter, expertise and (TODO) tempo.
;;;
;;; The original values are taken from Palmer & Krumhansl's
;;; responses to perceptual hierarchy experiment.
;;;
(defparameter *pk-meter-table* (make-hash-table :test 'equal))

;;; 4/4 duration 3.488 seconds
(setf (gethash '((2 2 2 2) musician) *pk-meter-table*) 
      '(6.14 3.98 4.22 3.32 5.88 2.92 4.36 3.40 5.68 2.72 3.02 3.12 5.38 3.26 4.72 4.30))

;;; TODO this needs to be distinguished by tempo.
;;; 2/4 duration 1.744 seconds
;;; (setf (gethash '((2 2 2) musician) *pk-meter-table*) 
;;;       '(5.97 2.69 3.79 3.14 5.04 3.59 4.62 4.07 6.14 3.59 3.00 3.52 4.90 4.24 4.00 3.04))

;;; 3/4 duration 2.400 seconds
(setf (gethash '((3 2 2) musician) *pk-meter-table*) 
      '(6.44 2.72 4.76 3.25 6.06 2.86 4.23 3.44 5.86 2.72 4.69 3.32))

;;; 6/8 duration 4.800 seconds
(setf (gethash '((2 3 2) musician) *pk-meter-table*) 
      '(6.11 4.09 5.09 2.95 4.92 3.16 5.61 3.71 4.07 3.02 5.11 4.57))

;;; TODO this needs to be distinguished by tempo.
;;; 2/4
;; (setf (gethash '((2 2 2) nonmusician) *pk-meter-table*) 
;;       '(4.424658 3.7945206 4.150685 3.739726 3.630137 4.424658 4.671233
;; 	5.0547943 5.191781 4.6164384 4.150685 4.1232877 4.2054796
;; 	3.4931507 4.452055 3.6027398))

;;; 4/4
(setf (gethash '((2 2 2 2) nonmusician) *pk-meter-table*)
      '(4.0993586 3.4437249 3.74174 3.7218723 5.172213 3.3443863
	3.2053127 3.9205492 4.735124 4.4768443 3.4238572 4.00002
	5.2318163 3.8212106 4.238432 4.298035))

;;; 3/4
(setf (gethash '((3 2 2) nonmusician) *pk-meter-table*)
      '(4.2093024 3.7441862 3.6744187 4.116279 5.3255816 3.511628
	4.2325583 4.0697675 5.3488374 3.3488371 3.2093024
	3.5348837))

;;; 6/8
(setf (gethash '((2 3 2) nonmusician) *pk-meter-table*)
      '(4.3095236 3.6666667 5.3095236 3.452381 4.642857 3.1666667
	4.8095236 3.5238094 4.9761906 4.0238094 4.8809524
	4.595238))

;; Conversion procedure: normalize the hierarchy values. Once
;; normalized, subtract from 1 so we match the sign of the values
;; expected by the LH&L model. Multiply by the number of hierarchical
;; levels of the meter.
(defun perceptual-hierarchy-to-salience (hierarchy hierarchy-depth)
  "Converts from Palmer and Krumhansl's perceptual hierarchy data
  (rated on a 7 point scale) to metrical salience measures"
  (let ((hierarchy-max (apply #'max hierarchy))
	(hierarchy-min (apply #'min hierarchy)))
    (mapcar #'(lambda (x) 
		(* (1- (/ (- x hierarchy-min)
			  (- hierarchy-max hierarchy-min))) hierarchy-depth))
	    hierarchy)))

;; (perceptual-hierarchy-to-salience *pk-34* (length '(3 2 2))) ->
;; (0.0 -3.0 -1.3500001 -2.5687501 -0.30000007 -2.8875 -1.78125 -2.41875 -0.46875018 -3.0 -1.4062502 -2.5125)
;; (perceptual-hierarchy-to-salience *pk-44* (length '(2 2 2 2))) ->
;; (0.0 -2.5263157 -2.2456143 -3.2982457 -0.30409336 -3.7660818 -2.081871 -3.2046783 -0.5380118 -4.0 -3.6491227 -3.5321639 -0.8888886 -3.368421 -1.6608188 -2.1520467)

(defun match-first (target candidate)
  "Checks if the first part of a candidate list matches the target,
  candidate can be longer than target"
  (if (null target)
      t
      (if (equal (first target) (first candidate))
	  (match-first (rest target) (rest candidate))
	  nil)))

;; Only the canonical meter matching the number of levels represented in the P&K data is valid.
;; We therefore attempt to match against subsets of all known hash keys.
(defun canonical-meter (meter expertise meter-table)
  "given a meter and expertise, return the canonical meter it matches in the supplied table of meters"
  ;; traverse the hash table keys (which are the canonical meters)
  (loop 
     for canonical-meter-candidate 
     being the hash-keys of meter-table
     when (and (match-first meter (first canonical-meter-candidate))
	       (equal expertise (second canonical-meter-candidate)))
     return (first canonical-meter-candidate)))

(defun pk-metric-salience (meter &optional (expertise 'musician) (meter-table *pk-meter-table*))
  "Return a list of metric weights derived from Palmer & Krumhansl's metrical perceptual
hierarchy data. Experiment 2. We default to  using the musician data"
  ;; Determine the canonical meter from that supplied.
  (let* ((canonical-meter (canonical-meter meter expertise meter-table))
	 ;; Determine the right perceptual hierarchy profile from the
	 ;; supplied meter (TODO and tempo).
	 (perceptual-hierarchy (gethash (list canonical-meter expertise) meter-table)))
    (loop
       ;; The length of the meter subdivision list matches the number of hierarchical levels of the meter.
       with entire-salience = (perceptual-hierarchy-to-salience perceptual-hierarchy (length canonical-meter))
       with grid-length = (apply #'* meter)
       with data-length = (length entire-salience)
       for pos from 0 below data-length by (/ data-length grid-length)
       collecting (elt entire-salience pos))))

; 3/4 (pk-metric-salience '(3 2)) -> (0.0 -1.3548385 -0.30645168 -1.7822582 -0.46774185 -1.4112902)
; 4/4 (pk-metric-salience '(2 2 2)) -> (0.0 -2.2456143 -0.30409336 -2.081871 -0.5380118 -3.6491227 -0.8888886 -1.6608188)
; 6/8 (pk-metric-salience '(2 3)) -> (0.0 -0.96835434 -1.1297468 -0.47468358 -1.9367087 -0.949367)
; 9/8 (pk-metric-salience '(3 3)) ->

; 3/4 (pk-metric-salience '(3 2) *pk-nonmusician-meter-table*) -> (-1.5978262 -2.347826 -0.032608688 -1.5652174 0.0 -3.0)

(defun pk-nonmuso-metric-salience (meter)
  (pk-metric-salience meter 'nonmusician *pk-meter-table*))

; 3/4 (pk-nonmuso-metric-salience '(3 2)) -> (-1.5978262 -2.347826 -0.032608688 -1.5652174 0.0 -3.0)

;;; utilities
          
(defun syncopation (note-weight rest-weight) 
  (cond ((> note-weight rest-weight)
         0)
        ((= note-weight rest-weight)
         1)
        (t (1+ (- rest-weight note-weight)))))

(defun note? (value) (= value 1))

(defun scale-rhythm (rhythm meter)
  "Return rhythm scaled such that it fits length of metric grid"
  (let* ((sum (apply #'+ rhythm))
         (grid (apply #'* meter))
         (factor (/ grid sum)))
    (if (= factor 1)
      rhythm
      (mapcar #'(lambda (x) (* x factor)) rhythm))))

;(scale-rhythm '(2 3 1) '(2 2 3)) -> (4 6 2)
;(scale-rhythm '(2 1 1 2 2 1 1 1 1) '(2 2))

(defun metric-salience-rest (position rhythm salience)
  "Return highest metric-salience in note-to-note interval"
  (loop for pos from (1+ position) below (next-note-position rhythm position)
        maximize (elt salience pos)))

;(metric-salience-rest 1 '(1 1 0 0 0 1) '(0 -2 -1 -2 -1 -2)) -> -1

(defun next-note-position (rhythm position)
  (loop for pos from (1+ position) below (length rhythm)
        when (note? (elt rhythm pos))
        do (return pos)))

;(next-note-position '(1 1 0 0 1) 1) -> 4
        
(defun iois-to-onsets (iois &optional (onset 0))
  (if iois
    (cons onset (iois-to-onsets (rest iois) (+ onset (first iois))))
    (list onset)))

;(iois-to-onsets '(1 3 2)) -> (0 1 4 6)

(defun onsets-to-grid (onsets)
  (loop with rhythm = (make-list (1+ (first (last onsets))) :initial-element 0)
        for onset in onsets
        do (setf (elt rhythm onset) 1)
        finally (return rhythm)))

;(onsets-to-grid '(0 1 4 6)) -> (1 1 0 0 1 0 1)

(defun can-meter-accomodate-rhythm? (meter rhythm)
  (let ((sum (apply #'+ rhythm))
        (grid (apply #'* meter)))
    (zerop (mod sum grid))))

;(can-meter-accomodate-rhythm? '(2 3) '(1 3 2)) -> t
;(can-meter-accomodate-rhythm? '(2 2) '(1 3 2)) -> nil

