(defgeneric rhythm-complexity (rhythm-to-analyse)
  (:documentation "Returns a normalised measure of the complexity of the rhythm,
   where 0 = impossibly simple -> 1 = impossibly hard."))

;; Approaches:
;; 1. Count the number of ridges.
;; 2. Each ridge is tempo weighted by each of it's scales. Therefore 1 long ridge would be more complex than shorter ridges.
;; 3. Weight the entire scale peaks or magnitude measure by tempo.
;; 4. Ease of handclapping as a measure of complexity?

;;; Could use a weighted bitmap of the ridges as a general density measure. This is
;;; weighted by absolute tempo. It should also be weighted by the number of ridges (a
;;; dense single ridge is less complex than one which is composed of many small ridges.
(defmethod rhythm-complexity ((rhythm-to-analyse rhythm))
  "Returns a normalised measure of the complexity of the rhythm, where 0 = impossibly simple -> 1 = impossibly hard."
    (length (ridges (skeleton (analysis-of-rhythm rhythm-to-analyse)))))

