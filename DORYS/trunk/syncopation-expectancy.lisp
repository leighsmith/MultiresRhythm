;; HH:

;; Alternatively, I would stick to metrical expectation, enhancing th eICMPC paper with more
;; baseline models and convincign examples: For instance, rhythms that will be consdirend
;; relatively unsyncopated acoording to a histogram/zero-order expectancy, and predicted
;; syncopated accordign to the MRR model (or vice versa.) A second line of the paper could
;; well be to show that models of explactany need to be (at least) n-th order (if not of the
;; complexity of MRR), and that a zero-order one (like implicitly proposed by Palmer &
;; Krumhansl and in some of the work of Huron) is not enough to capture metric expectancy. In
;; a way, it is seems also the point of Temperly (2007:35 and onward), but worded very
;; unclear, not? But I'm sure, by now, you have quite an overview of the literature yourself.

;; That is, Compare histogram vs. nth order models.

;; Syncopated rhythms can be compared by:
;; 1. Use LH&L to determine syncopation measures
;; 2. Perhaps use the S&P set?
;; 3. Determine expectancy patterns at points in the rhythms to measure the syncopation.

(in-package :dorys)
(use-package :syncopation)
(use-package :multires-rhythm)
(use-package :prob-downbeat)

#|
(defun (pattern  &key (sample-rate 200))
(syncopation-of-pattern pattern '(2 2 2 2) 'lh-metric-salience)
(setf test-rhythm (multires-rhythm:rhythm-of-grid "pattern" pattern :sample-rate sample-rate))

(hear (multires-rhythm:rhythm-of-grid "pattern" (multires-rhythm::repeat-rhythm '(1 1 0 1 1 0 1 1 1 1 1 0 1 0 0 0) 2)
				      :shortest-ioi 50 :sample-rate 200))

(syncopation-of-pattern '(1 1 0 1 1 0 1 1 1 1 1 0 1 0 0 0) '(2 2 2 2) 'lh-metric-salience)

(last-expectations (multires-rhythm:rhythm-of-grid "pattern" (multires-rhythm::repeat-rhythm '(1 1 0 1 1 0 1 1 1 1 1 0 1 0 0 0) 2)
				      :shortest-ioi 50 :sample-rate 200))
|#

(defun syncopation-for-corpus (corpus)
  "Generate syncopation pattern measures for the corpus"
  (loop
     for piece in corpus
     for annotated-rhythm = (read-analysed-rhythm (first piece)
						  :rhythm-directory-root *quaero-selection-directory*)
     for syncopation-profile = (syncopation::eval-syncopation-measures annotated-rhythm)
     do
       (prob-downbeat::write-syncopation (first piece) syncopation-profile :rhythm-directory-root *quaero-selection-directory*)
     collect syncopation-profile))

   
