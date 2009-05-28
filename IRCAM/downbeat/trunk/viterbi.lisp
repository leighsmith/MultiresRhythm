;;;; -*- Lisp -*-
;;;;
;;;; $Id$
;;;;
;;;; Implementation of the Viterbi decoding algorithm.
;;;; See L. Rabiner. A tutorial on hidden Markov models and selected applications in speech recognition. 
;;;; Proceedings of the IEEE, 77(2):257–286, 1989.
;;;; Ported from original matlab version written by G. Peeters
;;;;
;;;; In nlisp (Matlab-alike Common Lisp library www.nlisp.info)
;;;;
;;;; By Leigh M. Smith <leigh.smith@ircam.fr> 
;;;;
;;;; Copyright (c) 2009 All Rights Reserved.
;;;;

(declaim (optimize (speed 3) (safety 0) (debug 0)))
;; (declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :prob-downbeat)
(use-package :nlisp)
(use-package :multires-rhythm)

;;; TODO we currently assume an identity matrix for the emission probabilities.
(defun viterbi (initial-probabilities probability-of-observations state-transition-probabilities)
  "Viterbi decoding algorithm. Returns the path of transitions between states across time and the likelihood.
  Observations of states and times are specified by probabilities-of-observations,
  probability of transition from one state to another is given by
  state-transition-probabilities, primed by initial-probabilities for each state"
  ;; Perform the operations in the log domain to save multiplications. There is probably a
  ;; minimum time length before this becomes computationally beneficial.
  (let* ((state-time-dimensions (.array-dimensions probability-of-observations))
	 (number-of-states (first state-time-dimensions))
	 (number-of-times (second state-time-dimensions))
	 (epsilon 0.000001d0) ; avoids log 0 computations.
	 (log-prob-observe (.log (.+ probability-of-observations epsilon)))
	 (log-prob-transitions (.log (.+ state-transition-probabilities epsilon)))
         (maximising-state (make-integer-array state-time-dimensions))
	 (maximum-likelihood (make-double-array state-time-dimensions))
	 (state-path (make-integer-array number-of-times))
	 most-likely-state
	 state-likelihood)
    (declare (type fixnum number-of-times number-of-states))
    ;; 1) Initialisation:
    (setf (.column maximum-likelihood 0) (.+ (.log initial-probabilities) (.column log-prob-observe 0)))
    ;; 2) Recursion: Compute the forward maximum transition probabilities
    (loop for time fixnum from 1 below number-of-times
       do (loop for state fixnum from 0 below number-of-states
	     ;; Retrieve the enumeration of the transition probabilities, given the current state.
	     for transition-to-state = (.column log-prob-transitions state)
	     ;; Calculate the maximum transition probability from states at previous time to the current state.
	     for transition-likelihoods = (.+ (.column maximum-likelihood (1- time)) transition-to-state)
	     for max-transition-index = (argmax transition-likelihoods)
	     do
	       (setf (.aref maximising-state state time) max-transition-index)
	       ;; Compute the cumulative probability
	       (setf (.aref maximum-likelihood state time) 
		     (+ (.aref maximum-likelihood max-transition-index (1- time))
			(.aref transition-to-state max-transition-index)
			(.aref log-prob-observe state time)))))
    ;; (format t "Maximising state ~a~%" maximising-state)
    ;; 3) Termination: Select the maximum ending probability
    (setf most-likely-state (argmax (.column maximum-likelihood (1- number-of-times))))
    (setf state-likelihood (.aref maximum-likelihood most-likely-state (1- number-of-times)))
    ;; 4) Path (state sequence) backtracking
    (loop
       initially (setf (.aref state-path (1- number-of-times)) most-likely-state)
       for time from (- number-of-times 2) downto 0
       do (setf (.aref state-path time) (.aref maximising-state (.aref state-path (1+ time)) (1+ time))))
    (values state-path state-likelihood)))

;;;
;;; Tests
;;;
#|
(setf test-observations (make-narray '((0.1 0.4 0.1 0.2 0.1 0.1 0.3 0.3)
				       (0.7 0.4 0.5 0.3 0.2 0.2 0.1 0.2)
				       (0.1 0.1 0.2 0.4 0.6 0.3 0.5 0.4)
				       (0.1 0.1 0.2 0.1 0.1 0.4 0.1 0.1))))

(setf init-probs (make-narray '(0.25 0.25 0.25 0.25)))
(setf transition-probs (make-narray '((0.9 0.0333 0.0333 0.0333)
				      (0.0333 0.9 0.0333 0.0333)
				      (0.0333 0.0333 0.9 0.0333)
				      (0.0333 0.0333 0.0333 0.9))))

(viterbi init-probs test-observations transition-probs)
;;; Produces a path NLISP #(1 1 1 2 2 2 2 2) -11.627295282373277d0 matching the matlab code. 


|#
