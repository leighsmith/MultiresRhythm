;;; -*- Lisp -*-

;;; A percussive sample (a hi-hat) normally used in time keeping
;;; functions of western popular music. We just use an existing
;;; sampler instrument.
; for some reason the Linux version doesn't like ~
(load "~/Sources/Music/CommonMusic/instruments/sampler.fas")

#|
;;;
;;; Beat rates caused by phase shifting slightly different frequency
;;; isochronous beats wrt one another.
;;; TODO Work out how many beats will pass before synchronisation
;;;
(merge badcock-beating-rate-motion-periodicity-pitch ()
 (generator badcock-periodicity-static-rate rhythm-periodicity-pitch
	    (length 80 
		    panning 0.5
		    amplitude 0.5 duration 0.600 note 'a4 rhythm 0.600))
 (generator badcock-periodicity-moving-rate rhythm-periodicity-pitch
	    (length 80 
		    panning 0.5
		    amplitude 0.5 duration 0.500 note 'a4 rhythm 0.500)))
|#

;;;
;;; Beat rates caused by mixing a static rate with a rising beat
;;; frequency rate. We hold the static rate at twice the rate of the
;;; peak of tempo of pulse sensation (fraisse:rhythm)
;;; 
(generator badcock-periodicity-static-rate rhythm-periodicity-pitch
	   (length 80 panning 0.5 amplitude 0.5 note 'a4)
	   (setf rhythm 0.300)
	   (setf duration rhythm))

;;;
;;; Need rhythm to start at 800mS and move to 200mS over the
;;; length of the rhythm.
;;;
(thread badcock-periodicity-increasing-rate-thread () 
	(loop for beat-number from 0 to 100 do
	      (let ((r (interp beat-number '(0 0.800 100 0.200))))
		(object rhythm-periodicity-pitch
			note 'a4
			rhythm r
			duration r
			amplitude 0.5
			panning 0.5))))

;;
;; Use a percussive sample instead.
;;
(merge badcock-beating-rate-motion-percussive ()
 (generator badcock-percussive-static-rate sampler
	    (length 80 
		    sample-file "~/Library/Sounds/hihat_closed.aiff"
		    panning 0.0
		    amplitude 0.5 duration 0.158 rhythm 0.600))
 (generator badcock-percussive-moving-rate sampler
	    (length 80 sample-file "~/Library/Sounds/hihat_closed.aiff"
		    panning 1.0
		    amplitude 0.5 duration 0.158 rhythm 0.580)))
