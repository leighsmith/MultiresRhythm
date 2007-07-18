;;;; Profile skeleton-of-rhythm

(in-package :multires-rhythm)
(use-package :nlisp)
;; (require 'sb-sprof)

(time (skeleton-of-rhythm (anthem-rhythm (anthem-named 'australia))))
;; Length of Rhythm 38.5 seconds
;; CWT input signal length 7700 samples
;; Maximum time period analysed = 2048 samples, dyadic length = 8192 samples
;; Finished CWT, 144 scales, last time period = 2048 samples
;; Evaluation took:
;;   174.861 seconds of real time
;;   126.65817 seconds of user run time
;;   33.794704 seconds of system run time
;;   [Run times include 7.951 seconds GC run time.]
;;   0 calls to %EVAL
;;   0 page faults and
;;   11,079,499,912 bytes consed.

(setf x (time (scaleogram-of-rhythm (anthem-rhythm (anthem-named 'australia)))))
;; Length of Rhythm 38.5 seconds
;; CWT input signal length 7700 samples
;; Maximum time period analysed = 2048 samples, dyadic length = 8192 samples
;; Finished CWT, 144 scales, last time period = 2048 samples
;; Evaluation took:
;;   58.67 seconds of real time
;;   39.680832 seconds of user run time
;;   13.621586 seconds of system run time
;;   [Run times include 3.63 seconds GC run time.]
;;   0 calls to %EVAL
;;   0 page faults and
;;   4,671,703,488 bytes consed.

(setf crsp (time (scale-peaks-of-scaleogram x 200)))
;; Evaluation took:
;;   112.159 seconds of real time
;;   83.08438 seconds of user run time
;;   20.42316 seconds of system run time
;;   [Run times include 5.144 seconds GC run time.]
;;   0 calls to %EVAL
;;   0 page faults and
;;   6,381,644,736 bytes consed.

(setf ridges (time (extract-ridges crsp)))
;; Evaluation took:
;;   5.03 seconds of real time
;;   4.819393 seconds of user run time
;;   0.158742 seconds of system run time
;;   [Run times include 0.027 seconds GC run time.]
;;   0 calls to %EVAL
;;   0 page faults and
;;   23,252,376 bytes consed.

;; correlate-ridges is pretty damn slow.
(setf correlated-ridges (time (correlate-ridges (scaleogram-magnitude x) (scaleogram-phase x) 16)))
;; Evaluation took:
;;   81.474 seconds of real time
;;   59.448395 seconds of user run time
;;   15.602744 seconds of system run time
;;   [Run times include 3.608 seconds GC run time.]
;;   0 calls to %EVAL
;;   0 page faults and
;;   4,761,723,144 bytes consed.

(setf stat-phase (time (stationary-phase (scaleogram-magnitude x) (scaleogram-phase x) 16)))
;; Evaluation took:
;;   40.044 seconds of real time
;;   28.998241 seconds of user run time
;;   7.888868 seconds of system run time
;;   [Run times include 2.621 seconds GC run time.]
;;   0 calls to %EVAL
;;   0 page faults and
;;   2,343,033,032 bytes consed.

(setf local-pc (time (local-phase-congruency (scaleogram-magnitude x) (scaleogram-phase x))))
;; Evaluation took:
;;   28.22 seconds of real time
;;   18.721403 seconds of user run time
;;   5.898597 seconds of system run time
;;   [Run times include 1.564 seconds GC run time.]
;;   0 calls to %EVAL
;;   0 page faults and
;;   1,552,663,616 bytes consed.

(setf normalised-magnitude (time (normalise-by-scale (scaleogram-magnitude x))))
;; Evaluation took:
;;   13.93 seconds of real time
;;   10.738508 seconds of user run time
;;   2.1712 seconds of system run time
;;   [Run times include 0.48 seconds GC run time.]
;;   0 calls to %EVAL
;;   0 page faults and
;;   697,486,152 bytes consed.

;; So is determine-scale-peaks
(time (determine-scale-peaks correlated-ridges))
;; Evaluation took:
;;   2.815 seconds of real time
;;   1.67432 seconds of user run time
;;   0.759131 seconds of system run time
;;   [Run times include 0.232 seconds GC run time.]
;;   0 calls to %EVAL
;;   0 page faults and
;;   204,719,864 bytes consed.

;; .subarray is slow and would greatly benefit the whole system by improvement since it's
;; used so often. In particular, setf-subarray is used more often than the retrieval function.

;; This is too slow and would improve performance dramatically (since it's used so often).
(time (.diff (scaleogram-phase x)))
;; Evaluation took:
;;   1.427 seconds of real time
;;   0.880051 seconds of user run time
;;   0.385719 seconds of system run time
;;   [Run times include 0.101 seconds GC run time.]
;;   0 calls to %EVAL
;;   0 page faults and
;;   115,298,712 bytes consed.

;; Using statistical profiler
(sb-sprof:with-profiling (:max-samples 50000 :report :flat :loop nil) 
  (stationary-phase (scaleogram-magnitude x) (scaleogram-phase x) 16))

;; Using deterministic profiler
(sb-profile:profile STATIONARY-PHASE .SUBARRAY nlisp::setf-subarray .DIFF .ABS .* .- ./ .+)
(sb-profile:report)
(sb-profile:unprofile STATIONARY-PHASE .SUBARRAY nlisp::setf-subarray .DIFF .ABS .* .- ./ .+)

