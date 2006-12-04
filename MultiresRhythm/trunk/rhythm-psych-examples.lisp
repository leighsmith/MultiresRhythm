;;; -*- Lisp -*-
;;;
;;; $Id:$
;;;
;;; Psych test data from the musical rhythm psychology
;;; literature coded as Stella/Common Music programs to act as a set
;;; of stimulus data to compare human and automatic perception
;;; performances.  Leigh Smith 4/7/95

;;; Todo: look at representing rests in rhythms.

;;; Create a macro so we can choose to use whatever instrument is
;;; desired with the consistent rhythmic structures.

;;; Definitely need to conceive and structure these rhythmic examples
;;; in object orientation. That way we model the concept
;;; better. i.e a polyrhythm object, with a number of parameters to
;;; define an instance of a polyrhythm.
;;; Sketch of class hierarchy:
;;; rhythm-example -> polyrhythm-example.
;;; See how best to incorporate common-music item-streams and
;;; generators to reduce coding. Best way is prototype one or two in
;;; continual refinement of representation.


;;; Have these rhythmic tests been performed with fade in's rather than
;;; abrupt starts? Need to ensure the fade in is random so that the
;;; listener does not begin to perceive at the same beat (assuming
;;; fixed tempo). See for example parncutt:pulse for his approach and
;;; discussion of other researchers approaches


;;; rhythm-tone needs a fixed duration about 0.158 seconds long

#|
;;;
;;; From Handel 1989 page 401, beats conforming or not conforming to
;;; natural accents in a 16 beat measure, 1-16 16th notes.
;;;

;;; Beats match naturally accented elements
(setq handel-natural-accent-1 (rhythms 1* 2 3 4 5* 8 9* 11 13*))
(setq handel-natural-accent-2 (rhythms 1* 2 3* 5* 7* 8 9* 12 13*)))
(setq handel-natural-accent-3 (rhythms 1* 3* 4 5* 7* 8 9* 12 13*)))

;;; Beats do not match accented elements
(setq handel-unnatural-accent-1 (rhythms 1* 2 3 4* 6* 7 8* 11* 13*))
(setq handel-unnatural-accent-2 (rhythms 1* 2 3 4* 7 8* 10 11* 13*))
(setq handel-unnatural-accent-3 (rhythms 1 2* 4* 7* 8 9 10 11* 13*))

;;;
;;; Need to be able to specify several lists of: x beats per measure over a
;;; time per measure. Specify a number of measures to perform:
;;; beat-density-example
;;;

;;; Dowling and Harwood 1986 p186
;;; Alternating intervals in ratios between 4:1 and 5:4
( '(4 4 1)) ; (q:q:s)
( '(3 3 2)) ; equal
( '(6 6 4)) ; equal
( '(2 1 1)) ;
( '(3 1))   ; (e.:s)
(setf most-predominant '(2 1))   ; most predominant 
(setf difficult-4-3 '(4 3))   ; difficult
(setf difficult-5-4 '(5 4))   ; difficult
(setf isochronous '(1 1))   ; isochronous
|#

;;; Rhythmical derivatives.
;;; Fraisse, Dowling and Harwood \cite{fraisse:rhythm,dowling:musiccog}
(thread dowling-rhythmic-derivatives ()
	;; anapaest - 3 syllables, 2 unaccented syllables, then an accented one.
	;; 3-element, final accent pattern  
	(generator dowling-anapest rhythm-onset
		   (length 21 amplitude 0.75 note 'a4 duration 0.158)
		   (setf rhythm (item (rhythms e  e  q))))

	;; dactyl - 3 syllables, first accented, then 2 unaccented.
	;; 3-element, initial accent pattern
	(generator dowling-dactyl rhythm-onset
		   (length 21 amplitude 0.75 note 'a4 duration 0.158)
		   (setf rhythm (item (rhythms q  e  e))))

	;; iamb - 2 syllables, first unaccented, second accented.
	;; 2-element, final accent pattern
	(generator dowling-iamb rhythm-onset
		   (length 20 amplitude 0.75 note 'a4 duration 0.158)
		   (setf rhythm (item (rhythms e  q.))))

	;; trochee - 2 syllables, first accented, second unaccented.
	;; 2-element, initial accent pattern
	(generator dowling-trochee rhythm-onset
		   (length 20 amplitude 0.75 note 'a4 duration 0.158)
		   (setf rhythm (item (rhythms q. e))))

	;; amphibrach - 3 syllables, accented syllable between two
	;; unaccented syllables.			
	)

;;; Subjective rhythmisation, Handel p387, extending the interval
;;; between groups to induce accent patterns (Povel & Okkerman 1981).
;;; (interval accents?)
;;; subjective-metricality

;;; Categorical rhythm perception
;;; Handel p402, simplification to(wards) 2:1 ratio beats.
;;; IOI Values in seconds
;; reported as reproduced as 0.27 0.56 0.53
(setf handel-simplification-1 '(0.3  0.45 0.45))

;; reported as reproduced as 0.17 0.67 0.55
(setf handel-simplification-2 '(0.18 0.69 0.45))

;;; Additive Meters
;;; Rhythms of Yugoslavia, Bulgaria & Turkey
;XxxXxxxXxxx

#|
;;;
;;; Incompatible timing and melody rhythms
;;;
;;; eight element binary pitched melody with rests every 3 elements.
XXO-OXO-XOX-XOO-XOX-OXX-OOX-OXO-XXO-

;;;
;;; Viennese waltz timing Handel p417
;;; beat 1 is shortened, beat 2 is lengthed, making it early (wrt metronome)
;;;
;;; In percentages of a measure, calculated on IOI.
(33.3 33.3 33.3)	; extrema: most similar (isochronous)
(31 36 33)
(29 38 33)
(27 40 33)		; preferred.
(25 42 33)
(23 44 33)		; extrema: most different
;;; Tone duration of first beat always longer than 2nd & 3rd to create accent.


;;; jones:attending p213
;;; Interaction of objective and induced accents on melody discrimination tasks
;;; Accents formed from metrically lengthened events.
;;; All times in Seconds.
(setf jones-u :duration 0.250 :articulation 0.050) ; unaccented, IOI = 300mS
(setf jones-A :duration 0.500 :articulation 0.100) ; accented, IOI = 600mS
(setf jones-r :rhythm 0.35)                        ; irregular
|#

; phenomenal accents at 3 & 9, induced at 6
(setf jones-uuA '(u u A u u u r u u A)) 
; phenomenal accents at 1 & 7, induced at 4
(setf jones-Auu '(A u u r u u u A u u))

(setf jones-irreg '(A u u u 0.05 u u A u u))

; no-rule melodies
; rule 1
(setf jones-rule1 '(b3 d4 e g a b a d e))
; rule 2
(setf jones-rule2 '(a3 d4 e f e d a e f))
; one-rule melodies
; rule 3
(setf jones-rule3 '(c4 d e g a b c d e))
; rule 4
(setf jones-rule4 '(c4 d e f e d c d e))
; two rule melodies
; rule 5
(setf jones-rule5 '(c4 d e g a b f e d))
; rule 6
(setf jones-rule6 '(c4 d e f e d g a b))

;;; Final ritards
;;; desain:ritard p459

;;;
;;; Rhythmic structure for Ravel's Bolero
;;; \cite{tangiane,roberts:repetition} Notated in 3/4 as beginning the
;;; bar, but I think we feel the first beat as an upbeat, that is, as
;;; phase shifted pattern. It is only over the longer repeating phrase
;;; that we can resolve the pattern.  Play with snare drum.
;;;
(setf bolero-rhythm
      (rhythms 
       (rhythms
         e  ts ts ts   e  ts ts ts   e           e          
         e  ts ts ts   e  ts ts ts   ts ts ts    ts ts ts for 48) e))

(generator bolero-unaccented rhythm-onset (note 'c4 amplitude 0.75)
	   (setf rhythm (item bolero-rhythm :kill t))
	   (setf duration (/ rhythm 3)))

;;;
;;; Todd's Primal Sketch anapest rhythm example
;;; \cite[(example A, figure 9, pp.\ 46)]{todd:primal}.
(generator todd-anapest rhythm-onset
	   (length 64 note 'c4)
	   (vars (amplitude-stream (isochronous-amplitudes))
		 (rhythm-stream (items 0.25 0.25 0.5))
		 (duration-stream (fixed-duration 0.25 3)))
   	   (setf rhythm (item rhythm-stream))
           (setf amplitude (item amplitude-stream))
	   (setf duration (item duration-stream)))

;;; Same as above but leading with the long beat.
(generator todd-anapest-long-lead rhythm-onset
	   (length 32 note 'c4)
	   (vars (amplitude-stream (isochronous-amplitudes))
		 (rhythm-stream (items 0.5 0.25 0.25))
		 (duration-stream (fixed-duration 0.25 3)))
   	   (setf rhythm (item rhythm-stream))
           (setf amplitude (item amplitude-stream))
	   (setf duration (item duration-stream)))

;;; Todd's Primal Sketch cliche rhythm example from \cite{todd:primal}
;;; Fig 7(d), p44. IOIs are 200ms for an eigth note, with a 50ms duration.
;;; We emulate this using a tempo setting of 150 crochets per minute =
;;; and a quarter the duration.
(generator todd-cliche-unaccented-se rhythm-energy-square (amplitude 0.75)
	   (setf rhythm (item cliche-rhythm :kill t) tempo (tempo 150))
	   (setf note (item cliche-melody))
	   (setf duration (/ rhythm 4)))


;;; From Yeston \cite[p. 5]{yeston}, twelfth-centry rhythmic modes.
;;; "They are presented here as they may appear as the smallest units
;;; of rhythm that are legal in the system; that is, two occurrences
;;; of the primary pattern of each rhythmic mode."
(thread yeston-rhythmic-modes ()
       (generator yeston-rhythmic-mode-1 rhythm-onset 
		  (length 20 amplitude 0.75 note 'a4 duration 0.158)
		  (setf rhythm (item (rhythms q e q rest-e))))
       (generator yeston-rhythmic-mode-2 rhythm-onset 
		  (length 20 amplitude 0.75 note 'a4 duration 0.158)
		  (setf rhythm (item (rhythms e q e rest-q))))
       (generator yeston-rhythmic-mode-3 rhythm-onset 
		  (length 20 amplitude 0.75 note 'a4 duration 0.158)
		  (setf rhythm (item (rhythms q. e q q. rest-q.))))
       (generator yeston-rhythmic-mode-4 rhythm-onset 
		  (length 20 amplitude 0.75 note 'a4 duration 0.158)
		  (setf rhythm (item (rhythms e q q. e q rest-q.))))
       (generator yeston-rhythmic-mode-5 rhythm-onset 
		  (length 20 amplitude 0.75 note 'a4 duration 0.158)
		  (setf rhythm (item (rhythms q. q. q. rest-q.))))
       (generator yeston-rhythmic-mode-6 rhythm-onset 
		  (length 20 amplitude 0.75 note 'a4 duration 0.158)
		  (setf rhythm (item (rhythms e e e e rest-q)))))

;;; Tangiane's ambiguous segmentation example Fig 6.3, in 6/8
;(merge tangiane-ambiguous-segmentation ()
;       (thread () (object rhythm-onset rhythm (rhythms s e e e s e e :kill t)))
;       (thread () (object rhythm-onset rhythm (rhythms e e s e e e s))))

;;; Desain and Honing's Quantizer rhythm \cite{desain:connquant} from
;;; a recorded performance as criticised by Smoliar specified as IOI's
;;; in csecs
(setf desain-unquantized-rhythm
      '(11.77 5.92 2.88 3.37 4.36 3.37 3.87 6.0 6.34 2.96 2.8 2.96 3.46 11.93))

;;; normalising by the first interval indicates Smoliar's criticism
(defun normalise-rhythm (rhythm datum)
      (mapcar #'(lambda (n) (/ n datum)) rhythm))
;;(quantize (make-array 14 :initial-contents normalised-desain-quantizer-rhythm))

;;; Ideally I need to make a item-stream from a list.


;	   (vars (straight-rhythm 
;		   (make-item-stream 'rhythms 'cycle
;				     honing-web-rhythm :tempo 100)))

;;; Make a CM thread out of the IOI's
(generator desain-unquantized rhythm-onset ;rhythm-tone ;midi-note
	   (note 'c4 amplitude 0.95)
	   (vars (beat (make-item-stream 
			'items 'cycle 
			(normalise-rhythm desain-unquantized-rhythm
					  (first desain-unquantized-rhythm)))))
	   (setf rhythm (item beat :kill t))
	   (setf duration rhythm))

;	  (object rhythm-onset note 'c4 rhythm beat duration 0.12
;		  amplitude 0.95)))

;; The result obtained by 20 iterations of Desain and Honing's Connectionist Quantiser
(setf desain-quantized-rhythm 
      '(12.02 6.00 3.03 3.02 3.98 3.97 3.98 5.97 5.96 3.07 3.08 2.95 2.98 11.99))

; (load-file "~/Research/Software/Desain+Honing/POCO/quantizer.lisp")

; The "ideal" result of the quantization
; '(12.0 6.0 3.0 3.0 4.0 4.0 4.0 ; measure
; 6.0 6.0 3.0 3.0 3.0 3.0 12.0)
; (rhythms q e s s te te te e e s s s s q)
; in 3/4 

;;; Make a CM thread out of the IOI's
(thread desain-quantized ()
	(dolist (beat (normalise-rhythm desain-quantized-rhythm 
					(first desain-quantized-rhythm)))
	  (object rhythm-onset note 'c4 rhythm beat duration beat
		  amplitude 0.95)))


;;; Smoliar's except from Wagner's Tristan and Isolde
;;; couldn't this just be an item stream with expr to evaluate the divisions?
(setf smoliar-tristan-control-rhythm
      (list 2.0 2.5 0.5 1.5 0.5 1.5 0.5 0.5 0.5 1.5
	    0.5 2.0 2.0 (/ 1.0 3.0) (/ 1.0 3.0) (/ 1.0 3.0) 2.0 1.0
	    (/ 1.0 3.0) (/ 1.0 3.0) (/ 1.0 3.0)
	    2.0 1.0 (/ 1.0 3.0) (/ 1.0 3.0) (/ 1.0 3.0) 2.0 2.0 2.0
	    5.0 2.0))

(thread smoliar-tristan-performable ()
	(dolist (beat smoliar-tristan-control-rhythm)
	  (object rhythm-onset note 'c4 rhythm beat duration (/ beat 2.0)
		  amplitude 0.95)))
	
;;(quantize (make-array 31 :initial-contents smoliar-tristan-control-rhythm))

;; A complex rhythm with a rubato imposed over it
(generator complex-rubato-rhythm rhythm-onset (amplitude 0.95 note 'c4)
	   (setf rhythm (item (rhythms q. e q q e e q q) :kill t))
	   (setf duration (/ rhythm 2)))

;;; IOI points for Desain & Honings monophonic Beat Pattern (from their
;;; web site) example in seconds
;;; (thread honing ()
(setf honing-web-ioi
      (items 0.4474 0.1493 0.8983 0.3017 0.4479 0.1496 0.8984 0.2951 0.4511
	       0.1492 0.9011 0.2984 0.3015 0.2986 0.5973 0.5964))

;;; An equivalent rhythms item stream with canonical tempo measure to
;;; match the onset intervals of honing-web-ioi.
;;; |..||.....|.|..||.....|.|..||.....|.|.|.|...|...|
;;; *___+___+___*___+___+___*___+___+___*___+___+___*
;;; with every line being a 16th note and every dot a 16th rest
(setf honing-web-rhythm '(e. s q. e e. s q. e  e. s q. e e e q q h.))

;; rubato perturbation to apply to Desain and Honings rhythm
(setf short-rubato-tempo-curve
      (tempo 0 100.0 3 100.0 5 50.0 12 80.0 13 150.0 17 150.0 pulse 'q update after))

; need to have tempo determined by rhythm

;; Build a generator from the rhythm so we can perturb the tempo.
(generator honing-unaccented rhythm-tone (amplitude 0.70 note 'c5)
	   (vars (straight-rhythm 
		   (make-item-stream 'rhythms 'cycle
				     honing-web-rhythm :tempo 100)))
	   (setf rhythm (item straight-rhythm :kill t))
	   (setf duration rhythm))	; this ensures a proper final silence

; A generator which will tap on each beat of the honing rhythm.
(generator honing-foottap rhythm-onset 
	   (amplitude 0.4 rhythm (rhythm 'q 100) length 13)
	   (setf note (item (notes c3 a3 a3)))
	   (setf duration rhythm))

;; Just a generator to test our rubato 
(generator pulse-rubato-test rhythm-onset (amplitude 0.75 note 'a4 length 17)
	   (setf rhythm (item (rhythms q tempo short-rubato-tempo-curve)))
	   (setf duration 0.001))
	   
;; Apply a rubato perturbation to Honing's rhythm so we can check
;; resilance to tempo variation. 
(generator honing-rubato-unaccented rhythm-onset (amplitude 0.75 note 'a4)
	   (vars (rubato-rhythm 
		   (make-item-stream 'rhythms 'cycle honing-web-rhythm
				     :tempo short-rubato-tempo-curve)))
	   (setf rhythm (item rubato-rhythm :kill t))
	   (setf duration rhythm))

;;;
;;; From parncutt:pulse experiment 1
;;;
;;; Create a new thread object of Parncutt
;;; Need to create different tempo settings for each.
;;; Note that Parncutt uses a nonstandard tempo definition - audible events
;;; per minute rather than (theoretical) beats per minute. Combined
;;; with his definitions of rhythms using crochets as his slowest beat
;;; means we have some fanciful tempi to achieve the same IOI's as
;;; Parncutt's stimuli.
;;;
(thread parncutt-tapping-experiments ()
       (generator parncutt-pulse rhythm-onset
		  (length 20 amplitude 0.75 note 'a4 duration 0.158)
		  (setf rhythm (item (rhythms q tempo 100))))

       (generator parncutt-waltz rhythm-onset
		  (length 20 amplitude 0.75 note 'a4 duration 0.158)
		  (setf rhythm (item (rhythms h q tempo 100))))

       (generator parncutt-march rhythm-onset
		  (length 20 amplitude 0.75 note 'a4 duration 0.158)
		  (setf rhythm (item (rhythms h q q tempo 100))))

;;; For the swing and skip rhythms:
;;; 151bpm -> 400mS crochet IOI parncutts note rate 2
;;; 229bpm -> 261mS crochet IOI parncutts note rate 3
;;; 348bpm -> 172mS crochet IOI parncutts note rate 4
;;; 526bpm -> 114mS crochet IOI parncutts note rate 5
;;; 800bpm -> 75mS crochet IOI parncutts note rate 6

       (generator parncutt-swing rhythm-tone
		  (length 20 amplitude 0.75 note 'a4 duration 0.158)
		  (setf rhythm (item (rhythms h. h q tempo 526))))

       (generator parncutt-skip  rhythm-onset
		  (length 20 amplitude 0.75 note 'a4 duration 0.158)
		  (setf rhythm (item (rhythms h. q h tempo 526))))

       (generator parncutt-cross rhythm-onset
		  (length 20 amplitude 0.75 note 'a4 duration 0.158)
		  (setf rhythm (item (rhythms h q q h tempo 100))))

       ;; An example from parncutt:beat to compare template matching
       ;; in 12/8 
       (generator parncutt-salience-test rhythm-onset
		  (length 27 amplitude 0.75 duration 0.158)
		  (setf note (item (notes a5 (notes a4 for 8))))
		  (setf rhythm (item (rhythms e e e q e q e q e)))))
;;;
;;; From sloboda:mind p31
;;;
(setq delay-proportion (items 1/8 1/7 1/6 1/5 0.25 0.333 0.5 0.75 5/6 7/8))
; (setq sternberg-delay (q (* q delay-proportion) (* q (- 1 delay-proportion)) q)


;;;
;;; From rosenthal:machine_rhythm pp 93
;;;
;;; 30 = slow tempo Lee's algorithm 3/16 produces a grouping
;;; s e | s e | tie_e. | tie_s e | e s | tie_s e | e.
;;; 120 = fast tempo 2/8 produces a grouping 
;;; s e s | q | tie_e e | e e | q | tie_e e | e e|
(generator rosenthal-grouping rhythm-onset ;; rhythm-onset
	   (length 11 amplitude 0.75 duration 0.158 note 'a4)
	   (setf rhythm (item (rhythms s e s q. e e e q. e e e tempo 120))))


;; From parncutt:rhythm p133 
;; 4/4
;; Needs more transcription for complete fugue theme
;; re e e e e. 32 32 e e | e e tie_s s s s s s

;;; Charles Keil's Ride cymbal taps in attempting a "Elvin Jones" style of
;;; playing, from Progler:swing p36.
;;; In milliseconds deviation from metronome,
;;; -ve is deviation in timing *before* the metronome beat
(setq keil-jones-ridetap '(-50 -80 -60 -85 ; Measure 5
	-40 -45 -35 -55 ; Measure 6
	-40 -35 -17 -35 ; Measure 7
	-5  -55 -45 -60 ; Measure 8
	-40))

(setq keil-clarke-ridetap '(-80 -57 -67 -70
	-85 -89 -87 -97
	-80 -55 -65 -63
	-59 -55 -75 -110
	-85))

(setq keil-bassline '(0 -45 -49 -65
	-40 -69 -60 -38
	-40 -37 -40 -35
	-42 -35 -45 -123
	-40))

;;; clarke:struct p212
(setq clarke-metrical-context-monody (rhythms e. s e e q s s e s s q e
					      e e e e q m))
