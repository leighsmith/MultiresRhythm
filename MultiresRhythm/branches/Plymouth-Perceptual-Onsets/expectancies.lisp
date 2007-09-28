;;;; -*- Lisp -*-

(in-package :multires-rhythm)
(use-package :nlisp)


;;;; "Peter Seibels directory walking functions, to be drawn in using packages"

(defun component-present-p (value) 
  (and value (not (eql value :unspecific)))) 

(defun directory-pathname-p  (p) 
  (and 
   (not (component-present-p (pathname-name p))) 
   (not (component-present-p (pathname-type p))) 
   p)) 

(defun pathname-as-directory (name) 
  (let ((pathname (pathname name))) 
    (when (wild-pathname-p pathname) 
      (error "Can't reliably convert wild pathnames.")) 
    (if (not (directory-pathname-p name)) 
      (make-pathname 
       :directory (append (or (pathname-directory pathname) (list :relative)) 
                          (list (file-namestring pathname))) 
       :name      nil 
       :type      nil 
       :defaults pathname) 
      pathname))) 

(defun directory-wildcard (dirname) 
  (make-pathname 
   :name :wild 
   :type #-clisp :wild #+clisp nil 
   :defaults (pathname-as-directory dirname))) 

(defun list-directory (dirname) 
  (when (wild-pathname-p dirname) 
    (error "Can only list concrete directory names.")) 
  (directory (directory-wildcard dirname))) 

(defun walk-directory (dirname fn &key directories (test (constantly t))) 
  (labels 
      ((walk (name) 
         (cond 
           ((directory-pathname-p name) 
            (when (and directories (funcall test name)) 
              (funcall fn name)) 
            (dolist (x (list-directory name)) (walk x))) 
           ((funcall test name) (funcall fn name))))) 
    (walk (pathname-as-directory dirname))))

;;
(defun is-not-file-of-type (filepath type)
  (not (equal (pathname-type filepath) type)))

(defun is-dot-file (filepath)
  (equal (aref (pathname-name filepath) 0) #\.))

(defun does-not-have-claps-file (filepath)
  (and (is-not-file-of-type filepath "claps")	; ensure filepath isn't a clap file
       (not (is-dot-file filepath))		; it isn't a .DS_Store
       ;; and that it doesn't have an accompanying clap file
       (not (probe-file (make-pathname :defaults filepath :type "claps")))))
								    
;;; Convert times in seconds to a rhythm.
(defun clap-to-times-in-file (filepath)
  (let* ((sample-rate 200.0d0)
	 (times-in-seconds (nlisp::.load-text-file filepath))
	 (times-as-rhythm (rhythm-of-onsets (pathname-name filepath) times-in-seconds :sample-rate sample-rate))
	 (clap-times (clap-to-rhythm times-as-rhythm 
				     :tactus-selector #'create-weighted-beat-ridge 
				     ;; :beat-multiple 1
				     :start-from-beat 0))
    	 (clap-times-in-seconds (./ clap-times sample-rate))
	 (clap-filepath (make-pathname :defaults filepath :type "claps")))
    (format t "Beat times of ~a in seconds:~%~a~%" (name times-as-rhythm) clap-times-in-seconds)
    (nlisp::.save-text-file clap-times-in-seconds clap-filepath)))

;; (walk-directory "/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/" #'print :test #'does-not-have-claps-file)

;; (walk-directory "/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/" #'clap-to-times-in-file :test #'does-not-have-claps-file)

;; (clap-to-times-in-file #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/RobertRich/Java Gourd 01.corneronsetsqrt-p1-f30-s10-g075")
;; (clap-to-times-in-file
;;  #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/KeithLeblanc/clever01.corneronsetsqrt-p1-f30-s10-g075")
;; (clap-to-times-in-file
;;  #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/KeithLeblanc/4-Down 1.corneronsetsqrt-p1-f30-s10-g075")

;;; Crashers!
;; (walk-directory "/Volumes/iDisk/Research/Data/RicardsOnsetTests/crashers/" #'clap-to-times-in-file :test #'does-not-have-claps-file)
;; (clap-to-times-in-file #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/Acid/drums1304.corneronsetsqrt-p1-f30-s10-g075")
;; (clap-to-times-in-file #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/KeithLeblanc/fourkick1.corneronsetsqrt-p1-f30-s10-g075")

(defun expectancy-of-ridge-at-time (ridge time scaleogram)
  "Return a list indicating the expection time, the confidence and the precision" 
  (let* ((vpo (voices-per-octave scaleogram))
	 (max-time (duration-in-samples scaleogram))
	 (magnitude (scaleogram-magnitude scaleogram))
	 (scale (scale-at-time ridge time))
	 (expected-time (+ time (time-support scale vpo)))
	 ;; energy = relative height (of scaleogram or ridge-peaks)
	 (energy (/ (.aref magnitude scale time) (.max (.column magnitude time))))
	 (relative-ridge-duration (if (> time 0) (- 1.0 (/ (start-sample ridge) time)) 1))
	 ;; relative confidence = energy * duration of the ridge up until this moment.
	 (confidence (* energy relative-ridge-duration))
	 ;; precision = sharpness of peak. We calculate this as the area under the scale
	 ;; between the other peaks.
	 (precision 0))
    (list (min expected-time max-time) confidence precision)))

(defun expectancies-of-rhythm (rhythm)
  (let* ((rhythm-analysis (analysis-of-rhythm rhythm))
	 (rhythm-scaleogram (scaleogram rhythm-analysis))
	 ;; The skeleton has already picked scale peaks at each time.
	 (skeleton (skeleton rhythm-analysis))
	 (preferred-tempo-scale (preferred-tempo-scale (voices-per-octave rhythm-scaleogram) (sample-rate rhythm)))
	 ;; Cut off freq above 3 stddev's from preferred-scale = 3 octaves.
	 (cutoff-scale (- preferred-tempo-scale (* 3 (voices-per-octave rhythm-scaleogram))))
	 (times-to-check (butlast (nlisp::array-to-list (onsets-in-samples rhythm)))))
    (loop
       for time in times-to-check
       for event-index = 0 then (1+ event-index)
       ;;do (plot-scale-energy+peaks-at-time rhythm-scaleogram time (ridge-peaks rhythm-analysis) :sample-rate (sample-rate rhythm))
       collect (list event-index time
		     (loop
			for ridge in (ridges-at-time skeleton time)
			;; filter out the ridges higher than a cut off point determined by preferred tempo rate.
			when (> (scale-at-time ridge time) cutoff-scale)
			collect (expectancy-of-ridge-at-time ridge time rhythm-scaleogram))))))

(defun expectancies-in-seconds (expectancy-structure sample-rate)
  "Converts sample times to seconds"
  (mapcar (lambda (expected-time) 
	    (list (first expected-time)
		  (/ (second expected-time) (float sample-rate))
		  (mapcar (lambda (x) (list (/ (first x) (float sample-rate)) (second x) (third x)))
			  (third expected-time)))) expectancy-structure))

(defun expectancies-at-times-in-file (filepath)
  (let* ((sample-rate 200.0d0)
	 (times-in-seconds (nlisp::.load-text-file filepath))
	 (times-as-rhythm (rhythm-of-onsets (pathname-name filepath) times-in-seconds :sample-rate sample-rate))
	 (expectancies-at-times (expectancies-of-rhythm times-as-rhythm))
    	 (expectancy-times-in-seconds (expectancies-in-seconds expectancies-at-times sample-rate))
	 (expectancy-filepath (make-pathname :defaults filepath :type "expectancies")))
    ;; Write the structure to a file.
    (with-open-file (expectancy-file expectancy-filepath :direction :output :if-exists :supersede)
      (format expectancy-file "~{~{~d, ~,5f, ~:{~,5f ~,5f ~,5f~:^; ~}~}~%~}" expectancy-times-in-seconds))))

(defun does-not-have-expectancies-file (filepath)
  (and (is-not-file-of-type filepath "expectancies")	; ensure filepath isn't a clap file
       (not (is-dot-file filepath))		; it isn't a .DS_Store
       ;; and that it doesn't have an accompanying clap file
       (not (probe-file (make-pathname :defaults filepath :type "expectancies")))))

;; (expectancies-at-times-in-file #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/RobertRich/Java Gourd 01.corneronsetsqrt-p1-f30-s10-g075")
;; (walk-directory "/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/" #'expectancies-at-times-in-file :test #'does-not-have-expectancies-file)
