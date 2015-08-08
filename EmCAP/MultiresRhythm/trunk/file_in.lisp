;;; Because this format is so full of useless context sensitive formatting noise, we strip
;;; all the crap away to get it to a form that Lisp can devour simply.
(defun strip-emcap-formatting (emcap-data-line)
  (let* ((timed-feature-string emcap-data-line)
	 (separator-position (position #\; timed-feature-string))
	 (onset-and-variance (substitute #\Space #\, (subseq timed-feature-string 0 separator-position)))
	 (onset-stream (make-string-input-stream onset-and-variance))
	 (feature-string (substitute #\Space #\, (subseq timed-feature-string (1+ separator-position))))
	 (feature-stream (make-string-input-stream feature-string)))
    (list (read onset-stream) (read onset-stream nil))))

(with-input-from-string (string-stream "1234 23232 23.2") (loop for x = (read string-stream nil) while x collect x))
;; package adjacent floats into sublists.
(loop for (x y) on '(1 2 3 4 5 6 7 8) by #'cddr collect (list x y))


;;; File I/O routines.
(use-package :cl-fad)

;; Routines to use with the cl-fad directory routines.
(defun is-file-of-type (filepath type)
  (equal (pathname-type filepath) type))

(defun is-dot-file (filepath)
  (equal (aref (pathname-name filepath) 0) #\.))

(defun does-not-have-file-of-type (filepath type)
  (and (is-file-of-type filepath "corneronsetsqrt-p1-f30-s10-g075")	; ensure filepath isn't a clap file
       (not (is-dot-file filepath))		; it isn't a .DS_Store
       ;; and that it doesn't have an accompanying clap file
       (not (probe-file (make-pathname :defaults filepath :type type)))))

(defun does-not-have-expectancies-file (filepath)
  (does-not-have-file-of-type filepath "expectancies"))

(defun does-not-have-last-expectancy-file (filepath)
  (does-not-have-file-of-type filepath "last_expectancy"))

(defun does-not-have-claps-file (filepath)
  (does-not-have-file-of-type filepath "claps"))

(defparameter *ricards-data* "/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/")


;; (walk-directory *ricards-data* #'print :test #'does-not-have-claps-file)

;; (walk-directory *ricards-data* #'clap-to-times-in-file :test #'does-not-have-claps-file)

;; (clap-to-times-in-file #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/RobertRich/Java Gourd 01.corneronsetsqrt-p1-f30-s10-g075")
;; (clap-to-times-in-file
;;  #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/KeithLeblanc/clever01.corneronsetsqrt-p1-f30-s10-g075")
;; (clap-to-times-in-file
;;  #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/KeithLeblanc/4-Down 1.corneronsetsqrt-p1-f30-s10-g075")

;;; Crashers!
;; (walk-directory "/Volumes/iDisk/Research/Data/RicardsOnsetTests/crashers/" #'clap-to-times-in-file :test #'does-not-have-claps-file)
;; (clap-to-times-in-file #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/Acid/drums1304.corneronsetsqrt-p1-f30-s10-g075")
;; (clap-to-times-in-file #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/KeithLeblanc/fourkick1.corneronsetsqrt-p1-f30-s10-g075")


;; (expectancies-at-times-in-file #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/RobertRich/Java Gourd 01.corneronsetsqrt-p1-f30-s10-g075")

;; (expectancies-at-times-in-file #P"/Volumes/iDisk/Research/Data/RicardsOnsetTests/PercussivePhrases/Burning/3.corneronsetsqrt-p1-f30-s10-g075")

;; (walk-directory *ricards-data* #'expectancies-at-times-in-file :test #'does-not-have-expectancies-file)

;; (walk-directory *ricards-data* #'last-expectancy-of-file :test #'does-not-have-last-expectancy-file)


