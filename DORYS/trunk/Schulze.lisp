#|
;; Read a line of timings between intervals
(defun read-schulze-data (filename)
  (with-open-file (ioi-file filename :direction :input)
    (loop
       for line = (read-line ioi-file)
(read-from-string 
(read-sequence)
|#

(setf schulze-data (read-schulze-data  "/Users/leigh/Research/Data/HH_Schulze/AKC_IOI.txt"))

;; Bodgy!
(defun read-schulze-data (filename)
  (let ((akc-ioi-single (.load "/Users/lsmith/Research/Data/HH_Schulze/AKC_IOI_modified.txt" :format :text)))
    (.reshape akc-ioi-single '(563 37))))

(defun rhythm-of-schulze (rhythm-index schulze-data)
  "Returns a rhythm instance from the millisecond timing data"
  (iois-to-rhythm (format nil "schulze aksak ~d" rhythm-index)
		  (nlisp::array-to-list (./ (.subarray schulze-data (list rhythm-index '(4 36))) 1000.0d0))
		  :tempo 60.0d0))
