(defun fm-test ()
  "Frequency modulating signal for wavelet analysis."
  (let* ((signalLength 2048)
	 (carrierFreq 16)
	 (modFreq 2.1)
	 (norm-signal (.rseq 0 1 signalLength))
	 (mod (.* 3.0 (.cos (.* norm-signal 2.0 pi modFreq))))
	 (fm-signal (.cos (.+ (.* 2 pi carrierFreq norm-signal) mod))))
    ;; (plot mod nil)
    (plot fm-signal nil)
    fm-signal))

;; (dyadic-cwt (fm-test) 8 512)
