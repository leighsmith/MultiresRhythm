(defun fm-test (&key (signal-length 2048))
  "Frequency modulating signal for wavelet analysis."
  (let* ((carrierFreq 16)
	 (modFreq 2.1)
	 (norm-signal (.rseq 0 1 signal-length))
	 (mod (.* 3.0 (.cos (.* norm-signal 2.0 pi modFreq))))
	 (fm-signal (.cos (.+ (.* 2 pi carrierFreq norm-signal) mod))))
    ;; (plot mod nil)
    (plot fm-signal nil)
    fm-signal))

;; (multiple-value-setq (fm-mag fm-phase) (dyadic-cwt (fm-test) 8 512))
;; (setf x (dyadic-cwt (fm-test :signal-length 256) 8 256))
