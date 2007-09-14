;;; Drums
(compute-perceptual-versions "res3_1_resp_text"  "res3_1_pOnsets_text"  "res3_1")
(compute-perceptual-versions "res3_2_resp_text"  "res3_2_pOnsets_text"  "res3_2")
(compute-perceptual-versions "res3_7_resp_text"  "res3_7_pOnsets_text"  "results3_7")
(compute-perceptual-versions "res3_9_resp_text"  "res3_9_pOnsets_text"  "results3_9")
(compute-perceptual-versions "res3_11_resp_text" "res3_11_pOnsets_text" "results3_1" :start-from-beat 1)
(compute-perceptual-versions "res3_20_resp_text" "res3_20_pOnsets_text" "res3_20")
(compute-perceptual-versions "res3_38_resp_text" "res3_38_pOnsets_text" "res3_38")

;;; Freely Sung examples.
(compute-perceptual-versions "res1_1_resp_text" "res1_1_pOnsets_text" "res1_1")
;; Clapping from 0 creates a phase problem with res1_2
(compute-perceptual-versions "res1_2_resp_text" "res1_2_pOnsets_text" "res1_2" :start-from-beat 1) 
(compute-perceptual-versions "res1_3_resp_text" "res1_3_pOnsets_text" "res1_3")
(compute-perceptual-versions "res4_1_resp_text" "res4_1_pOnsets_text" "res4_1")
(compute-perceptual-versions "res4_2_resp_text" "res4_2_pOnsets_text" "res4_2")
(compute-perceptual-versions "res4_3_resp_text" "res4_3_pOnsets_text" "res4_3")
