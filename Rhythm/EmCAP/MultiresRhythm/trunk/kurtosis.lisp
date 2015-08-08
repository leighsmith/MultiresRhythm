(setf triangle (.* (.concatenate (.iseq 0 30) (.- 30 (.iseq 0 30))) 1d0))
(mean triangle)
(gsl:skewness (nlisp::nlisp->gsl triangle))
(gsl:kurtosis (nlisp::nlisp->gsl triangle)) ; Probably subtracts 3 for normalisation.

(setf g (mrr::gaussian-envelope 150)) ; normal, kurtosis should be 0.0
(setf gw (mrr::gaussian-envelope 150 :stddev 3.0d0)) ; platykurtic, kurtosis should be -ve
(setf gn (mrr::gaussian-envelope 150 :stddev 0.50d0)) ; leptokurtic, kurtosis should be +ve
(plot (list g gw gn) nil :legends '("normal" "platykurtic" "leptokurtic"))
(gsl:kurtosis (nlisp::nlisp->gsl g))
(gsl:kurtosis (nlisp::nlisp->gsl gw))
(gsl:kurtosis (nlisp::nlisp->gsl gn))

(gsl:skewness (nlisp::nlisp->gsl g))
(gsl:skewness (nlisp::nlisp->gsl gw))
(gsl:skewness (nlisp::nlisp->gsl gn))
