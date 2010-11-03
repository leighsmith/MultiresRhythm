# -*- Octave -*-
load "sorted-shmulevich.mat"
sorted_shmulevich = VARIABLE

load "lhl_model.mat"
lhl_model = VARIABLE

r = corrcoef(sorted_shmulevich, lhl_model)
N = length(lhl_model)

# correlation assuming binormal distribution
sig_when_large = erfc((abs(r) .* sqrt(N)) / sqrt(2))
student_t_when_small = r .* sqrt((N - 2) / (1 - r ^ 2))
nu = N - 2

# Students Distribution Probability Function: A()
# sig_when_small = 1 - A(t|nu)
sig_when_small = betainc(nu / (nu + student_t_when_small ^ 2), 0.5 * nu, 0.5)
## This should (and does) match the correlation test result of cor_test()
one_sided_significance = sig_when_small / 2

correlation_tests(sorted_shmulevich, r, lhl_model);

load "pk_muso.mat" 
pk_muso = VARIABLE
correlation_tests(sorted_shmulevich, r, pk_muso);

# If binormality holds between the sorted_shmulevich and lhl_model,
# which is a reasonable assumption since one is computed from the other,
# using Student's t probability, the correlation is significant.

load "pk_nonmuso.mat" 
pk_nonmuso = VARIABLE
correlation_tests(sorted_shmulevich, r, pk_nonmuso);

correlation_tests(lhl_model, r, pk_nonmuso);

## Test case: compare results when the data set is identical. We
## multiply by a constant to avoid divide by zero errors.
correlation_tests(sorted_shmulevich, r, sorted_shmulevich .* 3);
