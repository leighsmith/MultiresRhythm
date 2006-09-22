## -*- Octave -*-
##
## Correlation Significance of Difference
## See Numerical Recipes 2nd Ed, p643
function sig_of_diff = corr_sod(r1, r2, N1, N2)
  ## (sqrt(2.0) .* sqrt((1 / (N - 3)) + (1 / (N - 3))))
  ## abs(fisherz(r) - fisherz(r_pm))
  sig_of_diff = erfc(abs(fisherz(r1) - fisherz(r2)) / (sqrt(2.0) .* sqrt((1 / (N1 - 3)) + (1 / (N2 - 3)))));
endfunction
