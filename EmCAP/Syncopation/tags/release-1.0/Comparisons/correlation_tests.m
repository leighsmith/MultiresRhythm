## -*- Octave -*-
function r_test = correlation_tests(datum_set, r, test_set)
  N = length(datum_set);
  r_test = corrcoef(datum_set, test_set)
  pearson_correlation_test = cor_test(datum_set, test_set, ">", "pearson")
  ## Now check with nonparametric Spearman rank correlation
  spearman_correlation = spearman(datum_set, test_set)
  spearman_correlation_test = cor_test(datum_set, test_set, "<>", "spearman")

  [pval, t_test, dof] = t_test_2(datum_set, test_set)

  corr_sod = corr_sod(r, r_test, N, N)
endfunction
