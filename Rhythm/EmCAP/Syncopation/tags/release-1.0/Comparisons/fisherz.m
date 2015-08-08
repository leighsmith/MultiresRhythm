## -*- Octave -*-
## Computes Fisher's z transformation
## from Numerical Recipes in C++, 2nd Ed. p642.
function result = fisherz(r)
  result = 0.5 * log((1 + r) / (1 - r));
endfunction
