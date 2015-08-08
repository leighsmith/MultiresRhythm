## -*- Octave -*-
## Find the index where n would be inserted after, so as to 
## keep ascending sorted order assuming vector was sorted initially.
function point = insertpoint(vector, n)
  ## check special case of n being last element in vector
  vectlen = length(vector); 
  if vector(vectlen) == n
    point = vectlen;
  else
    point = find(n >= vector & n < shift(vector, -1));
  endif
endfunction
