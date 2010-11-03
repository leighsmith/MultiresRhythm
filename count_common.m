## this incantation gives us the number of consecutive common values
## in the vector.
function count = count_common(x)
  ## column vector version
  count = diff([0; find(diff(x)); length(x)]);
endfunction
