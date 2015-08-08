## -*- Octave -*-
## Find the lowest scale of the unquantized tactus and return the new
## one all on the same scale.
function quantTactus = straightenTactus(unquantTactus)
  quantTactus = unquantTactus;
  ## quantTactus(1,:) = min(unquantTactus(1, :).').';
  quantTactus(1,:) = floor(mean(unquantTactus(1, :)));
endfunction
