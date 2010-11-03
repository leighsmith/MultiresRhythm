## -*- Octave -*-
function print_ioi(filename)
  sig = loadRhythm(filename);
  length(sig)
  find(sig)
  diff([find(sig); length(sig)-1])
endfunction
