% -*- octave -*-
function peaks = peaks(signal,minimum)
  signalPlus = shift(signal, 1);
  signalMinus = shift(signal, -1);
  if nargin < 2
    peaks = signal > signalPlus & signal > signalMinus;
  else
    peaks = signal < signalPlus & signal < signalMinus;
  end
endfunction
