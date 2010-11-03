### -*- Octave -*-
### $Id$
### 
### No, this isn't a better version of a "MaxiSheild"...
### (sorry couldn't resist :-)
### 
### Instead we pad either side of the signal with mirrored portions
### of the signal to a dyadic length to enable efficient FFTs.
###
function [pad_signal,trimDyadic] = dyadicPad(signal)
  file_len = length(signal);
  paddedLength = dyadicLength(file_len);
  toPad = paddedLength - file_len;
  halfPad = floor(toPad / 2);
  offByOne = toPad - halfPad * 2;

  ## we can generate a empty matrix warning if the newLength matches
  ## the file_len.
  if(toPad == 0)
    pad_signal = signal;
  else
    ## padding with the signal ensures a periodicity of the window
    ## pad_signal = [zeros(halfPad + offByOne, 1); signal; zeros(halfPad, 1)];
    lastbit = signal(:, file_len - (halfPad + offByOne - 1) : file_len);
    pad_signal = [lastbit, signal, signal(:, 1 : halfPad)];
  endif

  ## trim the padded regions to make the plots readable.
  if(toPad != 0)
    trimDyadic = halfPad + offByOne + 1 : paddedLength - halfPad;
  else
    trimDyadic = 1 : file_len;
  endif
endfunction
