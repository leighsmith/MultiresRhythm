# -*- Octave -*-
# Load the rhythm, either from a sparse impulse NeXT/Sun sound file
# or from a octave ascii data file (assuming loading into the 1xN
# vector "signal").
function [signal, soundheader] = loadRhythm(filename, extension)
  if(nargin == 1 || extension == "snd")
    ## produces a 1xN vector in signal.
    [signal, soundheader] = loadaudio(tilde_expand(filename), "snd");
  else
    # system(["make_octave_data " filename]); # .text
    ## Sigh. load() returns the value as a structure with a field named
    ## the same as the variable, making practically impossible to know
    ## what that variable is named...
    signalStructure = load([tilde_expand(filename) "." extension]);
    ## This seems about the only way to store it into our own damn variable...
    for [val, key] = signalStructure
      signal = val;
    endfor
    soundheader.channels = 1;
    ## soundheader.info = ?;
    soundheader.readFormat = "short";
    soundheader.samplerate = 200;
    ## signal = ?;
    ## if a single column vector, standardise to a single row vector.
    if(rows(signal) > columns(signal))
      signal = signal.';
    endif
  endif
endfunction

