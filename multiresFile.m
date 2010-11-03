### -*- Octave -*-
### $Id$
###
### Perform all the analyses on the signal read from the file.
###
function [mag, phase, pc, tactus, claps] = multiresFile(comment, filename, expectedTactusIOI, beginClapAt, correlationMethod)
  rhythm.description = comment;
  ## Check for file extension, if so, strip it off so we can use the
  ## filename as a base for generating other files,
  extensionPos = rindex(filename, "."); 
  if(extensionPos != 0)
    extension = substr(filename, extensionPos + 1, length(filename) - extensionPos);
    filename = substr(filename, 1, extensionPos - 1)
    [rhythm.signal, header] = loadRhythm(filename, extension);
  else
    [rhythm.signal, header] = loadRhythm(filename);
  endif
  rhythm.SampleRate = header.samplerate;

  printf("Analysing rhythm loaded from file %s\n", header.info);

  if(nargin < 3)
    analysis.expectedTactusIOI = 0;
  else
    analysis.expectedTactusIOI = expectedTactusIOI;
  endif
  analysis.beginClapAt = beginClapAt;
  analysis.correlationMethod = correlationMethod;
  analysis.multiple = 1;   # to clap more than once per the tactus rate.

  [mag, phase, pc, tactus, claps] = multiresRhythm(rhythm, analysis, filename);

  ## use the rhythm sample rate to deduce rhythmic times in seconds.
  ## Octave indexes matrices beginning at 1, so we reduce by one to
  ## begin at 0.0 seconds. 
  times = (claps(1,:)-1) ./ rhythm.SampleRate;
  
  ## Write the handclapping out to either a Common Music file or a MusicKit scorefile.
  ## save_cm(filename, times, claps(2,:), "sampler");
  save_score(comment, filename, times, claps(2,:), "Sampler");
endfunction
