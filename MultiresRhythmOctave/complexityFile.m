### -*- Octave -*-
###
### $Id:$
###
function [magnitude, phase, pc, normalised_pc] = complexityFile(comment, filename)
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

  voicesPerOctave = 16;

  ## If we factor out the plotting
  [magnitude, phase, pc] = cwtToFile(rhythm.signal, voicesPerOctave);

  plotPhaseCongruency(rhythm.description, rhythm.signal, pc);

  normalised_pc = sum(pc) / length(rhythm.signal);
  printf("normalised phase congruency measure for entire rhythm %f\n", normalised_pc);
 
  ## pc(rhythm) = 0.0;
  ## find(pc == max(pc))
  
endfunction
