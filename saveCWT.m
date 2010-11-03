### -*- Octave -*-
###
### $Id:$
###
### perhaps we should save as binary?
### we flip the coords so the highest frequency scales are written last
### to match the MMA plotting code.
###
function saveCWT(filename, mag, phase, pc)
  ## The amount of downsampling of the cwt result on the translation axis
  ## we do before we write to a file.
  saveReduction = 4;

  phase = cleanPhase(mag, phase);
  MagToWrite = -flipud(mag);
  PhaseToWrite = flipud(phase);
  DownSampledPC = pc(:, 1 : saveReduction : length(mag));

  save([filename ".mag"], "MagToWrite");
  save([filename ".phase"], "PhaseToWrite");
  save([filename ".pc"], "DownSampledPC");
endfunction
