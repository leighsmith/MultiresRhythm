### -*- Octave -*-
###
### $Id:$
###
### Analysis of a numbered syncopation rhythm
###
function syncopation = syncopation(fileNumber)
  filename = sprintf("~/Research/Data/NewAnalysedRhythms/shmulevitch-pattern-%d", fileNumber);

  rhythm.description = sprintf("Shmulevitch %d", fileNumber);
  [singleMeasure, header] = loadRhythm(filename, "mat");
  rhythm.SampleRate = header.samplerate;

  ## Make 4 duplicates to match the original experiment.
  rhythm.signal = [singleMeasure, singleMeasure, singleMeasure, singleMeasure];

  analysis.beginClapAt = 1;
  analysis.expectedTactusIOI = 0;
  analysis.correlationMethod = "rd | rlp | rsp";
  analysis.multiple = 1;

  [mag, phase, pc, tactus, claps] = multiresRhythm(rhythm, analysis, "");

  normalised_pc = sum(pc) / length(rhythm.signal);
  printf("normalised phase congruency measure for entire rhythm %f\n", normalised_pc);

  ## plot(normalised_pc);
  ## pc(rhythm) = 0.0;
  ## find(pc == max(pc))

  ## use the rhythm sample rate to deduce rhythmic times in seconds.
  ## Octave indexes matrices beginning at 1, so we reduce by one to
  ## begin at 0.0 seconds. 
  handClapTimes = (claps(1,:)-1) ./ rhythm.SampleRate;
  
  ## Write the handclapping out to either a Common Music file or a MusicKit scorefile.
  ## save_cm(filename, times, claps(2,:), "sampler");
  save_score(rhythm.description, filename, handClapTimes, claps(2,:), "Sampler");
  ##
  beatTimes = (find(rhythm.signal) - 1) ./ rhythm.SampleRate
  ## save_score(rhythm.description, 

endfunction


