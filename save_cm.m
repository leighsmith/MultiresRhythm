# -*- Octave -*-
# Save the vectors as times and amplitudes of a Common Music thread.
function save_cm(filename, times, velocities, instrument)
  handClapFilename = tilde_expand([filename ".handclap.cm"]);
  [f,msg] = fopen(handClapFilename, "w");

  if(f == -1)
    error([msg, " ", handClapFilename]);
  endif

  ## retrieve the name of the file without the path to make a thread name
  basename = split(filename, "/");
  sz = size(basename);
  
  fprintf(f, "(thread %s-handclapping (start %6.3f)\n", deblank(basename(sz(1),:)), times(1));
  rhythms = diff(times);

  # final rhythm is immaterial, so long as it's long enough for the sample
  rhythms(length(times)) = 1.0;

  # normally we handclap
  if(nargin < 4)
    instrument = "sampler";
  endif

  for timeIndex = 1:length(times)
    fprintf(f, "  ; time = %6.3f\n", times(timeIndex));
    fprintf(f, "  (object %s ", instrument);
    if(strcmp(instrument, "sampler"))
      fprintf(f, "sample-file \"%s\" ", "~/Library/Sounds/hihat_closed.aiff");
    elseif(strcmp(instrument, "rhythm-tone") || \
	   strcmp(instrument, "midi-note") || \
	   strcmp(instrument, "rhythm-onset"))
      fprintf(f, "note 'g3 ");
    endif
    fprintf(f, "duration %6.3f rhythm %6.3f amplitude %f)\n", \
	    0.075, rhythms(timeIndex), velocities(timeIndex));
  endfor
  fprintf(f, ")\n");
  fclose(f);
endfunction


