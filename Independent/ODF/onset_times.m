function onsets_seconds = onset_times(filename)
% Return times in seconds of the location of onsets in the audio file.
  threshold = 1.5;

  [audio_signal, sample_rate, resolution] = wavread(tilde_expand(filename));

  if (resolution < 16)
    fprintf(stderr, 'dynamic range resolution %d below 16 bits\n', resolution);
  end

  fprintf('%s %d samples @ %f Hz, each %d bits\n', filename, length(audio_signal), sample_rate, resolution);
  [wodf, odf_sr] = odf_of_signal(audio_signal, sample_rate);
  % Find the relative difference of the wide-band onset detection function. 
  % This identifies rapid attack slopes.
  wodf_diff = diff(wodf);
  % The parabolic maxima identify the perceptual-centers of the onsets.
  allPeaks = peaks(wodf_diff);

  bigPeaks = (wodf_diff .* allPeaks) > (std(wodf_diff) * threshold);
  onset_times = find(bigPeaks);
  % All values will be moved forward one because of the 1st order difference.
  % Also, we've found the peaks of the relative difference, however, what we want are the
  % attack onset times preceding these. Therefore the compensation descends from the peaks to
  % the nearest preceding troughs.
  attack_times = find_onset_attacks(wodf, onset_times, odf_sr);

  onsets_seconds = attack_times ./ odf_sr;
end

