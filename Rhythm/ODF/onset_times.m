function [onsets_seconds, attack_times] = onset_times(audio_signal, sample_rate)
% Return times in seconds of the location of onsets in the audio file.
  threshold = 1.5;

  [wodf, odf_sr] = odf_of_signal(audio_signal, sample_rate);
  % Find the relative difference of the wide-band onset detection function. 
  % This identifies rapid attack slopes.
  wodf_diff = diff(wodf);
  % The parabolic maxima identify the perceptual-centers of the onsets.
  allPeaks = peaks(wodf_diff);

  bigPeaks = (wodf_diff .* allPeaks) > (std(wodf_diff) * threshold);
  peak_times = find(bigPeaks);
  % All values will be moved forward one because of the 1st order difference.
  % Also, we've found the peaks of the relative difference, however, what we want are the
  % attack onset times preceding these. Therefore the compensation descends from the peaks to
  % the nearest preceding troughs.
  onset_times = find_onset_attacks(wodf, peak_times, odf_sr);
  attack_times = (peak_times - onset_times) ./ odf_sr;

  onsets_seconds = onset_times ./ odf_sr;
end

