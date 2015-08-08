function attack_times = find_onset_attacks(odf, onset_times, odf_sr)
%
% Look a maximum of 50 milliseconds before the onset_time_peaks.
lookBackSeconds = 0.050;
lookBack = round(lookBackSeconds * odf_sr);
for onsetTimeIndex = 1:length(onset_times)
    onsetTime = onset_times(onsetTimeIndex);
    earliestTime = onsetTime - lookBack;
    % Find local minima troughs within the preceding attack window.
    minimaLocations = find(peaks(odf(earliestTime : onsetTime), true));
    if length(minimaLocations) == 0
       % if no local minima troughs were found, find the minimum in the window
       minimaLocations = find(min(odf(earliestTime : onsetTime)) == odf(earliestTime : onsetTime));
    end
    % Take the latest one in the window (i.e. nearest the peak) as the start of the attack.
    attack_times(onsetTimeIndex) = earliestTime + minimaLocations(end);
end
