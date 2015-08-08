function [ mean_quaver_alternation ] = quaver_alternation( pattern )
%quaver_alternation Calculate a metric indicating how much the quaver
%patterns alternate across the measure.
%   Calculates the variation between the metrical profile occurring on the
%   subbeats (not considering the beats).

metricalProfile = reducedMetricalProfile(pattern);

% Check the bass subbeats only.
% metricalProfile = metricalProfile(1:16);
% Check the treble subbeats only.
% metricalProfile = metricalProfile(17:32);

tatums_per_beat = 4;

% Remove the beat onsets since they will be large relative to the offbeats.
beatMeterProfile = reshape(metricalProfile, tatums_per_beat, length(metricalProfile) / tatums_per_beat);
subbeats = beatMeterProfile(2 : tatums_per_beat, :);
% Determine the maximum peak on the subbeats so we normalise the difference
% for comparison
maximumSubbeat = max(max(subbeats));
% Counts the number of alternations (first order differences) to determine
% the mean alternation.
numOfAlternations = (size(subbeats, 1) - 1) * size(subbeats, 2);

mean_quaver_alternation = sum(sum(abs(diff(subbeats)))) / (numOfAlternations * maximumSubbeat);

end

