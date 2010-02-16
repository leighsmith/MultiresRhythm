function [ mean_quaver_alternation ] = quaver_alternation_beats( pattern )
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

% Determine the maximum peak on the beat meter profile so we normalise the difference
% for comparison
maximumSubbeat = max(max(beatMeterProfile));
% Counts the number of alternations (first order differences) to determine
% the mean alternation.
numOfAlternations = numel(beatMeterProfile);

mean_quaver_alternation = sum(sum(abs(diff(beatMeterProfile)))) / (numOfAlternations * maximumSubbeat);

end

