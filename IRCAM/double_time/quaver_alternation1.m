function [ mean_quaver_alternation ] = quaver_alternation1( pattern )
%quaver_alternation Produces a metric indicating how much the quaver
%patterns alternate across the measure.

metricalProfile = reducedMetricalProfile(pattern);

% Remove the beat onsets since they will be large relative to the offbeats.
metricalProfile = metricalProfile(strip_beats(metricalProfile, 4));

% numOfQuavers = length(metricalProfile) / 2;

% This version assumes the first beat exceeds the second.
% mean_quaver_alternation = sum(abs(metricalProfile(1 : 2 : end) - metricalProfile(2 : 2 : end))) / numOfQuavers;
% This verifies an up/down motion.
mean_quaver_alternation = sum(abs(diff(metricalProfile))) / ((length(metricalProfile) - 1) * max(metricalProfile));

end

