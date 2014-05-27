function [ silence_probabilities ] = silence_evidence(rhythm_to_analyse, measure_start_sample, measure_index, beat_durations_in_measure, subdivisions_of_beat)
%silence_evidence From the rhythm_to_analyse, for one measure, starting at measure_start_sample, determines if each 
% beat and subdivision thereof is a silent region. Each individual tatum is returned
% as a normalised probability of being silence.
% $Id$

tatum_locations = onsets_at_subdivisions(beat_durations_in_measure, subdivisions_of_beat);
tatums_per_measure = length(tatum_locations);
tatum_score = zeros(1, tatums_per_measure);
tatum_durations = beat_durations_in_measure ./ subdivisions_of_beat;
rhythm_length = length(rhythm_to_analyse);
mean_odf = mean(rhythm_to_analyse);
std_odf = std(rhythm_to_analyse);

for tatum_index = 1 : tatums_per_measure
    tatum_location = tatum_locations(tatum_index);
    tatum_duration = tatum_durations(floor((tatum_index - 1) / subdivisions_of_beat) + 1);
    gap_start = round(measure_start_sample + tatum_location + (tatum_duration * -0.5));
    gap_end = min(round(gap_start + tatum_duration), rhythm_length);
    % tatum_score(tatum_index) = silence_score_max(rhythm_to_analyse(max(1, gap_start) : gap_end), rhythm_to_analyse);
    % tatum_score(tatum_index) = silence_score_stddev(rhythm_to_analyse(max(1, gap_start) : gap_end - 1), rhythm_to_analyse, 1);
    tatum_score(tatum_index) = silence_score_mean(rhythm_to_analyse(max(1, gap_start) : gap_end - 1), mean_odf, std_odf, 0);

    %fprintf('Measure %d tatum location %.2f samples, silence region (%d %d) score = %.3f\n', ...
	%      measure_index, tatum_location, gap_start, gap_end, tatum_score(tatum_index));
end

silence_probabilities = tatum_score; % means tatum score has to be normalised by silence_score_*
end
