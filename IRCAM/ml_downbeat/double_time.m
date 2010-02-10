function [ double_time_prob ] = double_time( rhythm_description )
%double_time Summary of this function goes here
%   Detailed explanation goes here

original_time_pattern = pattern_of_rhythm_description(rhythm_description);
half_time_rhythm_description = rhythm_description;
half_time_rhythm_description.beat_times = rhythm_description.beat_times(1 : 2 : end);
half_time_rhythm_description.beat_markers = rhythm_description.beat_markers(1 : 2: end);

half_time_pattern = pattern_of_rhythm_description(half_time_rhythm_description);

% These are normalised, so 0 indicates low alternation
original_quaver_alternation = quaver_alternation(original_time_pattern)
half_time_quaver_alternation = quaver_alternation(half_time_pattern)

double_time_prob = half_time_quaver_alternation / original_quaver_alternation;
% threshold = mean(double_time_prob) + std(double_time_prob);

end

