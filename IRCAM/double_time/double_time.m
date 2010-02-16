function [ double_time_prob ] = double_time( original_time_pattern, half_time_pattern )
%double_time compute the probability of the pattern being a double time
%(octave error) pattern.
%   Compares the quaver alternation scores of the original pattern and a
%   half-time pattern (assumed computed by skipping every second beat) 

% These are normalised, so 0 indicates low alternation
original_quaver_alternation = quaver_alternation(original_time_pattern);
half_time_quaver_alternation = quaver_alternation(half_time_pattern);

double_time_prob = half_time_quaver_alternation / original_quaver_alternation;
% threshold = mean(double_time_prob) + std(double_time_prob);

end

