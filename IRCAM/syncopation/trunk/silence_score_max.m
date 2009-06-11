function [ silence_score ] = silence_score_max( silence_evaluation_region, comparison_region )
%silence_score_max Indicate probability of the evaluated region being silent.
%   Values > 1 indicate the variation and mean of the silence evaluation region is less
%   than the comparison region, i.e more likely to be silence.
% $Id$

max_silence_region = max(silence_evaluation_region);
epsilon = 0.001; % Avoid div by 0 errors.
silence_score = max(comparison_region) / (max_silence_region + epsilon);
fprintf('max whole %.3f max silence %.3f\n', max(comparison_region), max_silence_region);
fprintf('silence score from ratio of local and global maxima %.3f\n', silence_score);

end

