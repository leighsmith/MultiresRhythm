function [ silence_score ] = silence_score_max( silence_evaluation_region, comparison_region )
%silence_score_max Indicate probability of the evaluated region being silent.
%   Normalised values indicate the silence evaluation region is more likely to be silence.
% $Id$

epsilon = 0.001; % Avoid div by 0 errors.
silence_score = 1 - (max(silence_evaluation_region) / (max(comparison_region) + epsilon));
fprintf('max whole %.3f max silence %.3f\n', max(comparison_region), max(silence_evaluation_region));
fprintf('silence score from ratio of local and global maxima %.3f\n', silence_score);

end

