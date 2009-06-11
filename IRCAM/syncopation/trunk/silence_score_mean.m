function [ silence_score ] = silence_score_mean( silence_evaluation_region, comparison_region, debug )
%silence_score_mean Values > 1 indicate the variation and mean of the silence evaluation region is less
%   than the comparison region, i.e more likely to be silence.
% $Id$

epsilon = 0.0001;

mean_silence_region = mean(silence_evaluation_region);

% We use the reciprocal of the mean of the silence region to account for the absolute magnitude of the signal.

silence_score = 1.0 / (mean_silence_region + epsilon);

if(debug)
    fprintf('silence-eval-region length %d sum %f\n', length(silence_evaluation_region), sum(silence_evaluation_region));
end


end

