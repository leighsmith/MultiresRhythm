function [ silence_score ] = silence_score_mean( silence_evaluation_region, comparison_region_mean, comparison_region_std, debug )
%silence_score_mean Returns a normalised value indicating the ratio of the mean of the
% silence evaluation region compared to 2 standard deviations above the mean of the comparison region.
% Higher values are more likely to be silence.
% $Id$

epsilon = 0.0001;
threshold = 3.0; % Number of standard deviations above the mean to be sure it is an onset.

mean_ratios = mean(silence_evaluation_region) / (comparison_region_mean + threshold * comparison_region_std + epsilon);

% Clip the ratio at 1, anything above this is clearly an onset.
if (mean_ratios > 1)
    mean_ratios = 1;
end

silence_score = 1 - mean_ratios;

if(debug)
    % fprintf('silence-eval-region length %d sum %f\n', length(silence_evaluation_region), sum(silence_evaluation_region));
    fprintf('mean_silence_region %.3f, mean_comparison_region %.3f\n', mean(silence_evaluation_region), comparison_region_mean);
    fprintf('silence score from ratio of local and global means %.3f\n', silence_score);
end

end

