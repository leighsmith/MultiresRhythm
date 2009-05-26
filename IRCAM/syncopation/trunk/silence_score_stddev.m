function [ silence_score ] = silence_score_stddev (silence_evaluation_region, comparison_region)
%silence_score_stddev Values > 1 indicate the variation and mean of the silence evaluation region is less
%   than the comparison region, i.e more likely to be silence.

epsilon = 0.0001;

%% Calculate the stddev & mean over the region of interest, 2 measures.
stddev_comparison = std(comparison_region);
mean_comparison = mean(comparison_region);

%% Stddev's are just RMS measures of the amplitude envelope. 
stddev_silence_region = std(silence_evaluation_region);
mean_silence_region = mean(silence_evaluation_region);
silence_score = ((stddev_comparison / (stddev_silence_region + epsilon)) * (mean_comparison / (mean_silence_region + epsilon))) / ...
                    (mean_silence_region + epsilon);
%fprintf('silence-eval-region length %d sum %d\n', length(silence_evaluation_region), sum(silence_evaluation_region));
%fprintf('comparison stddev %.3f mean %.3f\n', stddev_comparison, mean_comparison);
%fprintf('silence stddev %.3f mean %.3f\n', stddev_silence_region, mean_silence_region);
%fprintf('reciprocal mean %.3f\n', (/ 1.0d0 (mean_silence_region + epsilon)))
%fprintf('silence score from intersection of stddev & mean ratios %.3f\n', silence_score)

end

