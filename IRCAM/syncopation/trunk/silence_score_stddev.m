function [ silence_score ] = silence_score_stddev (silence_evaluation_region, comparison_region, debug)
%silence_score_stddev Values > 1 indicate the variation and mean of the silence evaluation region is less
%   than the comparison region, i.e more likely to be silence.

epsilon = 0.0001;

%% Calculate the stddev & mean over the region of interest, 2 measures.
stddev_comparison = std(comparison_region);
mean_comparison = mean(comparison_region);
% cov = coefficient of variance, not covariance matrix!
cov_comparison = (stddev_comparison / (mean_comparison + epsilon));

%% Stddev's are just RMS measures of the amplitude envelope. 
stddev_silence_region = std(silence_evaluation_region);
mean_silence_region = mean(silence_evaluation_region);
cov_silence_region = (stddev_silence_region / (mean_silence_region + epsilon));

% We compute the ratio of the comparison region against the silence region
% so larger values are greater likelihoods of silence.

% Multiplying the mean and stddev ratios produces a joint measure. The stddev and mean are both in units of ODF. 

% We multiply by the reciprocal of the mean of the silence region to account for the absolute magnitude of the signal.
% The combined score produces a relative measure of silence likelihood.

% silence_score = ((stddev_comparison / (stddev_silence_region + epsilon)) * (mean_comparison / (mean_silence_region + epsilon))) / ...
%                    (mean_silence_region + epsilon);

% ratios of the coefficients of variation
% silence_score_old = (stddev_comparison / (stddev_silence_region + epsilon)) * (mean_silence_region / (mean_comparison + epsilon))

% silence_score = cov_comparison / cov_silence_region;

% silence_score = cov_comparison / (cov_silence_region * (mean_silence_region + epsilon));
silence_score = 1.0 / (mean_silence_region + epsilon);

if(debug)
    fprintf('silence-eval-region length %d sum %f\n', length(silence_evaluation_region), sum(silence_evaluation_region));
    fprintf('comparison stddev %.3f mean %.3f cov %.3f\n', stddev_comparison, mean_comparison, cov_comparison);
    fprintf('silence stddev %.3f mean %.3f cov %.3f\n', stddev_silence_region, mean_silence_region, cov_silence_region);
    fprintf('reciprocal mean %.3f\n', 1.0 / (mean_silence_region + epsilon))
%fprintf('silence score from intersection of stddev & mean ratios %.3f\n', silence_score)
end

end

