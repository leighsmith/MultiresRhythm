function [ profile ] = hypermetrical_profile( silence_observations )
%hypermetrical_profile Compute the metrical profile over 4 measures.
%   Detailed explanation goes here

tatums_per_measure = size(silence_observations, 1);
num_of_measures = size(silence_observations, 2);
% Use fixed 4 bar measure hypermeter for pop/rock
hypermetrical_period = 4;
% reshape into a hypermetrical period so we average across the whole period.
shortened_silences = silence_observations(:, 1 : num_of_measures - mod(num_of_measures, hypermetrical_period));
phrase_of_silence_obs = reshape(shortened_silences, tatums_per_measure * hypermetrical_period, floor(num_of_measures / hypermetrical_period));

profile = 1 - normalise(sum(phrase_of_silence_obs') ./ num_of_measures);

end

