function [ profile ] = hypermetrical_profile( silence_observations )
%hypermetrical_profile Summary of this function goes here
%   Detailed explanation goes here

dim = size(silence_observations);
tatums_per_measure = dim(1);
num_of_measures = dim(2);
% Use fixed 4 bar measure hypermeter for pop/rock
hypermetrical_period = 4;
% reshape into a hypermetrical period so we average across the whole period.
shortened_silences = silence_observations(:, 1 : num_of_measures - mod(num_of_measures, hypermetrical_period));
phrase_of_silence_obs = reshape(shortened_silences, tatums_per_measure * hypermetrical_period, floor(num_of_measures / hypermetrical_period));

profile = 1 - normalise(sum(phrase_of_silence_obs') ./ num_of_measures);

end

