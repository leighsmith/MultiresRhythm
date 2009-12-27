function [ profile ] = hypermetrical_profile( silence_observations, hypermetrical_period )
%hypermetrical_profile Compute the metrical profile over 4 measures.
% Use fixed 4 bar measure hypermeter for pop/rock.

tatums_per_measure = size(silence_observations, 1);
num_of_measures = size(silence_observations, 2);

% reshape into a hypermetrical period so we average across the whole period.
shortened_silences = silence_observations(:, 1 : num_of_measures - mod(num_of_measures, hypermetrical_period));
num_of_hypermeasures = floor(num_of_measures / hypermetrical_period);
phrase_of_silence_obs = reshape(shortened_silences, num_of_hypermeasures, tatums_per_measure * hypermetrical_period);

profile = 1 - (sum(phrase_of_silence_obs, 1) ./ num_of_hypermeasures);

end

