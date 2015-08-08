function [ profile ] = hypermetrical_profile( silence_observations, hypermetrical_period )
%hypermetrical_profile Compute the metrical profile over a hypermetrical period (multiple measures).
% Use hypermetrical_period = 4 bars for common hypermeter for pop/rock.
% $Id$

tatums_per_measure = size(silence_observations, 1);
num_of_measures = size(silence_observations, 2);

num_of_hypermeasures = floor(num_of_measures / hypermetrical_period);
shortened_num_of_measures = num_of_hypermeasures * hypermetrical_period;
shortened_silences = silence_observations(:, 1 : shortened_num_of_measures);

% reshape into a hypermetrical period so we average across the whole period.
phrase_of_silence_obs = reshape(shortened_silences, tatums_per_measure * hypermetrical_period, num_of_hypermeasures);

profile = 1 - (sum(phrase_of_silence_obs, 2) ./ num_of_hypermeasures)';

end

