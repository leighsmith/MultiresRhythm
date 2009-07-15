function [ pattern ] = pattern_of_rhythm_description ( rhythm_description )
%pattern_of_rhythm Returns a pattern instance for the given rhythm.
% The pattern includes the metrical profile and the profile of syncopation
% per measure.
% $Id$

rhythm_name = rhythm_description.name;
[onset_observations, silence_observations] = observe_onsets(rhythm_description);
dim = size(onset_observations);
num_of_measures = dim(2);

rhythm_syncopation_measures = syncopation_measures(onset_observations, rhythm_description.meter);
% TODO, we normalise the syncopation measure, to produce an equally
% comparable measure for each tatum, however this seems counter to the goal
% of the measure, which is to measure the effect at different locations
% across the measure.
normalised_syncopation_measures = normalise_syncopation(rhythm_syncopation_measures, rhythm_description.meter);
dim = size(normalised_syncopation_measures);
num_of_tatums = dim(1); % recalc this in case the syncopation measures differ from the tatums.
syncopation_profile = sum(normalised_syncopation_measures') ./ num_of_measures;

metrical_profile = 1 - normalise(sum(silence_observations') ./ num_of_measures);
% metrical_profile = sum(onset_observations') ./ num_of_measures;
%% fprintf('metric profile %s syncopation_profile %s~%', metric_profile, syncopation_profile)

pattern = RhythmPattern(rhythm_name, syncopation_profile, metrical_profile);

% While this finds onset periodicities, in many ways it duplicates 
% the ACF * FFT function.
% onsets_signal = onset_likelihood(silence_observations);
% figure();
% bar(onsets_signal)
% acf = xcorr(onsets_signal,'biased')
% plot(acf(length(onsets_signal) : end))
% [max, indices] = sort(acf(length(onsets_signal) + 1 : end), 'descend');
% indices(1:10)
if (diag_plot('hypermeter'))
    figure();
    bar(hypermetrical_profile(silence_observations))
end
    
if (diag_plot('syncopation_profile'))
    plot_pattern(pattern);
end

if (diag_plot('syncopation_measures'))
    figure();
    imagesc(rhythm_syncopation_measures)
    % 	   :aspect_ratio 0.66
    title(sprintf('Syncopation intensity for %s', rhythm_name),'Interpreter','none');
    %close();
end

if (diag_plot('syncopation_variation'))
    syncopation_variation = sum(normalised_syncopation_measures) ./ num_of_tatums;
    figure();
    plot(syncopation_variation);
    % 	  :aspect_ratio 0.66
    title(sprintf('Evolution of syncopation of %s', rhythm_name),'Interpreter','none')
    % close();
end
  
end
