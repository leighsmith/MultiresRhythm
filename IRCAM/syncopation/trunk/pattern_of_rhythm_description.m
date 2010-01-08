function [ pattern ] = pattern_of_rhythm_description ( rhythm_description )
%pattern_of_rhythm Returns a pattern instance for the given rhythm description.
% The pattern includes the metrical profile and the profile of syncopation
% per measure. To not access the ODF according to the downbeat, set the anacrusis to 0.
% TODO This should be a factory method.
% $Id$

pattern = RhythmPattern(rhythm_description.name);

% Calculate the syncopation measures for each spectral subband.
for subBandIndex = 1 : size(rhythm_description.odfs, 1)
    onset_observations = observe_onsets(rhythm_description.odfs(subBandIndex,:), rhythm_description, pattern.syncopation_tatums_per_beat);
    num_of_measures = size(onset_observations, 2);

    rhythm_syncopation_measures = syncopation_measures(onset_observations, rhythm_description.meter);
    % TODO, we normalise the syncopation measure, to produce an equally
    % comparable measure for each tatum, however this seems counter to the goal
    % of the measure, which is to measure the effect at different locations
    % across the measure.
    normalised_syncopation_measures = normalise_syncopation(rhythm_syncopation_measures, rhythm_description.meter);
    % recalc this in case the syncopation measures differ from the tatums.
    num_of_tatums = size(normalised_syncopation_measures, 1);
    syncopation_profile = sum(normalised_syncopation_measures') ./ num_of_measures;
    % this doesn't work according to Matlab because RhythmPattern is a value class, not a
    % handle class.
    % setSyncopation(pattern, syncopation_profile); 
    pattern.syncopation(subBandIndex,:) = syncopation_profile;
    %% fprintf('syncopation_profile %s~%', syncopation_profile)
    
    if (diag_plot('syncopation_profile'))
        plot_pattern(pattern);
    end

    if (diag_plot('syncopation_measures'))
        figure();
        imagesc(rhythm_syncopation_measures)
        % 	   :aspect_ratio 0.66
        title(sprintf('Syncopation intensity for %s', rhythm_description.name),'Interpreter','none');
        %close();
    end

    if (diag_plot('syncopation_variation'))
        syncopation_variation = sum(normalised_syncopation_measures) ./ num_of_tatums;
        figure();
        plot(syncopation_variation);
        % 	  :aspect_ratio 0.66
        title(sprintf('Evolution of syncopation of %s', rhythm_description.name),'Interpreter','none')
        % close();
    end
  
end 

% Calculate the metrical profile for each spectral subband.
for subBandIndex = 1 : size(rhythm_description.odfs, 1)
    % Use silence observations since they capture the amplitude, with a higher sampling rate across the measure.
    [small_onset_observations, small_silence_observations] = observe_onsets(rhythm_description.odfs(subBandIndex,:), rhythm_description, pattern.metric_tatums_per_beat);
    num_of_measures = size(small_silence_observations, 2);
    % Calculate the metrical profile from the silence observations, not the onset observations, so we account for dynamics accentuation.
    metrical_profile = 1 - (sum(small_silence_observations') ./ num_of_measures);
    %% fprintf('metric profile %s~%', metric_profile)
    % bar(1:16/64:16.75, metrical_profile)
    pattern.metrical_profile(subBandIndex,:) = metrical_profile;
end

[onset_observations, silence_observations] = observe_onsets(rhythm_description.wideband_odf, rhythm_description, pattern.syncopation_tatums_per_beat);
pattern.hypermetrical_profile = hypermetrical_profile(silence_observations, pattern.phrase_length);

pattern.tempo = rhythm_description.tempo;
pattern.beats_per_measure = rhythm_description.beats_per_measure;

% While this finds onset periodicities, in many ways it duplicates 
% the ACF * FFT function.
% onsets_signal = onset_likelihood(silence_observations);
% figure();
% bar(onsets_signal)
% acf = xcorr(onsets_signal,'biased')
% plot(acf(length(onsets_signal) : end))
% [max, indices] = sort(acf(length(onsets_signal) + 1 : end), 'descend');
% indices(1:10)
    
end
