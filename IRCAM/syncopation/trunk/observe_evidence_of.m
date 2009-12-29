function [ feature_estimates ] = observe_evidence_of(rhythm_odf, rhythm_description, subdivisions_of_beat, feature_estimator)
%observe_evidence_of Returns an estimate of a feature across the entire ODF rhythm using an estimator function.
% $Id$

% set(title, 'Interpreter','none');
measures_to_plot = [77]; % should be global.

beat_times = rhythm_description.beat_times;
beats_per_measure = rhythm_description.beats_per_measure;
sample_rate = rhythm_description.sample_rate;

%% Convert beat_times in seconds to durations of each beat in samples.
beat_durations = round(diff(beat_times) * sample_rate);
%% Reduce by one measure to ensure the last search region is a full two bars,
%% avoiding a bias in the feature_estimator towards starting beats.
number_of_measures = floor(length(beat_durations) / beats_per_measure) - 1; % floor avoids partial bars
feature_estimates = zeros((beats_per_measure * subdivisions_of_beat), number_of_measures);

fprintf('\nrhythm duration %.3f samples, number of measures %.3f\n', length(rhythm_odf), number_of_measures); 
% fprintf('First beat_durations %s\n', sprintf('%.3f ', beat_durations(1 : 16)))

for measure_index = 1 : number_of_measures
    % in samples
    beat_duration_index = (beats_per_measure * (measure_index - 1));
    beat_durations_in_measure = beat_durations(beat_duration_index  + 1 : beat_duration_index + beats_per_measure);
    bar_duration = sum(beat_durations_in_measure); % in samples
    start_sample = round(beat_times(((measure_index - 1) * beats_per_measure) + 1) * sample_rate) + 1;
    fprintf('Measure %3d start sample %d seconds %f\n', measure_index, start_sample, start_sample / sample_rate);
    fprintf('beat duration in samples %s = %.3f\n', sprintf('%.3f ', beat_durations_in_measure), bar_duration);

    %% collect probabilities of the feature occuring at each measure
    %% location. These can be of the form of a normalised probability
    %% across the measure, or a probability for each tatum, depending on
    %% the feature_estimator.
    feature_probabilities = feature_estimator(rhythm_odf, start_sample, measure_index, beat_durations_in_measure, subdivisions_of_beat);

    % fprintf('Feature %s probabilities %s sum = %f\n', feature_estimator, sprintf('%.5f ', feature_probabilities), sum(feature_probabilities));
   
    if (diag_plot('gap_evaluation') && ismember(measures_to_plot, measure_index))
       search_region = rhythm_odf(start_sample : min(length(rhythm_odf), start_sample + (bar_duration * 2) - 1));

       % set(hlines(1), 'Displayname', 'ODF');
       % set(hlines(2), 'Displayname', 'beat gap likelihood');
       % set(hlines(3), 'Displayname', 'beat location');
       figure();
       plot(1 : length(search_region), normalise(search_region), ...
            onsets_at_subdivisions(beat_durations_in_measure, subdivisions_of_beat) + 1, ...
            feature_probabilities,'-+')
            %onsets_at_subdivisions(beat_durations_in_measure, 1) + 1, ...
            %feature_probabilities(1 : subdivisions_of_beat : beats_per_measure * subdivisions_of_beat))

       title(sprintf('plot of measure %.3f of %s', measure_index, rhythm_description.name),'Interpreter','none');
    end
    % collect likelihood in feature_estimates
    feature_estimates(:, measure_index) = feature_probabilities;
end


end

