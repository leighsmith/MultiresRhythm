function [ downbeat_estimates ] = observe_downbeat_of( annotated_rhythm, subdivisions_of_beat, downbeat_estimator )
%observe_downbeat_of Returns an estimate of downbeat location across the entire ODF rhythm using an estimator
%   Detailed explanation goes here

measures_to_plot = [30, 31, 32, 33]; % should be global.
plotting = ''; % should be global.

rhythm = annotated_rhythm.odf;
beat_times = annotated_rhythm.beat_times;
beats_per_measure = annotated_rhythm.beats_per_measure;
sample_rate = annotated_rhythm.sample_rate;

%% Convert beat_times in seconds to durations of each beat in samples.
beat_durations = round(diff(beat_times) * sample_rate);
%% Reduce by one measure to ensure the last search region is a full two bars,
%% avoiding a bias in the downbeat_estimator towards starting beats.
number_of_measures = floor(length(beat_durations) / beats_per_measure) - 1; % floor avoids partial bars
downbeat_estimates = zeros((beats_per_measure * subdivisions_of_beat), number_of_measures);

fprintf('\nrhythm duration %.3f samples, number of measures %.3f\n', length(rhythm), number_of_measures); 
% fprintf('First beat_durations %s\n', sprintf('%.3f ', beat_durations(1 : 16)))

for measure_index = 1 : number_of_measures
    % in samples
    beat_duration_index = (beats_per_measure * (measure_index - 1));
    beat_durations_in_measure = beat_durations(beat_duration_index  + 1 : beat_duration_index + beats_per_measure);
    bar_duration = sum(beat_durations_in_measure); % in samples
    start_sample = round((beat_times(((measure_index - 1) * beats_per_measure) + 1) - beat_times(1)) * sample_rate) + 1;
    % fprintf('Measure %3d start sample %d\n', measure_index, start_sample);
    % fprintf('beat duration in samples %s = %.3f\n', sprintf('%.3f ', beat_durations_in_measure), bar_duration);

    %% collect probabilities of the downbeat occuring at each measure location.    
    downbeat_probabilities = downbeat_estimator(rhythm, start_sample, measure_index, beat_durations_in_measure, subdivisions_of_beat);

    % fprintf('Downbeat probabilities %s\n', sprintf('%.5f ', downbeat_probabilities));
   
    if (strmatch('gap_evaluation', plotting) & ismember(measures_to_plot, measure_index))
       search_region = rhythm(start_sample : min(length(rhythm), start_sample + (bar_duration * 2) - 1));
       title(sprintf('plot of measure %.3f', measure_index));
       set(hlines(1), 'Displayname', 'ODF');
       set(hlines(2), 'Displayname', 'beat gap likelihood');
       set(hlines(3), 'Displayname', 'beat location');

       plot(normalize(search_region), 0 : length(search_region) - 1, ...
            downbeat_probabilities, onsets_at_subdivisions(beat_durations_in_measure, subdivisions_of_beat), ...
            downbeat_probabilities(0 : subdivisions_of_beat : beats_per_measure * subdivisions_of_beat), ...
            onsets_at_subdivisions(beat_durations_in_measure, 1))
    end
    % collect likelihood in downbeat_estimates
    downbeat_estimates(:, measure_index) = downbeat_probabilities;
end


end

