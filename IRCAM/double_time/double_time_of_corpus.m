function [ double_time_probs ] = double_time_of_corpus ( corpus, sound_directory_root )
%double_time_of_corpus Returns the double time probability scores for all
%files in the corpus. 
%   Computes and caches the original time and double time patterns.
%   Column order for quaver alternation: doubled time, half time, counter phase.
diag = false;

double_time_probs = zeros(length(corpus), 4);
for piece_index = 1 : length(corpus)
    beat_markers_filepath = corpus{1, piece_index};
    % rhythm_description = 0;
            
    original_pattern = filtered_pattern(beat_markers_filepath, sound_directory_root, 'Pattern');
    half_time_pattern = filtered_pattern(beat_markers_filepath, sound_directory_root, 'HalfPattern', @half_time_rhythm_description);
    counter_phase_pattern = filtered_pattern(beat_markers_filepath, sound_directory_root, 'CounterPhasePattern', @counter_phase_rhythm_description);
    double_time_pattern = filtered_pattern(beat_markers_filepath, sound_directory_root, 'DoubleTimePattern', @double_time_rhythm_description);
 
    % Display the reduced metrical pattern as a single bar graph.
    if(diag)
        figure();
        subplot(4, 1, 1);
        bar(reducedMetricalProfile(original_pattern));
        axis([1, length(reducedMetricalProfile(original_pattern)), 0.0, 1.0]);
        title(sprintf('Reduced metrical profile of original %s', original_pattern.name));
        subplot(4, 1, 2);
        bar(reducedMetricalProfile(half_time_pattern));
        axis([1, length(reducedMetricalProfile(half_time_pattern)), 0.0, 1.0]);
        title(sprintf('Reduced metrical profile of half time %s', half_time_pattern.name));
        subplot(4, 1, 3);
        bar(reducedMetricalProfile(counter_phase_pattern));
        axis([1, length(reducedMetricalProfile(counter_phase_pattern)), 0.0, 1.0]);
        title(sprintf('Reduced metrical profile of counter phase half time %s', counter_phase_pattern.name));
        subplot(4, 1, 4);
        bar(reducedMetricalProfile(double_time_pattern));
        axis([1, length(reducedMetricalProfile(double_time_pattern)), 0.0, 1.0]);
        title(sprintf('Reduced metrical profile of double time %s', double_time_pattern.name));
    end
    
    % double_time_probs(piece_index, 2) = double_time(original_pattern, counter_phase_pattern);
    % double_time_probs(piece_index, 3) = double_time(original_pattern, half_time_pattern);
    % double_time_probs(piece_index, 1) = double_time(double_time_pattern, original_pattern);
    
    % > 1 if there is less quaver alternation at the double rate than the original.
    double_time_probs(piece_index, 1) = quaver_alternation(original_pattern) / quaver_alternation(double_time_pattern);
    % > 1 if there is more quaver alternation at the half beat rate than the original.
    double_time_probs(piece_index, 2) = quaver_alternation(counter_phase_pattern) / quaver_alternation(original_pattern);
    double_time_probs(piece_index, 3) = quaver_alternation(half_time_pattern) / quaver_alternation(original_pattern);

    % double_time_probs(piece_index, 4) = quaver_alternation(half_time_pattern) / quaver_alternation_beats(double_time_pattern);
    % > 1 if the quaver at the half beat rate is greater than the double rate.
    double_time_probs(piece_index, 4) = (quaver_alternation(half_time_pattern) * quaver_alternation_beats(double_time_pattern)) / (quaver_alternation(original_pattern) ^ 2);
end

end

function half_time_rd = half_time_rhythm_description(rd)
    half_time_rd = rd;
    half_time_rd.beat_times = rd.beat_times(1 : 2 : end);
    half_time_rd.beat_markers = rd.beat_markers(1 : 2: end);
end

function counter_phase_rd = counter_phase_rhythm_description(rd)
    counter_phase_rd = rd;
    counter_phase_rd.beat_times = rd.beat_times(2 : 2 : end);
    counter_phase_rd.beat_markers = rd.beat_markers(2 : 2: end);
end

function double_time_rd = double_time_rhythm_description(rd)
    double_time_rd = rd;
    
    bisected_durations = diff(rd.beat_times) / 2;
    double_times = [rd.beat_times, rd.beat_times + [bisected_durations; bisected_durations(end)]];
    double_time_rd.beat_times = reshape(double_times', 1, numel(double_times))';
    % Too difficult to do and unnecessary at this stage, requires renumbering.
    % double_time_rd.beat_markers = rd.beat_markers(2 : 2: end);
end
