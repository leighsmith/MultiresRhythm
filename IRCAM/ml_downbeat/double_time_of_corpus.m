function [ double_time_prob ] = double_time_of_corpus ( corpus, sound_directory_root )
%double_time_of_corpus Returns the double time probability scores for all
%files in the corpus.
%   Detailed explanation goes here

double_time_prob = zeros(length(corpus), 1);
for piece_index = 1 : length(corpus)
    beat_markers_filepath = corpus{1, piece_index};
    [filepath, filename] = fileparts(beat_markers_filepath);
    % fileparts differs from ircambeat's notion of extension, taking the last period as the
    % start of the extension, not the first.
    piece_name = strtok(filename, '.');
    pattern_filepath = [filepath '/Pattern/' piece_name '.pattern.xml'];
    audio_filepath = [sound_directory_root '/' piece_name '.wav']
    rhythm_description = 0;
    half_time_pattern_filepath = [filepath '/HalfPattern/' piece_name '.pattern.xml'];
            
    try
        pattern = read_pattern(piece_name, pattern_filepath);
    catch no_pattern
        rhythm_description = read_rhythm_description(beat_markers_filepath, audio_filepath);
        pattern = pattern_of_rhythm_description(rhythm_description);
        write_pattern(pattern, piece_name, pattern_filepath);
    end
    
    try
        half_time_pattern = read_pattern(piece_name, half_time_pattern_filepath);
    catch
        if(rhythm_description == 0)
            half_time_rhythm_description = read_rhythm_description(beat_markers_filepath, audio_filepath);
        else
            half_time_rhythm_description = rhythm_description;
        end
        half_time_rhythm_description.beat_times = half_time_rhythm_description.beat_times(1 : 2 : end);
        half_time_rhythm_description.beat_markers = half_time_rhythm_description.beat_markers(1 : 2: end);

        half_time_pattern = pattern_of_rhythm_description(half_time_rhythm_description);
        write_pattern(half_time_pattern, piece_name, half_time_pattern_filepath);
    end
    
    double_time_prob(piece_index) = double_time(rhythm_description);

end

end

