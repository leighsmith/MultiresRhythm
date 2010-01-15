function [ corpus_rhythm_patterns ] = pattern_for_corpus(corpus, sound_directory_root)
%pattern_for_corpus Returns a cell array of matrices of rhythm pattern measures for the entire corpus.
% Assumes corpus is a cell array of strings giving full pathnames to each beat file.
% TODO Assumes all members of the corpus are the same meter.
% $Id$

corpus_rhythm_patterns = cell(length(corpus),1);
        
for piece_index = 1 : length(corpus)
    beat_markers_filepath = corpus{1, piece_index};
    [filepath, filename] = fileparts(beat_markers_filepath);
    % fileparts differs from ircambeat's notion of extension, taking the last period as the
    % start of the extension, not the first.
    piece_name = strtok(filename, '.');
    pattern_filepath = [filepath '/Pattern/' piece_name '.pattern.xml'];
    
    try
        pattern = read_pattern(piece_name, pattern_filepath);
    catch no_pattern
        audio_filepath = [sound_directory_root '/' piece_name '.wav']

        rhythm_description = read_rhythm_description(beat_markers_filepath, audio_filepath);
        pattern = pattern_of_rhythm_description(rhythm_description);
        write_pattern(pattern, piece_name, pattern_filepath);
    end
    pattern.periodicities = read_qima_periodicities_file([filepath '/' piece_name '.wav.bpm.xml']);
    if (diag_plot('syncopation_profile'))
        plot_pattern(pattern);
    end
    corpus_rhythm_patterns{piece_index, 1} = pattern;
end
    
end

