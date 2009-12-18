function [ corpus_rhythm_patterns ] = pattern_for_corpus( corpus, rhythm_directory_root )
%pattern_for_corpus Returns a cell array of matrices of rhythm pattern measures for the entire corpus.
% Assumes corpus is a cell array of strings.
% TODO Assumes all members of the corpus are the same meter.
% $Id$

corpus_rhythm_patterns = cell(length(corpus),1);
annotation_type = '.beat';
        
for piece_index = 1 : length(corpus)
    piece = corpus{1, piece_index};
    pattern_filepath = [rhythm_directory_root 'Analysis/' piece '.pattern.xml'];
    
    try
        pattern = read_pattern(piece, pattern_filepath);
    catch no_pattern
        % We use the annotated rhythms to ensure the downbeats are correct.
        abs_rhythm_directory = tilde_expand(rhythm_directory_root);
        sound_directory_root = '/Volumes/Quaerodb/music/Annotation Corpus - 1000 C/WAV stereo 44k/';
        % sound_directory_root =  [abs_rhythm_directory 'Audio/']

        beat_markers_filepath = [abs_rhythm_directory 'Annotation/' piece annotation_type '.xml'];
        audio_filepath = [sound_directory_root piece '.wav'];

        rhythm_description = read_rhythm_description(beat_markers_filepath, audio_filepath);
        pattern = pattern_of_rhythm_description(rhythm_description);
        write_pattern(pattern, piece, tilde_expand(pattern_filepath));
    end
    if (diag_plot('syncopation_profile'))
        plot_pattern(pattern);
    end
    corpus_rhythm_patterns{piece_index, 1} = pattern;
end
    
end

