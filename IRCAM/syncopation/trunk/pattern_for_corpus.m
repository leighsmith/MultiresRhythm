function [ corpus_rhythm_patterns ] = pattern_for_corpus( corpus, quaero_directory )
%pattern_for_corpus Returns a cell array of matrices of rhythm pattern measures for the entire corpus.
% Assumes corpus is a cell array of strings.
% TODO Assumes all members of the corpus are the same meter.
% $Id$

corpus_rhythm_patterns = cell(length(corpus),1);
annotation_type = '.beat';
rhythm_directory_root = tilde_expand(['~/Research/Data/IRCAM-Beat/' quaero_directory '/']);
        
for piece_index = 1 : length(corpus)
    piece = corpus{1, piece_index};
    pattern_filepath = [rhythm_directory_root 'Analysis/' piece '.pattern.xml'];
    
    try
        pattern = read_pattern(piece, pattern_filepath);
    catch no_pattern
        % We use the annotated rhythms to ensure the downbeats are correct.
        rhythm_description = read_quaero_rhythm(piece, rhythm_directory_root, annotation_type);
        pattern = pattern_of_rhythm_description(rhythm_description);
        write_pattern(pattern, piece, tilde_expand(pattern_filepath));
    end
    if (diag_plot('syncopation_profile'))
        plot_pattern(pattern);
    end
    corpus_rhythm_patterns{piece_index, 1} = pattern;
end
    
end

