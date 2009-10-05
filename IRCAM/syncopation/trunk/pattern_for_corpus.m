function [ corpus_rhythm_pattern ] = pattern_for_corpus( corpus, quaero_directory )
%pattern_for_corpus Returns a matrix of rhythm pattern measures for the corpus.
% Assumes corpus is a cell array of strings.
% Assumes all members of the corpus are the same meter.
% $Id$

corpus_rhythm_pattern = zeros(length(corpus), featureVectorLength(RhythmPattern('')));
annotation_type = '.beat';
% annotation_type = '.b';


for piece_index = 1 : length(corpus)
    piece = corpus{1, piece_index};
    
    try
        pattern = read_pattern(piece, quaero_directory);
    catch
        % We use the annotated rhythms to ensure the downbeats are correct.
        rhythm_description = read_annotated_rhythm(piece, ['~/Research/Data/IRCAM-Beat/', quaero_directory, '/'], annotation_type);
        pattern = pattern_of_rhythm_description(rhythm_description);
        write_pattern(pattern, piece);
    end
    if (diag_plot('syncopation_profile'))
        plot_pattern(pattern);
    end
    corpus_rhythm_pattern(piece_index, :) = featureVector(pattern);
end
    
end

