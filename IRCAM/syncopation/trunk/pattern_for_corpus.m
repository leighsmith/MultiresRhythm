function [ corpus_rhythm_pattern ] = pattern_for_corpus( corpus )
%pattern_for_corpus Returns a matrix of rhythm pattern measures for the corpus.
% Assumes corpus is a cell array of strings.
% Assumes all members of the corpus are the same meter.
% A pattern for each rhythm is a concatenation of the syncopation and the metrical profile of each rhythm,
% with the tactus beats of the syncopation removed since they will never
% return a syncopation and only increase the dimensionality of the distance measures.
% $Id$

corpus_rhythm_pattern = zeros(length(corpus), 12 + 16); % TODO hardwired to 16 tatums.

for piece_index = 1 : length(corpus)
    piece = corpus{1, piece_index};
    
    try
        patt = RhythmPattern([],[]);
        pattern = read_pattern(patt, piece);
    catch
        % We use the annotated rhythms to ensure the downbeats are correct.
        rhythm_description = read_annotated_rhythm(piece);
        pattern = pattern_of_rhythm_description(rhythm_description);
        write_pattern(pattern, piece);
    end
    % Concatenate the syncopation and metrical profiles.
    % TODO Hardwired at 4 tatums per beat. 
    corpus_rhythm_pattern(piece_index, :) = [strip_beats(pattern.syncopation, 4) pattern.metrical_profile];
end
    
end

