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
        pattern_read = read_pattern(patt, piece);
        corpus_rhythm_pattern(piece_index, :) = [strip_beats(pattern_read.syncopation, 4) pattern_read.metrical_profile];
    catch
        analysed_rhythm = read_analysed_rhythm(piece);
        [syncopation_profile, metrical_profile] = eval_syncopation_measures(analysed_rhythm);
        pattern_to_write = RhythmPattern(syncopation_profile, metrical_profile);
        write_pattern(pattern_to_write, piece);
        % Concatenate the syncopation and metrical profiles.
        % TODO Hardwired at 4 tatums per beat. 
        corpus_rhythm_pattern(piece_index, :) = [strip_beats(syncopation_profile, 4) metrical_profile];
    end
end
    
end

