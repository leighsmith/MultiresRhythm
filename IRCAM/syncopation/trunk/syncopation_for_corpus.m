function [ corpus_syncopation ] = syncopation_for_corpus( corpus )
%syncopation_for_corpus Generate syncopation pattern measures for the corpus.
% Assumes corpus is a cell array of strings.

% We assume all members of the corpus are the same meter.
corpus_syncopation = zeros(length(corpus), 16);

for piece_index = 1 : length(corpus)
    piece = corpus{1, piece_index};
    
    try
        syncopation_profile = read_syncopation(piece);
    catch
        analysed_rhythm = read_analysed_rhythm(piece);
        syncopation_profile = eval_syncopation_measures(analysed_rhythm);
        write_syncopation(piece, syncopation_profile);
    end
    corpus_syncopation(piece_index, :) = syncopation_profile;
end
    
end

