function [ similarity_matrix ] = corpus_similarity(corpus_name)
%corpus_similarity Returns the similarity matrix of the corpus named by corpus_name.
% for example: corpus_similarity('Quaero_Selection')
% $Id$

global plotting;

% This is crap, it shouldn't be defined here, but because Matlab methods
% are not proper functions and require a base type, we have to.
plotting = {}; 
set_diag_plot('similarity_matrix')

corpus_dataset = make_quaero_dataset(corpus_name);

% Writes out new syncopation measures.
corpus_patterns = pattern_for_corpus(corpus_dataset, corpus_name);

similarity_matrix = pattern_similarity(corpus_patterns);

closest_song_indices = closest_rhythms(similarity_matrix);

% Print most similar to
for i = 1 : length(corpus_dataset)
    fprintf('%d: (%.3f) "%s" most similar to "%s"\n', ...
            i, similarity_matrix(i, closest_song_indices(i)), corpus_dataset{i}, corpus_dataset{closest_song_indices(i)})
end

end

