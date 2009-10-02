function [ similarity_matrix ] = corpus_similarity(corpus_name)
%corpus_similarity Returns the similarity matrix of the corpus named by corpus_name.
% for example: corpus_similarity('Quaero_Selection')
% $Id$

global plotting;

% This is crap, it shouldn't be defined here, but because Matlab methods
% are not proper functions and require a base type, we have to.
plotting = {}; 
set_diag_plot('similarity_matrix')

select_quaero_names = make_quaero_dataset(100, 1, corpus_name);

% Writes out new syncopation measures.
select_patterns = pattern_for_corpus(select_quaero_names, corpus_name);

similarity_matrix = pattern_similarity(select_patterns);

closest_song_indices = closest_rhythms(similarity_matrix);

% Print most similar to
for i = 1 : length(select_quaero_names)
    fprintf('%d: "%s" most similar to "%s"\n', i, select_quaero_names{i}, select_quaero_names{closest_song_indices(i)})
end

end

