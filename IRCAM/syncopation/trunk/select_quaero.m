function [ similarity_matrix ] = select_quaero()
%select_quaero Summary of this function goes here
% $Id$

global plotting;

% This is crap, it shouldn't be defined here, but because Matlab methods
% are not proper functions and require a base type, we have to.
plotting = {}; 
set_diag_plot('similarity_matrix')

select_quaero_names = make_quaero_dataset(100, 1, 'Quaero_Selection');

% Writes out new syncopation measures.
select_patterns = pattern_for_corpus(select_quaero_names, 'Quaero_Selection');

similarity_matrix = pattern_similarity(select_patterns);

closest_song_indices = closest_rhythms(similarity_matrix);

% Print most similar to
for i = 1 : length(select_quaero_names)
    fprintf('%d: "%s" most similar to "%s"\n', i, select_quaero_names{i}, select_quaero_names{closest_song_indices(i)})
end

end

