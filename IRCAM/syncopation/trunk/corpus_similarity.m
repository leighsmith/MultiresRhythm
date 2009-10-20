function [ similarity_matrix ] = corpus_similarity(corpus_name)
%corpus_similarity Returns the similarity matrix of the corpus named by corpus_name.
% for example: corpus_similarity('Quaero_Selection')
% $Id$

global plotting;

% This is crap, it shouldn't be defined here, but because Matlab methods
% are not proper functions and require a base type, we have to.
plotting = {}; 
set_diag_plot('similarity_matrix')
set_diag_plot('similarity_comparisons')
late_fusion = 1;

corpus_dataset = make_quaero_dataset(corpus_name);

corpus_patterns = pattern_for_corpus(corpus_dataset, corpus_name);

if (late_fusion == 1)
    similarity_matrix = fused_similarity(corpus_patterns);
else
    similarity_matrix = pattern_similarity_of_feature(@featureVector, corpus_patterns);
end

if (diag_plot('similarity_matrix'))
    figure();
    imagesc(similarity_matrix)
    title(sprintf('Dissimilarity of pattern of Quaero Selection by %s distance metric', 'cityblock'))
    xlabel('Quaero Track');
    ylabel('Quaero Track');
end

closest_song_indices = closest_rhythms(similarity_matrix);

% Print most similar to
for i = 1 : length(corpus_dataset)
    fprintf('%d: (%.3f) "%s" most similar to "%s"\n', ...
            i, similarity_matrix(i, closest_song_indices(i)), corpus_dataset{i}, corpus_dataset{closest_song_indices(i)})
end

end

