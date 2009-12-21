function [ similarity_matrix, corpus_patterns ] = rhythm_similarity_of_corpus(corpus_name)
%rhythm_similarity_of_corpus Returns the similarity matrix of the corpus named by corpus_name.
% for example: corpus_similarity('Quaero_Selection');
% $Id$

global plotting;

% Having to set the plotting descriptions here is crap, it shouldn't be defined here, but because Matlab methods
% are not proper functions and require a base type, we have to.
plotting = {}; 
set_diag_plot('similarity_matrix')
set_diag_plot('similarity_comparisons')
similarity = 'early_fusion';

% The directories we work on.
rhythm_directory_root = tilde_expand(['~/Research/Data/IRCAM-Beat/' corpus_name '/']);
annotations_directory = [rhythm_directory_root 'Annotation/'];
sound_directory_root = '/Volumes/Quaerodb/music/Annotation Corpus - 1000 C/WAV stereo 44k/';
beat_file_extension = '.beat.xml';

corpus_dataset = make_quaero_dataset(annotations_directory, beat_file_extension);
corpus_patterns = pattern_for_corpus(corpus_dataset, sound_directory_root);
best_similarity = zeros(1, length(corpus_patterns));

switch (similarity)
    case 'late_fusion'
        distance = 'euclidean';
        similarity_matrix = fused_similarity(corpus_patterns, distance);
    case 'syncopation_alone'
        distance = 'euclidean';
        similarity_matrix = pattern_similarity_of_feature(@syncopationProfile, corpus_patterns, distance);
    case 'early_fusion'
        distance = 'cosine';
        similarity_matrix = pattern_similarity_of_feature(@featureVector, corpus_patterns, distance);
    otherwise
        fprintf('Unknown similarity method\n');
end

if (diag_plot('similarity_matrix'))
    figure();
    imagesc(similarity_matrix)
    title(sprintf('Dissimilarity of pattern of Quaero Selection by %s distance metric', distance))
    xlabel('Quaero Track');
    ylabel('Quaero Track');
end

closest_song_indices = closest_rhythms(similarity_matrix);

% Print most similar to
for i = 1 : length(corpus_dataset)
    fprintf('%3d: (%.4f) "%s" most similar to "%s"\n', ...
            i, similarity_matrix(i, closest_song_indices(i)), filename(corpus_dataset{i}), filename(corpus_dataset{closest_song_indices(i)}));
    best_similarity(i) = similarity_matrix(i, closest_song_indices(i));
end

[top_matches, matching_indices] = sort(best_similarity, 'ascend');

% Since the match will be symmetrical, we need double the number of matches
% to catch the five best.
fprintf('Top 10 matches\n');
for j = 1 : 10
    i = matching_indices(j);
    fprintf('%3d: (%.4f) "%s" most similar to "%s"\n', ...
            i, similarity_matrix(i, closest_song_indices(i)), filename(corpus_dataset{i}), filename(corpus_dataset{closest_song_indices(i)}));
end

end

% fileparts differs from ircambeat's notion of extension, taking the last period as the
% start of the extension, not the first.
function [filename] = filename(filepath)
    [directory, filename_and_extension] = fileparts(filepath);
    filename = strtok(filename_and_extension, '.');
end
