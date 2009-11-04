function [ similarity_matrix ] = fused_similarity( corpus_patterns )
%fuse_similarity Returns a fusion of similarity measures on each feature.
%   Detailed explanation goes here
    distance = 'euclidean';

    % Uses late fusion of similarity measures.
    metrical_similarity = pattern_similarity_of_feature(@metricalProfile, corpus_patterns);
    syncopation_similarity = pattern_similarity_of_feature(@syncopationProfile, corpus_patterns);
    hypermetrical_similarity = rotated_hypermetrical_distances(feature_matrix(@hypermetricalProfile, corpus_patterns));
    tempo_similarity = pattern_similarity_of_feature(@tempoMeasure, corpus_patterns);

    % TODO Super dumb.
    similarity_matrix = (normalise(metrical_similarity) + normalise(syncopation_similarity) + normalise(hypermetrical_similarity) + normalise(tempo_similarity)) ./ 4;

    if (diag_plot('similarity_comparisons'))
        figure();
        subplot(2,2,1);
        imagesc(metrical_similarity);
        title(sprintf('Metrical Dissimilarity of pattern of Quaero Selection by %s distance metric', distance))
        xlabel('Quaero Track');
        ylabel('Quaero Track');
        subplot(2,2,2);
        imagesc(syncopation_similarity);
        title(sprintf('Syncopation Dissimilarity of pattern of Quaero Selection by %s distance metric', distance))
        xlabel('Quaero Track');
        ylabel('Quaero Track');
        subplot(2,2,3);
        imagesc(hypermetrical_similarity);
        title(sprintf('Hypermetrical Dissimilarity of pattern of Quaero Selection by %s distance metric', 'euclidean'))
        xlabel('Quaero Track');
        ylabel('Quaero Track');
        subplot(2,2,4);
        imagesc(tempo_similarity);
        title(sprintf('Tempo Dissimilarity of pattern of Quaero Selection by %s distance metric', distance))
        xlabel('Quaero Track');
        ylabel('Quaero Track');
    end
end

