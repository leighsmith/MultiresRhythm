function [ similarity_matrix ] = pattern_similarity_of_feature( featureMethod, corpus_patterns, distance_metric )
%pattern_similarity_of_feature Returns a similarity matrix for the feature
%retrieved by featureMethod from the cell array of RhythmPattern instances.
%   Retrieve the feature vector (which may vary in length between features)
%   over the corpus patterns, coerce them into a matrix and apply the
%   similarity measure.

similarity_matrix = pattern_similarity(feature_matrix(featureMethod, corpus_patterns), distance_metric);

end
