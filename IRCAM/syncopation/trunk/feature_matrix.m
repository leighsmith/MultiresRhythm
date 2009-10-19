function [ featureMatrix ] = feature_matrix( featureMethod, corpus_patterns )
%feature_matrix Returns the request feature accessed by a pattern method as
% a matrix of values.

featureMatrix = cell2mat(cellfun(featureMethod, corpus_patterns, 'UniformOutput', 0));

end

