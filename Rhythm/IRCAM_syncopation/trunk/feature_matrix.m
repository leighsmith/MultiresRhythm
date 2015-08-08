function [ featureMatrix ] = feature_matrix( featureMethod, corpus_patterns )
%feature_matrix Returns the requested feature accessed by a pattern method (featureMethod) as
% a matrix of values. Columns are the feature elements, rows are each corpus example.
% $Id$

featureMatrix = cell2mat(cellfun(featureMethod, corpus_patterns, 'UniformOutput', 0));

end

