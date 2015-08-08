function [ all_features ] = all_feature_matrix( corpus_patterns )
%all_feature_matrix Convenience function to return the matrix of entire
%features for all patterns.
% $Id:$

all_features = feature_matrix(@featureVector, corpus_patterns);

end

