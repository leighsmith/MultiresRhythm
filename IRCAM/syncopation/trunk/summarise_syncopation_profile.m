function [ syncopation_profile ] = summarise_syncopation_profile( corpus_name, corpus_patterns )
%summarise_syncopation_profile Indicates which syncopation measures are most
%consistently present.
%   Detailed explanation goes here

pattern_syncopations = feature_matrix(@syncopationProfile, corpus_patterns);
syncopation_profile = sum(pattern_syncopations, 1);
figure(); bar(syncopation_profile)
title(sprintf('Syncopation profile of corpus %s', corpus_name));
end

