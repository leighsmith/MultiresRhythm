function [ output_args ] = downbeats_for_corpus( input_args )
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here

% average the profiles to see how they fair across a given homogenous set.
downbeat_corpus = make_dataset(ballroom('ComputedDownbeat/'), '.wav.markers.xml');

downbeat_chacha_patterns = pattern_for_corpus(downbeat_corpus, ballroom('Audio/'));

downbeat_profile = sum(feature_matrix(@reducedMetricalProfile, downbeat_chacha_patterns))

% The no downbeat version
no_downbeat_corpus = make_dataset(ballroom('NoDownbeat/'), '.wav.markers.xml');

no_downbeat_chacha_patterns = pattern_for_corpus(no_downbeat_corpus, ballroom('Audio/'));

no_downbeat_profile = sum(feature_matrix(@reducedMetricalProfile, no_downbeat_chacha_patterns))

figure();
subplot(2,1,1);
bar(downbeat_profile);
subplot(2,1,2);
bar(no_downbeat_profile);

end

