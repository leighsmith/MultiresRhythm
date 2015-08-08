function [ metrical_profile ] = representative_metrical_profile( silence_observations )
%classify_measures Summary of this function goes here
%   Detailed explanation goes here

num_of_clusters = 4; % "k" means.
idx = kmeans(silence_observations', num_of_clusters);
most_frequent = mode(idx);
representative_measures = idx == most_frequent;
representative_silence_observations = silence_observations(:, representative_measures);
num_of_representative_measures = size(silence_observations, 2);
metrical_profile = 1 - (sum(representative_silence_observations, 2)' ./ num_of_representative_measures);

subplot(3, 1, 1);
imagesc(silence_observations)
subplot(3, 1, 2);
bar(idx');
axis([1 length(idx) 1 num_of_clusters]);
subplot(3, 1, 3);
bar(representative_measures)
axis([1 length(representative_measures) 0 1]);

end

