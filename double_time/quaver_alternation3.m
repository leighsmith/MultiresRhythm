function [ mean_quaver_alternation ] = quaver_alternation3( pattern )
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

tatums_per_beat = 16;
% For both treble and bass.
number_of_beats = numel(pattern.metrical_profile) / tatums_per_beat;
subbeats = reshape(reshape(pattern.metrical_profile', 1, numel(pattern.metrical_profile)), tatums_per_beat, number_of_beats)';
maximumSubbeat = max(max(subbeats));

mean_quaver_alternation = mean(std(subbeats')) ./ maximumSubbeat;

end

