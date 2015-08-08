function [ onsets ] = iois_to_onsets( iois )
%iois_to_onsets Summary of this function goes here
%   Detailed explanation goes here

onsets = zeros(1, length(iois) + 1);

current_onset = 0;

for ioi_index = 1 : length(iois)
    onsets(ioi_index) = current_onset;
    current_onset = current_onset + iois(ioi_index);
end

onsets(end) = current_onset;
end

