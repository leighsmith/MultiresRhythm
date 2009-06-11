function [ syncopation ] = syncopation_scalar ( syncopation_measures )
%syncopation_scalar Summary of this function goes here
% $Id$

dim = size(syncopation_measures);
rhythm_count = dim(1);

syncopation = zeros(rhythm_count, 1); % match the orientation of the syncopation measures.

for rhythm_index = 1 : rhythm_count
    syncopation(rhythm_index, 1) = norm(syncopation_measures(rhythm_index, :));
end

