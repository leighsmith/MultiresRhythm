function [ averaged_tatums ] = average_tatums( silence_observations, decimate_by_tatums )
%average_tatums Downsample the tatums by local averaging.
%   Assumes the silence_observations are arranged with rows as tatums, columns as measures.
%   decimate_by_tatums indicates the number of tatums to reduce.
%   decimate_by_tatums = 4 for 1/16ths for output, if 1/64ths for input.
% $Id$

tatums_per_measure = size(silence_observations, 1);
num_of_measures = size(silence_observations, 2);
tatums_vector = reshape(silence_observations, decimate_by_tatums, numel(silence_observations) / decimate_by_tatums);
averaged_tatums = reshape(sum(tatums_vector) ./ decimate_by_tatums, tatums_per_measure / decimate_by_tatums, num_of_measures) ;

end
