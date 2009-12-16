function [ upsampled_odf ] = normalise_by_beats( odf, beat_times, resample_rate )
%normalise_by_beats Normalises the time scale of the odf given a set of time markers.
%   This first version is super dumb, using a dead-reckoning interpolation,
%   with no higher order interpolation. We should probably be mapping onto
%   a manifold or effectively doing multi-rate upsampling.
% $Id:$

% resample_rate currently specified as number of samples between beats,
% needs to be respecified as Hz.
resample_period = 1 / resample_rate;

region_to_upsample = zeros((length(beat_times) - 1) * resample_period, 1);
% beat_times in samples (according to sample-rate of odf).
beat_intervals = diff(beat_times);

% interpolate each region to the next upsample region
for beat_index = 1 : length(beat_times) - 1
    resampled_region = beat_times(beat_index) : beat_intervals(beat_index) / resample_period : beat_times(beat_index + 1);
    region_to_upsample(((beat_index - 1) * resample_period : beat_index * resample_period) + 1) = resampled_region;
end

plot(region_to_upsample);

upsampled_odf = interp1(odf, region_to_upsample);
    % interp1q
    % interpft    


end

