function plot_rhythm_description(rhythm_description, start_time, end_time)
% Plot the rhythm description ODF against the beat times for a region of the rhythm,
% specified in seconds.
% $Id:$
    
sr = rhythm_description.sample_rate;
if(nargin < 2)
    start_time = 0;
    end_time = length(rhythm_description.wideband_odf) / sr;
end
region = [start_time : 1 / rhythm_description.sample_rate : end_time];
beat_samples_to_plot = (rhythm_description.beat_times >= start_time) & (rhythm_description.beat_times <= end_time);
metrical_strength = (rhythm_description.beat_markers(beat_samples_to_plot) == 1) + 1;
% metrical_strength = 1;
odf_region = rhythm_description.wideband_odf(round(region .* sr) + 1);
plot(region, odf_region, '-', ...
     rhythm_description.beat_times(beat_samples_to_plot), metrical_strength, '-+k');
title(rhythm_description.name);
axis([start_time end_time 0 max(odf_region) + 1]);
xlabel('Time (seconds)');

end

