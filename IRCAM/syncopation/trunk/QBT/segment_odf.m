function [segments, segmentation_flux] = segment_odf(odf, odf_sr)
% segment_odf Segments the onset detection function into figural groups.
% Blurs the rhythm across a temporal window of a measure to capture the
% grouping segments.
% $Id$

% The most preferred (i.e typical) beat interval (see Fraisse 1982, van Noorden & Moelants 1999).
preferred_beat_period = 0.600;

% The number of standard deviations below the mean Gaussian convolved flux
% level to be considered a segment boundary.
segmentation_likelihood = 1;

% The maximum number of beats a single phrase will last.
segmentation_duration = 8; % Two measures.

preferred_measure = preferred_beat_period * segmentation_duration * odf_sr; % in samples.
segmentation_flux = gaussianRhythm(odf, preferred_measure);

segflux_minima = find_local_extrema(segmentation_flux, 'min');
flux_threshold = mean(segmentation_flux) - segmentation_likelihood * std(segmentation_flux);
segment_boundaries = segflux_minima(segmentation_flux(segflux_minima) < flux_threshold);
% First segment starts at the start of the onset detection function.
segment_boundaries = [1; segment_boundaries];

if(length(odf) - segment_boundaries(end) > 0)
    segment_boundaries = [segment_boundaries; length(odf)];
end

% merge consecutive segments which are too short.
% TODO Find the shortest ones and merge them to the smallest adjacent segment.
temporal_threshold = preferred_beat_period * 2.0 * odf_sr;
above_minimum = [diff(segment_boundaries) > temporal_threshold; true];
segment_boundaries = segment_boundaries(above_minimum);

% Pack into start and end columns.
segments = zeros(length(segment_boundaries) - 1, 2);
segments(:,1) = segment_boundaries(1:end-1);
segments(:,2) = segment_boundaries(2:end);

% For debugging
% minima = zeros(size(segmentation_flux));
% minima(segment_boundaries) = 1;
% threshold = (zeros(length(segmentation_flux), 1) + flux_threshold) ./ max(segmentation_flux); 
% plot([odf ./ max(odf), segmentation_flux ./ max(segmentation_flux), minima, threshold])

end
 