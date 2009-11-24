function [segments, segmentation_flux] = segment_odf(odf, odf_sr)
% segment_odf Segments the onset detection function into figural groups.
% Blurs the rhythm across a temporal window of a measure to capture the
% grouping segments.
% $Id$

preferred_beat_period = 0.600; % Preferred beat period (see Fraisse 1982, van Noorden & Moelants 1999).
preferred_measure = preferred_beat_period * 4 * odf_sr; % in samples.
segmentation_flux = gaussianRhythm(odf, preferred_measure);

segflux_minima = find_local_extrema(segmentation_flux, 'min');
threshold = mean(segmentation_flux) - std(segmentation_flux);
segment_boundaries = segflux_minima(segmentation_flux(segflux_minima) < threshold);

if(length(odf) - segment_boundaries(end) > 0)
    segment_boundaries = [segment_boundaries; length(odf)];
end

% TODO should merge consecutive segments which are too short.
% Find the shortest ones and merge them to the smallest adjacent segment.

% Pack into start and end columns.
segments = zeros(length(segment_boundaries) - 1, 2);
segments(:,1) = segment_boundaries(1:end-1);
segments(:,2) = segment_boundaries(2:end);

% For debugging
minima = zeros(size(segmentation_flux));
minima(segment_boundaries) = 1;
plot(1:length(odf), odf ./ max(odf), ...
     1:length(segmentation_flux), segmentation_flux ./ max(segmentation_flux), ...
     1:length(segmentation_flux), minima)

end
 