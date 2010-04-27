function [ segments ] = segment_iois(query_iois, first_onset_time)
%segment_iois Return a matrix of segments given the IOIs in seconds.
% The first IOI is the distance between the first onset and the second, so
% we need to align the first beat with first_onset_time. Zero if there is no leading silence.

% Onsets in seconds.
query_onsets = iois_to_onsets(query_iois) + first_onset_time;

% Overly simple segmenter using duration based regions. Determines the boundaries of segments.
% TODO impose minimum duration of segments and minimum number of events within the segment.
threshold = 0.5;
beat_period = median(query_iois);
one_stddev = std(query_iois);
% Mark the first interval as beginning a boundary.
onset_is_boundary = [1, query_iois > (beat_period + one_stddev * threshold)];
segment_boundaries = query_onsets(logical(onset_is_boundary));

% Pack into start and end columns.
% TODO, need to handle uneven number of segment boundaries.
segments = zeros(length(segment_boundaries) - 1, 2);
segments(:,1) = segment_boundaries(1:end-1);
segments(:,2) = segment_boundaries(2:end);

end

