function [ downbeat_times ] = downbeat_times( beat_markers_filepath )
%downbeats Return the downbeat times of the marker file.
%   If there are no downbeats marked, return as if the first beat is the downbeat.
% $Id$
%

% (TODO hardwired for now). 
beats_per_measure = 4;
% [all_beats_per_measure, metrical_hierarchy] = read_ircam_timesignatures(beat_markers_filepath);
% beats_per_measure = all_beats_per_measure(1); % TODO take the first beats-per-measure as the standard

% Set this to 1 to measure the baseline accuracy
% of the analysis against the annotated beat times.
% beats_per_measure = 1;

[beat_times, beat_markers] = read_beats(tilde_expand(beat_markers_filepath));

downbeat_times = beat_times(beat_markers == 1);
% Check we do have downbeats being distinguished.
if (~sum(beat_markers > 1))
    % If downbeats aren't distinguished, resort to defaulting to every
    % fourth beat .
    fprintf('No downbeats marked, default to every %d beats\n', beats_per_measure);
    downbeat_times = downbeat_times(1 : beats_per_measure : length(downbeat_times));
end    
    
end

