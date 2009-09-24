function [ segment_locations, segment_transition_probs ] = match_rhythm_files( query_file, target_file )
%match_rhythm_files Returns the possible locations and probabilities of
%matching segments of the query against the target.
% $Id:$

query_rhythm_descr = read_analysed_rhythm(query_file, '~/Research/Data/IRCAM-Beat/QueryByTapping/Query/');
target_rhythm_descr = read_analysed_rhythm(target_file, '~/Research/Data/IRCAM-Beat/QueryByTapping/');
%% Returns the IOIs in milliseconds.
query_iois = load(tilde_expand(['~/Research/Data/IRCAM-Beat/QueryByTapping/onset/' query_file '.onset']));
% The first IOI is the distance between the first onset and the second, so
% we need to align the first beat. 
% TODO Hand coded for now. Zero if there is no leading silence.
% We need this set correctly to determine the correct ODF segments.
first_onset_time = 0.5315;
%first_onset_time = 0.0; % No difference in segmentation from leading silence.
% + 1 for the Fortran indexing of matlab.
segments = round(query_rhythm_descr.sample_rate .* segment_iois(query_iois ./ 1000, first_onset_time)) + 1;
number_of_segments = size(segments,1);
number_of_matches = 10;
segment_transition_probs = zeros(number_of_matches, number_of_segments);
segment_locations = zeros(number_of_matches, number_of_segments);
match_from = 0; % TODO should be 1?

for segment_index = 1 : number_of_segments
    query_segment = query_rhythm_descr.odf(segments(segment_index,1) : segments(segment_index,2));
    [peak_match_indices, peak_match_values] = cross_correlation_match(query_segment ./ max(query_segment), target_rhythm_descr.odf ./ max(target_rhythm_descr.odf), number_of_matches, match_from);
    fprintf('segment %d:%d matches (%s)\n', segments(segment_index,1), segments(segment_index,2), sprintf('%d ', peak_match_indices));
    segment_transition_probs(1:length(peak_match_values), segment_index) = peak_match_values ./ sum(peak_match_values);
    segment_locations(1:length(peak_match_values), segment_index) = peak_match_indices;
    % match_from = min(peak_match_indices) % No point looking before the earliest previous segment match.
    plot_correlation_match(query_segment, target_rhythm_descr.odf, peak_match_indices(1));
    % pause();
end

% figure()
% subplot(3,1,1)
% plot_correlation_match(query_rhythm_descr.odf, target_rhythm_descr.odf, peak_match_indices(1));
% subplot(3,1,2)
% plot_correlation_match(query_rhythm_descr.odf, target_rhythm_descr.odf, peak_match_indices(2));
% subplot(3,1,3)
% plot_correlation_match(query_rhythm_descr.odf, target_rhythm_descr.odf, peak_match_indices(3));

end

