function [ match ] = match_score( query_segment, target_segment )
%match_score Count the number of onsets which match.

% Normalise and convolve with a Gaussian envelope to match the width of the
% target ODF pulses.
onset_equivalence = 60; % In samples, in seconds = 60 / 172.27 = 0.349 seconds.
norm_query_seg = normalise(gaussianRhythm(query_segment, onset_equivalence));
norm_target_seg = normalise(gaussianRhythm(target_segment, onset_equivalence));
% norm_query_seg = normalise(query_segment);
% norm_target_seg = normalise(target_segment);

% Count the number of peaks which differ between the two segments
% difference = norm_query_seg - norm_target_seg;
 
% queries_alone = (difference > 0) .* abs(difference);

% match = normalise((norm_target_seg - abs(norm_target_seg -
% norm_query_seg)) ./ );
% + max(norm_target_seg, norm_query_seg);

% match = [diff(normalise(cumsum(query_segment)) - normalise(cumsum(target_segment)), 2); 0; 0]])

% Absolute difference accounts for mismatch from the query or the target.
query_target_diff = abs(norm_query_seg - norm_target_seg);
% Queries which do not match the target will be negative.
queries_matchup = norm_target_seg - query_target_diff;
% Targets which do not match the query will be negative.
% targets_matchup = norm_query_seg - query_target_diff;
% Half wave rectify so we measure which are matches to queries alone.
only_queries_that_match = (queries_matchup > 0) .* queries_matchup;
% Ditto for targets matching to queries.
% only_targets_that_match = (targets_matchup > 0) .* targets_matchup;
% Calculate a normalised score comparing the queries that match against the original query.
single_query_match = sum(only_queries_that_match ./ ((norm_query_seg + eps) * length(norm_query_seg)));
% single_target_match = sum(only_targets_that_match ./ ((norm_target_seg + eps) * length(norm_target_seg)));

% plot([norm_query_seg, norm_target_seg, only_queries_that_match])
% legend('Query', 'Target', 'Queries matching Target');

% plot([norm_query_seg, norm_target_seg, only_targets_that_match])
% legend('Query', 'Target', 'Targets matching Query');

% Compute the harmonic mean of the single query and target matches.
% match = (2 * single_query_match * single_target_match) / (single_query_match + single_target_match + eps);

match = single_query_match;

end

