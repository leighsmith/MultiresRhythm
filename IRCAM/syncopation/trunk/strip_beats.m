function [ beats_removed ] = strip_beats( rhythms, tatums_per_beat )
%strip_beats Removes those tatums which are on the beat.
% Tatums which are on the beat will therefore never constitute a syncopation. 
% The reason to strip beats after creating a syncopation measure
% incorporating them is to retain a full set of tatum values for
% comparisons & future applications.
% $Id$

tatum_count = size(rhythms, 2);
tatums_to_remove = 1 : tatums_per_beat : tatum_count;
keep = ones(1, tatum_count);
keep(tatums_to_remove) = 0;
tatums_to_keep = find(keep);
beats_removed = rhythms(:, tatums_to_keep);

end