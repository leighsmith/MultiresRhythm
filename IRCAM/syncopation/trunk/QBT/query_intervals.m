% -*- Octave -*-
function [ normalised_times ] = query_intervals( filename )
%query_intervals Return the relative intervals of the query file.
%   Detailed explanation goes here

% These are actually inter-onset intervals measured in milliseconds.
iois = load(tilde_expand(['~/Research/Data/IRCAM-Beat/QueryByTapping/onset/' filename '.onset']));
% normalised_times = onsets ./ min(onsets);
% Using the median avoids very small IOIs from ornaments (grace-notes etc).
normalised_times = iois ./ median(iois);
figure()
bar(normalised_times);
title(filename);

end

