% -*- Octave *-*
function [ pruned ] = prune_outliers( a, lower_limit, upper_limit )
%prune_outliers Remove any elements which are above or below the limit
%value.
%
% $Id: prune_outliers.m 993 2009-07-10 15:42:43Z lsmith $
%
% Copyright (c) 2009 IRCAM, All Rights Reserved.
% Permission is only granted to use this code for Quaero evaluation purposes.

pruned = a(a >= lower_limit & a <= upper_limit);

end

