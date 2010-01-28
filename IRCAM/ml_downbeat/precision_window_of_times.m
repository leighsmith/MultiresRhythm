% -*- Octave *-*
function [ precision_windows ] = precision_window_of_times( annotation_times, relative_precision_window )
%precision_window_of_times Retrieve the precision window in seconds from annotated beat times and relative
%  precision window as a proportion of the beat period
%
% $Id: precision_window_of_times.m 989 2009-07-10 15:20:17Z lsmith $
%
% Copyright (c) 2009 IRCAM, All Rights Reserved.
% Permission is only granted to use this code for Quaero evaluation purposes.

%% Add an extra beat period at the start to match the number of annotations.
annotated_beat_periods = [diff(annotation_times(1:2)); diff(annotation_times)];
precision_windows = annotated_beat_periods .* relative_precision_window;

%%fprintf('annotated beat periods %f\nabsolute precision windows %f\n',
%%	    annotated_beat_periods(1 : 6), precision_windows(1 : 6))

end

