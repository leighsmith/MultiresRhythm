function [octave_error_count, octave_error_types] = octave_errors(scoresPerTrack)
%octave_errors From the precision, recall and f-score matrix, returns the
%number and type of octave error.
%   octave_error_types is 0 for correct beats, -1 for half the number
%   of beats than expected, +1 for double the number of beats expected.

    % dividing recall by precision indicates the number of correct against
    % the number selected.
    octave_error_types = round(log2(scoresPerTrack(:,2) ./ scoresPerTrack(:,1)));
    octave_error_count = sum(octave_error_types ~= 0);
end


