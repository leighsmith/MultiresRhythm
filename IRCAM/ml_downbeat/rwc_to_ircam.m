function rwc_to_ircam( rwc_filename, ircam_filename )
%rwc_to_ircam Convert AIST RWC annotations to ircam beat files.
% $Id$
markers = load(tilde_expand(rwc_filename));

% RWC describes the timing as:
%
% "Each time step or section (temporal region) is represented, in a separate
% text file line, as a pair consisting of its absolute time (with temporal 
% resolution of 10 ms) and values/words."
% RWC needs the onset times adjusted by a minor amount
times = (markers(:,1) ./ 100) + 0.085;

divisor = min(markers(:,3));
% Downbeat marked by bit 3 (1000 binary) set.
beats = bitand(markers(:,3) ./ divisor, 7) + 1;

marker_diff = diff(beats);

% Check if there is a skip in the increment.
if(max(marker_diff) > 1)
    location_of_max_skip = find(marker_diff == max(marker_diff)) + 1;
    % location_of_min_skip = find(marker_diff == min(marker_diff)) + 1;
    fprintf('Skipped as not all beats annotated, difference of %d at beat %d\n', ...
        max(marker_diff), location_of_max_skip(1));
else
    if(max(beats) > 4)
        fprintf('unknown time_signature, maximum beat is %d, not skipped\n', max(beats));
    end
    time_signature = [max(beats), 4];
    write_ircam_markers(tilde_expand(ircam_filename), times, beats, time_signature);
end

end

