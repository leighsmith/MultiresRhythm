function rwc_to_ircam( rwc_filename, ircam_filename )
%rwc_to_ircam Convert AIST RWC annotations to ircam beat files.
% $Id$
markers = load(rwc_filename);

% RWC describes the timing as:
%
% "Each time step or section (temporal region) is represented, in a separate
% text file line, as a pair consisting of its absolute time (with temporal 
% resolution of 10 ms) and values/words."
% RWC needs the onset times adjusted by a minor amount
times = (markers(:,1) ./ 100) + 0.085;
beats = (markers(:,3) ./ 48) + 1;
beats(beats == 9) = 1;

if(max(diff(beats)) > 1)
    fprintf('not all beats annotated\n');
else
    if(max(beats) > 4)
        fprintf('unknown time_signature, maximum beat is %d\n', max(beats));
    end
    time_signature = [max(beats), 4];
    write_ircam_markers(ircam_filename, times, beats, time_signature);
end

end

