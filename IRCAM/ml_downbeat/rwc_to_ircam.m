function rwc_to_ircam( rwc_filename, ircam_filename )
%rwc_to_ircam Convert AIST RWC annotations to ircam beat files.
% $Id$
markers = load(rwc_filename);

times = (markers(:,1) ./ 100) + 0.085;
beats = (markers(:,3) ./ 48) + 1;
beats(beats == 9) = 1;
if(max(beats) == 4)
    time_signature = [4 4];
    write_ircam_markers(ircam_filename, times, beats, time_signature);
else
    fprintf('unknown time_signature, maximum beat is %d\n', max(beats));
end

end

