function [ normalised_times ] = target_intervals ( filename )
%target_intervals Return the relative intervals of the target file.
%   Detailed explanation goes here

root_dir = '~/Research/Data/IRCAM-Beat/QueryByTapping/REFMIDI/';
onsets_pathname = tilde_expand([root_dir filename '.onsets']);
midi_pathname = tilde_expand([root_dir filename '.mid']);

if(exist(onsets_pathname, 'file') == 0)
    system(['/usr/local/bin/convertscore -s -c  -o /tmp/' filename '.score ' midi_pathname]);
    system(['sed -n -e "/t /s/t \([0-9.]*\);/\1/p" /tmp/' filename '.score > ' onsets_pathname]);
end

% These truly are onsets and must be converted to IOIs.
onsets = load(onsets_pathname);
iois = diff(onsets);
% Using the median avoids very small IOIs from ornaments (grace-notes etc).
normalised_times = iois ./ median(iois);
if(diag_plot('target_intervals'))
    figure()
    bar(normalised_times)
    title(filename);
end

end

