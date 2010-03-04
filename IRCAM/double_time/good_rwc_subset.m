function [ good_beat_tracking, all_octave_errors ] = good_rwc_subset( analysis_dir )
%good_rwc_subset Generate a list of all RWC files which beat track
%correctly.
%   Works across the five datasets of RWC.

dir_root = '~/Research/Data/IRCAM-Beat/RWC/';

annotated_files = make_dataset(tilde_expand([dir_root 'Annotation/']), '.beat.xml', 0);
% [good_beat_tracking, all_octave_errors] = prune_bad_beat_tracking(tilde_expand([dir_root '/Analysis']), annotated_files);
[good_beat_tracking, all_octave_errors] = prune_bad_beat_tracking_new('Analysis', annotated_files);

% Create a text file with the octave error names ("the ugly").
fod = fopen(tilde_expand([dir_root 'generated_octave_errors.txt']), 'w');
for i = 1 : length(all_octave_errors)
    fprintf(fod, '%s\n', basename(all_octave_errors{i}));
end
fclose(fod);

good_analyses_directory = tilde_expand([dir_root analysis_dir]);

% Create the directory
mkdir(good_analyses_directory);
for i = 1 : length(good_beat_tracking)
    copyfile(good_beat_tracking{i}, good_analyses_directory);
    [filename, directory] = basename(good_beat_tracking{i});
    copyfile([directory, '/', filename, '.wav.bpm.xml'], good_analyses_directory);
end

end
