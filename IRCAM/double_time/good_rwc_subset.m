function [ good_beat_tracking, all_octave_errors ] = good_rwc_subset( analysis_dir )
%good_rwc_subset Generate a list of all RWC files which beat track
%correctly.
%   Works across the annotations datasets of RWC.

root_dir = '~/Research/Data/IRCAM-Beat/RWC/';

annotated_files = make_dataset(tilde_expand([root_dir 'Annotation/']), '.beat.xml', 0);
% For tempo based assessments:
[good_beat_tracking, all_octave_errors] = prune_bad_tempo(tilde_expand([root_dir 'Analysis']), annotated_files);
% For beat tracking based assessments:
% [good_beat_tracking, all_octave_errors] = prune_bad_beat_tracking(tilde_expand([root_dir 'Analysis']), annotated_files);
% [good_beat_tracking, all_octave_errors] = prune_bad_beat_tracking_new([root_dir 'Analysis'], annotated_files);

% Create a text file with the octave error names ("the ugly").
write_filenames(tilde_expand([root_dir 'generated_octave_errors.txt']), all_octave_errors);

% Create a text file with the good beat tracking names ("the good").
write_filenames(tilde_expand([root_dir 'good_beat_tracking.txt']), good_beat_tracking);

if (nargin > 1)
    good_analyses_directory = tilde_expand([root_dir analysis_dir]);
 
    % Create the directory
    mkdir(good_analyses_directory);
    for i = 1 : length(good_beat_tracking)
         copyfile(good_beat_tracking{i}, good_analyses_directory);
         [filename, directory] = basename(good_beat_tracking{i});
         copyfile([directory, '/', filename, '.wav.bpm.xml'], good_analyses_directory);
    end
end

end
