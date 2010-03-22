function [ good_beat_tracking, all_octave_errors ] = find_good_beat_analyses( root_dir, test_tempo, analysis_dir )
%find_good_beat_analyses Generate a list of all RWC files which beat track correctly.
%   Works across the annotations datasets of RWC.
% [good_beat_tracking, octave_errors] = find_good_beat_analyses('~/Research/Data/IRCAM-Beat/RWC/', false, 'AllGoodBeatAnalyses');

annotated_files = make_dataset(tilde_expand([root_dir 'Annotation/']), '.beat.xml', 0);
% For tempo based assessments:
if (test_tempo)
    [good_beat_tracking, all_octave_errors] = prune_bad_tempo(tilde_expand([root_dir 'Analysis']), annotated_files);
else
    % For beat tracking based assessments:
    [good_beat_tracking, all_octave_errors] = prune_bad_beat_tracking(tilde_expand([root_dir 'Analysis']), annotated_files);
    % [good_beat_tracking, all_octave_errors] = prune_bad_beat_tracking_new([root_dir 'Analysis'], annotated_files);
end

% Create a text file with the octave error names ("the ugly").
write_filenames(tilde_expand([root_dir 'generated_octave_errors.txt']), all_octave_errors);

% Create a text file with the good beat tracking names ("the good").
write_filenames(tilde_expand([root_dir 'good_beat_tracking.txt']), good_beat_tracking);

if (nargin > 2)
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
