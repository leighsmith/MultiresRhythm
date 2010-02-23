function [ all_acceptable_files, all_octave_errors ] = good_rwc_subset( analysis_dir )
%good_rwc_subset Generate a list of all RWC files which beat track
%correctly.
%   Works across the five datasets of RWC.

rwc_dirs = { 'AIST.RWC-MDB-C-2001.BEAT', 'AIST.RWC-MDB-G-2001.BEAT', 'AIST.RWC-MDB-J-2001.BEAT', 'AIST.RWC-MDB-P-2001.BEAT', 'AIST.RWC-MDB-R-2001.BEAT' };

[acceptable_files, octave_errors] = cellfun(@find_good_beat_analyses, rwc_dirs, 'UniformOutput', false);
all_acceptable_files = [];
all_octave_errors = [];

% Gee do I hate Matlab, where is (cons)?
for i = 1 : length(rwc_dirs)
   all_acceptable_files = cat(2, all_acceptable_files, acceptable_files{i});
   all_octave_errors = cat(2, all_octave_errors, octave_errors{i});
end

octave_error_filenames = cellfun(@basename, all_octave_errors, 'UniformOutput', false);

% Create a text file with the octave error names ("the ugly").
fod = fopen(tilde_expand('~/Research/Data/IRCAM-Beat/RWC/octave_errors.txt'), 'w');
for i = 1 : length(octave_error_filenames)
    fprintf(fod, '%s\n', octave_error_filenames{i});
end
fclose(fod);

good_analyses_directory = tilde_expand(['~/Research/Data/IRCAM-Beat/RWC/' analysis_dir]);
% Create the directory
mkdir(good_analyses_directory);
for i = 1 : length(all_acceptable_files)
    copyfile(all_acceptable_files{i}, good_analyses_directory);
    [filename, directory] = basename(all_acceptable_files{i});
    copyfile([directory, '/', filename, '.wav.bpm.xml'], good_analyses_directory);
end

end
