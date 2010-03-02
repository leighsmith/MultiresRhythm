function [ output_args ] = reestimate_tempo( filenames, reestimation_directory )
%reestimate_tempo Run ircambeat with the prior tempo halved for the given files.

root_dir = '~/Research/Data/IRCAM-Beat/RWC/';
    
for filename_index = 1 : length(filenames)
    filename = filenames{filename_index};

    beat_markers_filepath = [root_dir 'Analysis/' filename '.wav.markers.xml'];

    % Read the tempo of the file
    [beat_times] = read_beats(beat_markers_filepath);

    % calculate from beat times. Perhaps one day read it from bpm_filepath XML file.
    tempo = 60 / median(diff(beat_times)); 

    new_markers_filepath = [root_dir reestimation_directory '/' filename '.wav.markers.xml'];
    new_bpm_filepath = [root_dir reestimation_directory '/' filename '.wav.bpm.xml'];

    sound_filepath = ['~/Local_Data/RWC_WAV/' filename '.wav'];

    ircambeat_command = sprintf('~/ApplicationBuilding/IrcamBeatQuaero.build/bin/ircambeat -i %s --ref_tempo %d -o %s -r %s', ...
        sound_filepath, round(tempo / 2), new_markers_filepath, new_bpm_filepath);

    fprintf('%s original tempo %.3f BPM, reestimating with %.3f BPM\n', filename, tempo, round(tempo / 2));
    fprintf('%s\n', ircambeat_command);
    system(ircambeat_command);
end

end
