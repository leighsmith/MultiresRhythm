function [ original_pattern, rhythm_description ] = filtered_pattern( beat_markers_filepath, sound_directory_root, pattern_directory, filter, rhythm_description )
%filtered_pattern Returns a rhythm pattern filtered using the supplied function.
%   Caches the pattern to the pattern_directory. Can reuse a single
%   pre-generated rhythm_description if supplied as a parameter. Returns
%   the rhythm description if it is generated.

    [filepath, filename] = fileparts(beat_markers_filepath);
    % fileparts differs from ircambeat's notion of extension, taking the last period as the
    % start of the extension, not the first.
    piece_name = strtok(filename, '.');
    pattern_filepath = [filepath '/' pattern_directory '/' piece_name '.pattern.xml'];
    fprintf('%s\n', pattern_filepath);
    audio_filepath = [sound_directory_root '/' piece_name '.wav'];

    try
        original_pattern = read_pattern(piece_name, pattern_filepath);
    catch no_pattern % no pattern found
        if(nargin < 5)
            fprintf('Reading rhythm description\n');
            rhythm_description = read_rhythm_description(beat_markers_filepath, audio_filepath, odfSpectralBands(4));
        end
        if(nargin < 4)
            filtered_rhythm_description = rhythm_description;
        else
            filtered_rhythm_description = filter(rhythm_description);
        end
        original_pattern = pattern_of_rhythm_description(filtered_rhythm_description);
        write_pattern(original_pattern, piece_name, pattern_filepath);
    end

end

