function [ file_to_write ] = audition_rwc_file( beat_filepath, description )
%audition_rwc_file Creates a sound file mixing the original sound with the
%beat markers.

filename = basename(beat_filepath);
original_wav = sprintf('~/Local_Data/RWC_WAV/%s.wav', filename);
file_to_write = sprintf('~/Local_Data/RWC_Auditions/%s_beats_%s.wav', filename, description);
sinburst = sinbursts(2000, 1, 0.050, 44100);
[beat_times] = read_beats(tilde_expand(beat_filepath));
save_rhythm_mix(file_to_write, original_wav, beat_times, sinburst);

end

