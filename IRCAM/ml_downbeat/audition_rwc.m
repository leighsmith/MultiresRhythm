function audition_rwc( filenum, annotation )
%audition_rwc Creates a 
%   Detailed explanation goes here
if annotation
    beat_filepath = sprintf('~/Research/Data/IRCAM-Beat/RWC/Annotation/AIST.RWC-MDB-P-2001.BEAT/RM-P%03d.beat.xml', filenum);
    comment = 'annotated'; 
else
    beat_filepath = sprintf('~/Research/Data/IRCAM-Beat/RWC/Analysis/RM-P%03d.wav.markers.xml', filenum);
    comment = 'tracked'; 
end
original_wav = sprintf('~/Research/Data/IRCAM-Beat/RWC/Audio/Popular_Music/WAV/RM-P%03d.wav', filenum);
file_to_write = sprintf('~/Local_Data/RM-P%03d_beats_%s.wav', filenum, comment);
sinburst = sinbursts(2000, 1, 0.050, 44100);
[beat_times] = read_beats(tilde_expand(beat_filepath));
save_rhythm_mix(file_to_write, original_wav, beat_times, sinburst);

end

