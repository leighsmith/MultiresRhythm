function generate_datasets()
%generate_datasets Just holds all the absolute file paths.
%   Detailed explanation goes here

generate_annotated_dataset('~/Research/Data/IRCAM-Beat/RWC/Annotation/AIST.RWC-MDB-P-2001.BEAT/', ...
    '~/Research/Data/IRCAM-Beat/RWC/Audio/Popular_Music/WAV');

generate_analysed_dataset('~/Research/Data/IRCAM-Beat/Quaero_Selection/Analysis',...
    '~/Research/Data/IRCAM-Beat/Quaero_Selection/Annotation', ...
    '~/Research/Data/IRCAM-Beat/Quaero_Selection/Audio')

generate_analysed_dataset('~/Research/Data/IRCAM-Beat/RWC/Analysis',...
    '~/Research/Data/IRCAM-Beat/RWC/Annotation/AIST.RWC-MDB-P-2001.BEAT/', ...
    '~/Research/Data/IRCAM-Beat/RWC/Audio/Popular_Music/WAV')

end

