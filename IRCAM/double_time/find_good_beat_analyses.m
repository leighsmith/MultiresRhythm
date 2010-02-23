function [ good_beat_tracking, octave_errors ] = find_good_beat_analyses( annotation_dir )
%good_beat_analyses Summary of this function goes here
%   find_good_beat_analyses('AIST.RWC-MDB-P-2001.BEAT')

dir_root = '~/Research/Data/IRCAM-Beat/RWC';

annotated_files = make_dataset(tilde_expand([dir_root '/Annotation/' annotation_dir]), '.beat.xml', 0);
[good_beat_tracking, octave_errors] = prune_bad_beat_tracking(tilde_expand([dir_root '/Analysis']), annotated_files);

end

