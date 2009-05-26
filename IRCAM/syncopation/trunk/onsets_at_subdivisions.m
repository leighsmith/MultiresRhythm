function [ onsets ] = onsets_at_subdivisions( beat_durations_in_measure, subdivisions_of_beat )
%onsets_at_subdivisions Converts from IOI's for main beats to onset times in samples for tatums
%   Detailed explanation goes here
beat_onset_sample = 0;
onsets = zeros(1, length(beat_durations_in_measure) * subdivisions_of_beat);
tatum_durations = beat_durations_in_measure / subdivisions_of_beat;

for beat_index = 0 : length(beat_durations_in_measure) - 1
    for subdiv_index = 1 : subdivisions_of_beat
	    onsets((beat_index * subdivisions_of_beat) + subdiv_index) = beat_onset_sample;
	    beat_onset_sample = beat_onset_sample + tatum_durations(beat_index + 1);
    end
end

end

