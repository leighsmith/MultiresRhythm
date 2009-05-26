classdef RhythmDescription
    % RhythmDescription Holds an rhythm, including onset detection function, beat times and metrical structure
    properties
        name = '';
        odf = 0;
        beat_times = 0;
        meter = [];
        beats_per_measure = 4;
        sample_rate = 172.27;
    end
    methods
        function new_rhythm = RhythmDescription(name, odf, beat_times, meter, beats_per_measure, sample_rate)
            new_rhythm.name = name;
            new_rhythm.odf = odf;
            new_rhythm.beat_times = beat_times;
            new_rhythm.meter = meter;
            new_rhythm.beats_per_measure = beats_per_measure;
            new_rhythm.sample_rate = sample_rate;
        end
    end
end
