classdef RhythmDescription
    % RhythmDescription Holds an rhythm, including onset detection function, beat times and metrical structure.
    % $Id$
    
    properties
        name = '';
        % onset detection functions, in grouped subbands per row.
        odfs = [];
        % TODO should have a mean and std for each odf subband computed for reuse.
        wideband_odf = [];
        beat_times = 0;
        meter = [];
        beats_per_measure = 4;
        sample_rate = 172.27;
        anacrusis = 0; % number of beats before the first downbeat.
        tempo = 0; % in median bpm
    end
    
    methods
        
    function new_rhythm = RhythmDescription(name, wideband_odf, odfs, beat_times, meter, beats_per_measure, anacrusis, sample_rate, tempo)
        new_rhythm.name = name;
        new_rhythm.odfs = odfs;
        new_rhythm.wideband_odf = wideband_odf;
        new_rhythm.beat_times = beat_times;
        new_rhythm.meter = meter;
        new_rhythm.beats_per_measure = beats_per_measure;
        new_rhythm.sample_rate = sample_rate;
        new_rhythm.anacrusis = anacrusis;
        new_rhythm.tempo = tempo;
    end

    function plot_rhythm(rhythm_description)
        beat_samples = round(rhythm_description.beat_times * rhythm_description.sample_rate);
        plot(1:length(rhythm_description.wideband_odf), rhythm_description.wideband_odf, '-', beat_samples, 1, '-+k');
        title(rhythm_description.name);
    end
    
    function [rhythm_name] = rhythm_name(rhythm_description)
        rhythm_name = rhythm_description.name;
    end
    
    end
end
