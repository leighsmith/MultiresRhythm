classdef RhythmDescription
    % RhythmDescription Holds an rhythm, including onset detection function, beat times and metrical structure.
    % $Id$
    
    properties
        name = '';
        % onset detection functions, in grouped subbands per row.
        odfs = [];
        % TODO should have a mean and std for each odf subband computed for reuse.
        wideband_odf = [];
        beat_times = [];
        beat_markers = [];
        meter = [];
        beats_per_measure = 4;
        sample_rate = 172.27;
        anacrusis = 0; % number of beats before the first downbeat.
        tempo = 0; % in median bpm
    end
    
    methods
        
    function new_rhythm = RhythmDescription(name, wideband_odf, odfs, beat_times, beat_markers, meter, beats_per_measure, anacrusis, sample_rate, tempo)
        new_rhythm.name = name;
        new_rhythm.odfs = odfs;
        new_rhythm.wideband_odf = wideband_odf;
        new_rhythm.beat_times = beat_times;
        new_rhythm.beat_markers = beat_markers;
        new_rhythm.meter = meter;
        new_rhythm.beats_per_measure = beats_per_measure;
        new_rhythm.sample_rate = sample_rate;
        new_rhythm.anacrusis = anacrusis;
        new_rhythm.tempo = tempo;
    end

    function plot_rhythm(rhythm_description, start_time, end_time)
        sr = rhythm_description.sample_rate;
        if(nargin < 2)
            start_time = 0;
            end_time = length(rhythm_description.wideband_odf / sr);
        end
        region = [start_time : 1 / rhythm_description.sample_rate : end_time];
        beat_samples_to_plot = (rhythm_description.beat_times >= min(region)) & (rhythm_description.beat_times <= max(region));
        metrical_strength = (rhythm_description.beat_markers(beat_samples_to_plot) == 1) + 1;
        % metrical_strength = 1;
        plot(region, rhythm_description.wideband_odf(round(region .* sr)), '-', rhythm_description.beat_times(beat_samples_to_plot), metrical_strength, '-+k');
        title(rhythm_description.name);
        axis([start_time end_time 0 12]);
        xlabel('Time (seconds)');
    end
    
    function [rhythm_name] = rhythm_name(rhythm_description)
        rhythm_name = rhythm_description.name;
    end
    
    function [trimmed_odf, trimmed_odf_subbands] = beat_synchronised_odf(rhythm_description)
    % Returns the ODF trimmed of any anacrusis or leading silence, so that
    % sample 1 of the returned ODF is at the first downbeat. Returns this
    % but doesn't modify the original ODF so the beat times and ODF stay in
    % sync.
        % Trim the ODF according to downbeat locations.
        start_from = rhythm_description.beat_times(rhythm_description.anacrusis + 1);
        start_sample = round(rhythm_description.sample_rate * start_from) + 1;
        trimmed_odf = rhythm_description.wideband_odf(start_sample : end);
        % Create an matrix of different subbands.
        trimmed_odf_subbands = rhythm_description.odfs(:, start_sample : end);
        fprintf('Starting downbeat finding from %.3f seconds, %.3f samples\n', start_from, start_sample);
    end
    
    end
end
