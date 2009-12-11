classdef RhythmPattern
%RhythmPattern Holds a description of the rhythmic pattern defining a complete musical piece.
% (We may need more than one per piece for dramatically varying pieces). 
%
% $Id$       
%
% The pattern includes the metrical profile and the profile of syncopation
% per measure, and the hypermetrical profile over several measures.
properties
    name = '';
    % We use beats per measure as a pattern feature, rather than full meter
    % so that matching can still occur with some some compatible meters.
    beats_per_measure = 4; 
    syncopation = [];
    syncopation_tatums_per_beat = 4; % to 1/16th subdivisions.
    % Those tatums which are non-zero on at least one pattern in the corpus.
    principle_syncopation_tatums = []; 
    metrical_profile = [];
    metric_tatums_per_beat = 16;  % to 1/64th subdivisions.
    hypermetrical_profile = [];
    phrase_length = 4; % hypermetrical profile length in measures (bars)
    tempo = 0; % This makes the profiles tempo dependent.
end
    
methods

function new_pattern = RhythmPattern(name)
    % Factory method.
    new_pattern.name = name;
end

function new_pattern = setSyncopation(pattern, syncopation)
    pattern.syncopation = syncopation;
    new_pattern = pattern;
end
        
function syncopation = syncopation_of(rhythm)
    syncopation = rhythm.syncopation;
end

function write_pattern ( pattern, filename, pattern_filepath )
    %write_pattern Writes the syncopation and metrical profiles to XML file.

    dom = com.mathworks.xml.XMLUtils.createDocument('pattern-description');
    dom.createComment(sprintf('Pattern profile for %s', filename));
    pattern_description = dom.getDocumentElement();
    
    for subbandIndex = 1 : size(pattern.syncopation, 1)
        syncopation_description = dom.createElement('syncopation-description');
        write_metrical_profile('syncopation', pattern.syncopation(subbandIndex,:), dom, syncopation_description, subbandIndex);
        pattern_description.appendChild(syncopation_description);
    end
    
    for subbandIndex = 1 : size(pattern.metrical_profile, 1)
        metrical_description = dom.createElement('metrical-description');
        write_metrical_profile('meter', pattern.metrical_profile(subbandIndex,:), dom, metrical_description, subbandIndex);
        pattern_description.appendChild(metrical_description);
    end
    
    hypermetrical_description = dom.createElement('hypermetrical-description');
    write_metrical_profile('meter', pattern.hypermetrical_profile, dom, hypermetrical_description, 1);
    pattern_description.appendChild(hypermetrical_description);

    tempo_element = dom.createElement('tempo');
    tempo_element.setAttribute('bpm', sprintf('%f', pattern.tempo));
    pattern_description.appendChild(tempo_element);

    beats_element = dom.createElement('measure-duration');
    beats_element.setAttribute('beats', sprintf('%f', pattern.beats_per_measure));
    pattern_description.appendChild(beats_element);

    xmlwrite(pattern_filepath, dom);
end

function plot_pattern (pattern)
    % Display the rhythm pattern instance.
    figure();
    numOfProfiles = size(pattern.metrical_profile, 1);
    for subbandIndex = 1 : numOfProfiles
        subplot(numOfProfiles + 1, 1, subbandIndex);
        % Number of subdivisions per beat for metrical profile, and syncopation.
        profile_to_syncopation_ratio = pattern.metric_tatums_per_beat / pattern.syncopation_tatums_per_beat;
        profile_length = length(pattern.metrical_profile);
        syncopation_plot = zeros(1, profile_length);
        syncopation_plot(1 : profile_to_syncopation_ratio : profile_length) = pattern.syncopation(subbandIndex, :);
        beat_labels = ((0 : profile_length - 1) / profile_to_syncopation_ratio) + 1;
        h = bar(beat_labels, [pattern.metrical_profile(subbandIndex, :); syncopation_plot]');
        set(h(1), 'FaceColor','b');
        set(h(2), 'FaceColor','g');
        % title(sprintf('Metric profile'),'Interpreter','none');
        title(sprintf('Metric profile of %s', pattern.name), 'Interpreter', 'none');
        legend('Beat Occurrence', 'Syncopation Intensity');
        axis([63/64 17 0 1.0]);
        xlabel('Metric Location (semiquavers 1/16)');
        ylabel('Relative Amplitude');
    end
    
    subplot(numOfProfiles + 1, 1, numOfProfiles + 1);
    % TODO hardwired.
    bar(1: 1/16 : 5-1/16, pattern.hypermetrical_profile);
    axis([15/16 5 0 1.0]);
    ylabel('Relative Amplitude');
    xlabel('Phrase Location (Bars)');
    title(sprintf('Hyper-metric profile of %s', pattern.name),'Interpreter','none');
    % close();
end

function features = featureVector(pattern)
    % featureVector - Returns a single feature vector from the pattern.
    % A feature vector of the rhythm pattern is a concatenation of the
    % syncopation, metrical and hypermetrical profiles of each rhythm, with
    % the tactus beats of the syncopation removed since they will never
    % return a syncopation and only increase the dimensionality of the
    % distance measures.
    features = [syncopationProfile(pattern) metricalProfile(pattern) pattern.hypermetrical_profile pattern.tempo];
end

function vector_length = featureVectorLength(pattern)
    vector_length = length(featureVector(pattern));
end

function features = syncopationProfile(pattern)
    % Returns the syncopation pattern, stripped of the beat locations which
    % will also register 0 syncopation.

    % TODO we should have the principle tatums assigned after finding only
    % those tatums which are non-zero.
    principle_syncopation_tatums = strip_beats(pattern.syncopation, pattern.syncopation_tatums_per_beat);
    strippedOfBeats = pattern.syncopation(:, principle_syncopation_tatums);
    features = reshape(strippedOfBeats, 1, numel(strippedOfBeats));
end

function features = metricalProfile(pattern)
    features = reshape(pattern.metrical_profile', 1, numel(pattern.metrical_profile));
end

% Just returns the first profile.
function features = singleMetricalProfile(pattern)
    features = pattern.metrical_profile(1,:);
end

function features = hypermetricalProfile(pattern)
    features = pattern.hypermetrical_profile;
end

function features = tempoMeasure(pattern)
    features = [pattern.tempo];
end

end % methods

end % classdef

