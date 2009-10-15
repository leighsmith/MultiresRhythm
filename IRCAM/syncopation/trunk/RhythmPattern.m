classdef RhythmPattern
%RhythmPattern Holds a description of the rhythmic pattern defining a complete musical piece.
% (We may need more than one per piece for dramatically varying pieces). 
%
% $Id$       
%
% The pattern includes the metrical profile and the profile of syncopation
% per measure, and the hypermetrical profile over several measures.
properties
    syncopation = [];
    metrical_profile = [];
    hypermetrical_profile = [];
    tempo = 0; % This makes the profiles tempo dependent.
    name = '';
    metric_tatums_per_beat = 16;
    syncopation_tatums_per_beat = 4;
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

function write_syncopation ( pattern, dom, syncopation_description )
    %write_syncopation Writes the syncopation values to the syncopation_description DOM element.

    syncopation_description.setAttribute('tatum-count', sprintf('%d', length(pattern.syncopation)));

    for tatum_index = 1 : length(pattern.syncopation)
        tatum_syncopation = pattern.syncopation(tatum_index);

        tatum_element = dom.createElement('tatum');
        tatum_element.setAttribute('id', sprintf('%d', tatum_index - 1));
        tatum_element.setAttribute('syncopation', sprintf('%f', tatum_syncopation));

        syncopation_description.appendChild(tatum_element);
    end

end

function write_pattern ( pattern, filename )
    %write_pattern Writes the syncopation and metrical profiles to XML file.

    rhythm_directory_root = tilde_expand('~/Research/Data/IRCAM-Beat/Quaero_Selection/');

    syncopation_filepath = [rhythm_directory_root 'Analysis/' filename '.pattern.xml'];

    dom = com.mathworks.xml.XMLUtils.createDocument('pattern-description');
    dom.createComment(sprintf('Pattern profile for %s', filename));
    pattern_description = dom.getDocumentElement();
    
    syncopation_description = dom.createElement('syncopation-description');
    write_syncopation(pattern, dom, syncopation_description);
    pattern_description.appendChild(syncopation_description);
    
    metrical_description = dom.createElement('metrical-description');
    write_metrical_profile(pattern.metrical_profile, dom, metrical_description);
    pattern_description.appendChild(metrical_description);
    
    hypermetrical_description = dom.createElement('hypermetrical-description');
    write_metrical_profile(pattern.hypermetrical_profile, dom, hypermetrical_description);
    pattern_description.appendChild(hypermetrical_description);

    tempo_element = dom.createElement('tempo');
    tempo_element.setAttribute('bpm', sprintf('%f', pattern.tempo));
    pattern_description.appendChild(tempo_element);
    
    xmlwrite(syncopation_filepath, dom);
end

function plot_pattern (pattern)
    % Display the rhythm pattern instance.
    figure();
    subplot(2,1,1);
    % Number of subdivisions per beat for metrical profile, and syncopation.
    profile_to_syncopation_ratio = pattern.metric_tatums_per_beat / pattern.syncopation_tatums_per_beat;
    profile_length = length(pattern.metrical_profile);
    syncopation_plot = zeros(1, profile_length);
    syncopation_plot(1 : profile_to_syncopation_ratio : profile_length) = pattern.syncopation;
    beat_labels = ((0 : profile_length - 1) / profile_to_syncopation_ratio) + 1;
    bar(beat_labels, [pattern.metrical_profile; syncopation_plot]');
    % title(sprintf('Metric profile'),'Interpreter','none');
    title(sprintf('Metric profile of %s', pattern.name),'Interpreter','none');
    legend('Beat Occurrence', 'Syncopation Intensity');
    axis([63/64 17 0 1.0]);
    xlabel('Metric Location (semiquavers 1/16)');
    ylabel('Relative Amplitude');
    subplot(2,1,2);
    % TODO hardwired.
    bar([1: 1/16 : 5-1/16], pattern.hypermetrical_profile);
    axis([15/16 5 0 1.0]);
    ylabel('Relative Amplitude');
    xlabel('Phrase Location (Bars)');
    title(sprintf('Hyper-metric profile of %s', pattern.name),'Interpreter','none');
    % close();
end

function features = featureVector(pattern)
    % featureVector - Returns a single feature vector from the pattern.
    % A feature vector of the rhythm pattern is a concatenation of the
    % syncopation metrical and hypermetrical profiles of each rhythm, with
    % the tactus beats of the syncopation removed since they will never
    % return a syncopation and only increase the dimensionality of the
    % distance measures.
    features = [strip_beats(pattern.syncopation, pattern.syncopation_tatums_per_beat) pattern.metrical_profile pattern.hypermetrical_profile pattern.tempo];
    % features = [pattern.metrical_profile];
    % features = [pattern.metrical_profile pattern.tempo];
end    
    
function length = featureVectorLength(pattern)
    % TODO hardwired to 16 tatums, 64 hemidemisemiquavers, 64 hyper meter, 1 tempo.
    beats_per_measure = 4;
    
    length = ((beats_per_measure - 1) * pattern.syncopation_tatums_per_beat) + (pattern.metric_tatums_per_beat * beats_per_measure) + 64 + 1;
    % length = 16;
    % length = 17; % length(pattern.metrical_profile) + 1
end

end % methods

end % classdef

