classdef RhythmPattern
%RhythmPattern Holds a description of the rhythmic pattern defining a
%complete musical piece. (We may need more than one per piece for dramatically
%varying pieces). 

% $Id$       

% The pattern includes the metrical profile and the profile of syncopation
% per measure, and the hypermetrical profile over several measures.
properties
    syncopation = [];
    metrical_profile = [];
    hypermetrical_profile = [];
    name = '';
end
    
methods

function new_pattern = RhythmPattern(name)
    % Factory method.
    new_pattern.name = name;
end

function setSyncopation(pattern, syncopation)
    pattern.syncopation = syncopation;
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

    xmlwrite(syncopation_filepath, dom);
end

function plot_pattern (pattern)
    % Display the rhythm pattern instance.
    figure();
    subplot(2,1,1);
    bar([pattern.metrical_profile; pattern.syncopation]');
    % title(sprintf('Metric profile'),'Interpreter','none');
    title(sprintf('Metric profile of %s', pattern.name),'Interpreter','none');
    legend('Beat Occurrence', 'Syncopation Intensity');
    xlabel('Metric Location');
    ylabel('Relative Occurrence in Piece');
    subplot(2,1,2);
    bar(pattern.hypermetrical_profile);
    ylabel('Relative Occurrence in Piece');
    xlabel('Tatum Location');
    % close();
end

function features = featureVector(pattern)
    % featureVector - Returns a single feature vector from the pattern.
    % A feature vector of the rhythm pattern is a concatenation of the
    % syncopation metrical and hypermetrical profiles of each rhythm, with
    % the tactus beats of the syncopation removed since they will never
    % return a syncopation and only increase the dimensionality of the distance measures.
    % TODO Hardwired at 4 tatums per beat.
    features = [strip_beats(pattern.syncopation, 4) pattern.metrical_profile pattern.hypermetrical_profile];
end    
    
function length = featureVectorLength(pattern)
    % TODO hardwired to 16 tatums.
    length = 12 + 16 + 64;
end

end % methods

end % classdef

