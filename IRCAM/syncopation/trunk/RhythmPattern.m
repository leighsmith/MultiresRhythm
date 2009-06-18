classdef RhythmPattern
%RhythmPattern Holds a description of the rhythmic pattern defining a
%complete musical piece. (We may need more than one per piece for dramatically
%varying pieces).
% $Id$       

properties
    syncopation = [];
    metrical_profile = [];
    name = '';
end
    
methods

function new_pattern = RhythmPattern(name, syncopation, metrical_profile)
    new_pattern.syncopation = syncopation;
    new_pattern.metrical_profile = metrical_profile;
    new_pattern.name = name;
end
        
function setSyncopation(pattern, syncopation)
    pattern.syncopation = syncopation;
end
        
function syncopation = syncopation_of(rhythm)
    syncopation = rhythm.syncopation;
end

function [ syncopation ] = read_syncopation (pattern, dom_element)
    tatum_count = str2num(char(dom_element.getAttribute('tatum-count')));
    syncopation = zeros(1, tatum_count);

    tatums = dom_element.getElementsByTagName('tatum');
    for tatum_index = 1 : tatums.getLength()
        tatum_syncopation = tatums.item(tatum_index - 1);

        % getAttribute returns Java Strings, convert them to standard Matlab integers and floats.
        tatum_index_attribute = str2num(char(tatum_syncopation.getAttribute('id')));
        % TODO should use tatum_index_attribute instead of tatum_index
        syncopation(1, tatum_index) = str2num(char(tatum_syncopation.getAttribute('syncopation')));
    end
end

function [ metrical_profile ] = read_metrical_profile (pattern, dom_element)
    % tatum_count = 16;
    tatum_count = str2num(char(dom_element.getAttribute('tatum-count')));
    metrical_profile = zeros(1, tatum_count);

    tatums = dom_element.getElementsByTagName('tatum');
    
    for tatum_index = 1 : tatums.getLength()
        tatum_occurrence = tatums.item(tatum_index - 1);

        % getAttribute returns Java Strings, convert them to standard Matlab integers and floats.
        tatum_index_attribute = str2num(char(tatum_occurrence.getAttribute('id')));
        % TODO should use tatum_index_attribute instead of tatum_index
        metrical_profile(1, tatum_index) = str2num(char(tatum_occurrence.getAttribute('meter')));
    end
end

function [ new_pattern ] = read_pattern(pattern, filename)
    %read_pattern Returns the rhythm pattern instance of the named file.
    
    rhythm_directory_root = tilde_expand('~/Research/Data/IRCAM-Beat/Quaero_Selection/');
    pattern_filepath = [rhythm_directory_root 'Analysis/' filename '.pattern.xml'];

    pattern_document = xmlread(pattern_filepath);
    document_root = pattern_document.getDocumentElement();
    
    syncopation_descriptions = pattern_document.getElementsByTagName('syncopation-description');
    if (syncopation_descriptions.length() ~= 0)
        syncopation_description = syncopation_descriptions.item(0);
        pattern.syncopation = read_syncopation(pattern, syncopation_description);
    else
        fprintf('Missing syncopation-description');
    end
    
    metrical_profiles = pattern_document.getElementsByTagName('metrical-description');
    if (metrical_profiles.length() ~= 0)
        metrical_profile = metrical_profiles.item(0);
        pattern.metrical_profile = read_metrical_profile(pattern, metrical_profile);
    else
        fprintf('Missing metrical-description format');
    end
    pattern.name = filename;
    new_pattern = pattern; % return what we were given.
end

function [ success ] = write_metrical_profile ( pattern, dom, metrical_description )
    %write_metrical_profile Writes the metrical_profile values to the metrical_description DOM element.

    metrical_description.setAttribute('tatum-count', sprintf('%d', length(pattern.metrical_profile)));

    for tatum_index = 1 : length(pattern.metrical_profile)
        tatum_meter = pattern.metrical_profile(tatum_index);

        tatum_element = dom.createElement('tatum');
        tatum_element.setAttribute('id', sprintf('%d', tatum_index - 1));
        tatum_element.setAttribute('meter', sprintf('%f', tatum_meter));

        metrical_description.appendChild(tatum_element);
    end

end

function [ success ] = write_syncopation ( pattern, dom, syncopation_description )
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

function [ success ] = write_pattern ( pattern, filename )
    %write_syncopation Writes the syncopation values to XML file.

    rhythm_directory_root = tilde_expand('~/Research/Data/IRCAM-Beat/Quaero_Selection/');

    syncopation_filepath = [rhythm_directory_root 'Analysis/' filename '.pattern.xml'];

    dom = com.mathworks.xml.XMLUtils.createDocument('pattern-description');
    dom.createComment(sprintf('Pattern profile for %s', filename));
    pattern_description = dom.getDocumentElement();
    
    syncopation_description = dom.createElement('syncopation-description');
    write_syncopation(pattern, dom, syncopation_description);
    pattern_description.appendChild(syncopation_description);
    
    metrical_description = dom.createElement('metrical-description');
    write_metrical_profile(pattern, dom, metrical_description);
    pattern_description.appendChild(metrical_description);
    
    xmlwrite(syncopation_filepath, dom);
end

function plot_pattern (pattern)
    % Display the rhythm pattern instance.
    figure();
    bar([pattern.metrical_profile; pattern.syncopation]');
    % title(sprintf('Metric profile'),'Interpreter','none');
    title(sprintf('Metric profile of %s', pattern.name),'Interpreter','none');
    legend('Beat Occurrence', 'Syncopation Intensity');
    xlabel('Metric Location');
    ylabel('Relative Occurrence in Piece');
    % close();
end

end % methods

end % classdef

