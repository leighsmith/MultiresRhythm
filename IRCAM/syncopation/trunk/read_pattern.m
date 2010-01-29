function [ new_pattern ] = read_pattern(filename, pattern_filepath)
%read_pattern Returns the rhythm pattern instance of the named file.
% $Id$

    pattern_document = xmlread(pattern_filepath);
    % document_root = pattern_document.getDocumentElement();
    
    new_pattern = RhythmPattern(filename);
    
    syncopation_descriptions = pattern_document.getElementsByTagName('syncopation-description');
    numOfSyncopations = syncopation_descriptions.getLength();
    if (numOfSyncopations ~= 0)
        for descriptionIndex = 1 : numOfSyncopations
            syncopation_description = syncopation_descriptions.item(descriptionIndex - 1);
            new_pattern.syncopation(descriptionIndex,:) = read_syncopation(syncopation_description);
        end
    else
        fprintf('Missing syncopation-description');
    end
    
    metrical_profiles = pattern_document.getElementsByTagName('metrical-description');
    numOfMetricalProfiles = metrical_profiles.getLength();
    if (numOfMetricalProfiles ~= 0)
        for descriptionIndex = 1 : numOfMetricalProfiles
            metrical_profile = metrical_profiles.item(descriptionIndex - 1);
            new_pattern.metrical_profile(descriptionIndex,:) = read_metrical_profile(metrical_profile);
        end
    else
        fprintf('Missing metrical-description format');
    end
    
    hypermetrical_profiles = pattern_document.getElementsByTagName('hypermetrical-description');
    if (hypermetrical_profiles.length() ~= 0)
        for descriptionIndex = 1 : metrical_profiles.length()
            hypermetrical_profile = hypermetrical_profiles.item(0);
            new_pattern.hypermetrical_profile = read_metrical_profile(hypermetrical_profile);
        end
    else
        fprintf('Missing hypermetrical-description format');
    end
    
    tempo_elements = pattern_document.getElementsByTagName('tempo');
    if (tempo_elements.length() ~= 0)
        first_tempo_element = tempo_elements.item(0);
        new_pattern.tempo = str2double(char(first_tempo_element.getAttribute('bpm')));
    else
        fprintf('Missing tempo format');
    end
    
    anacrusis_elements = pattern_document.getElementsByTagName('anacrusis');
    if (anacrusis_elements.length() ~= 0)
        first_anacrusis_element = anacrusis_elements.item(0);
        new_pattern.anacrusis = str2double(char(first_anacrusis_element.getAttribute('beats')));
    else
        fprintf('Missing anacrusis format');
    end

    measure_elements = pattern_document.getElementsByTagName('measure-duration');
    if (measure_elements.length() ~= 0)
        first_measure_element = measure_elements.item(0);
        new_pattern.beats_per_measure = str2double(char(first_measure_element.getAttribute('beats')));
    else
        fprintf('Missing measure format');
    end


end

