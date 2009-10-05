function [ new_pattern ] = read_pattern(filename, quaero_directory)
    %read_pattern Returns the rhythm pattern instance of the named file.
    
    rhythm_directory_root = tilde_expand('~/Research/Data/IRCAM-Beat/');
    pattern_filepath = [rhythm_directory_root quaero_directory '/Analysis/' filename '.pattern.xml'];

    pattern_document = xmlread(pattern_filepath);
    document_root = pattern_document.getDocumentElement();
    
    new_pattern = RhythmPattern(filename);
    
    syncopation_descriptions = pattern_document.getElementsByTagName('syncopation-description');
    if (syncopation_descriptions.length() ~= 0)
        syncopation_description = syncopation_descriptions.item(0);
        new_pattern.syncopation = read_syncopation(syncopation_description);
    else
        fprintf('Missing syncopation-description');
    end
    
    metrical_profiles = pattern_document.getElementsByTagName('metrical-description');
    if (metrical_profiles.length() ~= 0)
        metrical_profile = metrical_profiles.item(0);
        new_pattern.metrical_profile = read_metrical_profile(metrical_profile);
    else
        fprintf('Missing metrical-description format');
    end
    
    hypermetrical_profiles = pattern_document.getElementsByTagName('hypermetrical-description');
    if (hypermetrical_profiles.length() ~= 0)
        hypermetrical_profile = hypermetrical_profiles.item(0);
        new_pattern.hypermetrical_profile = read_metrical_profile(hypermetrical_profile);
    else
        fprintf('Missing hypermetrical-description format');
    end
    
    tempo_elements = pattern_document.getElementsByTagName('tempo');
    if (tempo_elements.length() ~= 0)
        first_tempo_element = tempo_elements.item(0);
        new_pattern.tempo = str2num(char(first_tempo_element.getAttribute('bpm')));
    else
        fprintf('Missing tempo format');
    end
end

