function [ success ] = write_syncopation ( filename, syncopation_profile )
%write_syncopation Writes the syncopation values to XML file.
%   Detailed explanation goes here

rhythm_directory_root = tilde_expand('~/Research/Data/IRCAM-Beat/Quaero_Selection/');

syncopation_filepath = [rhythm_directory_root 'Analysis/' filename '.syncopation.xml'];

syncopation_dom = com.mathworks.xml.XMLUtils.createDocument('syncopation-description');
syncopation_description = syncopation_dom.getDocumentElement();
syncopation_description.setAttribute('tatum-count', sprintf('%d', length(syncopation_profile)));
syncopation_dom.createComment(sprintf('Syncopation profile for %s', filename));

for tatum_index = 1 : length(syncopation_profile)
    tatum_syncopation = syncopation_profile(tatum_index);

    tatum_element = syncopation_dom.createElement('tatum');
    tatum_element.setAttribute('id', sprintf('%d', tatum_index - 1));
    tatum_element.setAttribute('syncopation', sprintf('%f', tatum_syncopation));

    syncopation_description.appendChild(tatum_element);
end

xmlwrite(syncopation_filepath, syncopation_dom);

end

