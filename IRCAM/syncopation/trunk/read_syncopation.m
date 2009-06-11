function [ syncopation_profile ] = read_syncopation( filename )
%read_syncopation Returns the syncopation profile of the named file.
% $Id$

rhythm_directory_root = tilde_expand('~/Research/Data/IRCAM-Beat/Quaero_Selection/');

syncopation_filepath = [rhythm_directory_root 'Analysis/' filename '.syncopation.xml'];

syncopation_document = xmlread(syncopation_filepath);
document_root = syncopation_document.getDocumentElement();
first_tagname = document_root.getTagName();

if (strcmp(first_tagname, 'syncopation-description'))
	tatum_count = document_root.getAttribute('tatum-count');
else
	fprintf('Unusual syncopation-description format %s discovered', first_tagname);
end

tatum_count = 16;
syncopation_profile = zeros(1, tatum_count);

tatums = document_root.getElementsByTagName('tatum');
% tatums.getLength()
    
for tatum_index = 1 : tatum_count
    %% (dom:get_attribute (dom:document_element marker_document) 'format')
    tatum_syncopation = tatums.item(tatum_index - 1);

    % getAttribute returns Java Strings, convert them to standard Matlab integers and floats.
    tatum_index_attribute = str2num(char(tatum_syncopation.getAttribute('id')));
    % TODO should use tatum_index_attribute
    syncopation_profile(1, tatum_index) = str2num(char(tatum_syncopation.getAttribute('syncopation')));
end

end

