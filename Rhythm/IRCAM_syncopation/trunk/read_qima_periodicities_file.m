function [ rhythm_pattern ] = read_qima_periodicities_file( filepath )
% Read the given XML file to return an array of beat patterns
% $Id$

rhythmdescription_document = xmlread(filepath);
rhythmdescription_root = rhythmdescription_document.getDocumentElement();
patterns = rhythmdescription_root.getElementsByTagName('rhythmpattern');

rhythm_pattern = zeros(patterns.getLength(), 18);
for pattern_index = 0 : patterns.getLength() - 1
    pattern_node = patterns.item(pattern_index);
    pattern_text_node = pattern_node.getFirstChild();
    pattern_text = pattern_text_node.getNodeValue()
    rhythm_pattern = str2num(char(pattern_text));
    % fprintf('%s\n', sprintf('%f ', rhythm_pattern));
end

end

