function [ syncopation ] = read_syncopation (dom_element)
    tatumCount = str2num(char(dom_element.getAttribute('tatum-count')));
    syncopation = zeros(1, tatumCount);

    tatums = dom_element.getElementsByTagName('tatum');
    for tatum_index = 1 : tatums.getLength()
        tatum_syncopation = tatums.item(tatum_index - 1);

        % getAttribute returns Java Strings, convert them to standard Matlab integers and floats.
        tatum_index_attribute = str2num(char(tatum_syncopation.getAttribute('id')));
        % TODO should use tatum_index_attribute instead of tatum_index
        syncopation(1, tatum_index) = str2num(char(tatum_syncopation.getAttribute('syncopation')));
    end
end


