function [ metrical_profile ] = read_metrical_profile (dom_element)
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

