function write_metrical_profile ( profile_title, profile, dom, metrical_description, subband)
%write_metrical_profile Writes the metrical_profile values to the metrical_description DOM element.
% $Id$

    metrical_description.setAttribute('subband', int2str(subband));
    metrical_description.setAttribute('tatum-count', int2str(length(profile)));
    
    for tatum_index = 1 : length(profile)
        tatum_meter = profile(tatum_index);

        tatum_element = dom.createElement('tatum');
        tatum_element.setAttribute('id', int2str(tatum_index - 1));
        tatum_element.setAttribute(profile_title, sprintf('%f', tatum_meter));

        metrical_description.appendChild(tatum_element);
    end

end

