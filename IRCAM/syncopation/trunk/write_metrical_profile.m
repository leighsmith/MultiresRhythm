function write_metrical_profile ( profile, dom, metrical_description )
%write_metrical_profile Writes the metrical_profile values to the metrical_description DOM element.

    metrical_description.setAttribute('tatum-count', sprintf('%d', length(profile)));

    for tatum_index = 1 : length(profile)
        tatum_meter = profile(tatum_index);

        tatum_element = dom.createElement('tatum');
        tatum_element.setAttribute('id', sprintf('%d', tatum_index - 1));
        tatum_element.setAttribute('meter', sprintf('%f', tatum_meter));

        metrical_description.appendChild(tatum_element);
    end

end

