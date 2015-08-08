function write_ircam_markers( marker_filepath, times, beat_markers, time_signature )
%write_ircam_markers Writes the beat marker times and position with measure
%to an IRCAM QIMA XML format file.
% $Id$

    dom = com.mathworks.xml.XMLUtils.createDocument('musicdescription');

    music_description = dom.getDocumentElement();

    music_description.setAttribute('xmlns', 'http://www.quaero.org/Music_6/1.0');
    music_description.setAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');
    music_description.setAttribute('format', '20090701A');
    music_description.setAttribute('xsi:schemaLocation', 'http://www.quaero.org/Music_6/1.0 http://www.ircam.fr/anasyn/quaero/quaero_ctc_6_music.xsd');
    music_description.setAttribute('date', datestr(now, 29));
     
    media_element = dom.createElement('media');
    path_text = dom.createTextNode(marker_filepath);
    media_element.appendChild(path_text);
    music_description.appendChild(media_element);

    generator_identifier = '1';
    generator_description = dom.createElement('descriptiondefinition');
    generator_description.setAttribute('id', generator_identifier);
    type_description = dom.createElement('type');
    type_description.appendChild(dom.createTextNode('beat'));
    generator_description.appendChild(type_description);
    generator_element = dom.createElement('generator');
    generator_element.setAttribute('name', 'LMS write_ircam_markers');
    generator_element.setAttribute('version', '1.0.0');
    generator_element.setAttribute('date', '2010-01-28');
    generator_description.appendChild(generator_element);
    
    music_description.appendChild(generator_description);

    segment_element = create_segment(dom, 0, times(1), '', 0);
    audiocontent_element = dom.createElement('audiocontent');
    audiocontent_element.setAttribute('id', generator_identifier);
    audiocontent_element.setAttribute('type', 'silence');
    segment_element.appendChild(audiocontent_element);
    music_description.appendChild(segment_element);
    
    segment_element = create_segment(dom, times(1), 0, sprintf('timesignature-%d/%d', time_signature(1), time_signature(2)), 0);
    timesignature_element = dom.createElement('timesignature');
    timesignature_element.setAttribute('id', generator_identifier);
    timesignature_element.setAttribute('beatspermeasure', int2str(time_signature(1)));
    timesignature_element.setAttribute('beatduration', int2str(time_signature(2)));
    segment_element.appendChild(timesignature_element);
    music_description.appendChild(segment_element);

    for markerIndex = 1 : size(times, 1)

        segment_element = create_segment(dom, times(markerIndex), 0, '', 0);
        beat_type_element = dom.createElement('beattype');
        beat_type_element.setAttribute('id', generator_identifier);
        beat_type_element.setAttribute('pattern', '0');
        if(beat_markers(markerIndex) == 1)
            measure = 1;
        else
            measure = 0;
        end
        beat_type_element.setAttribute('measure', int2str(measure));
        beat_type_element.setAttribute('beat', int2str(beat_markers(markerIndex)));
        beat_type_element.setAttribute('tatum', '1');
        segment_element.appendChild(beat_type_element);
        music_description.appendChild(segment_element);

    end
    
    xmlwrite(marker_filepath, dom);
end

function [segment_element] = create_segment ( dom, time, length, comment, sourcetrack )
%create_segment Writes the metrical_profile values to the metrical_description DOM element.

    segment_element = dom.createElement('segment');
    segment_element.setAttribute('time', sprintf('%f', time));
    segment_element.setAttribute('length', sprintf('%f', length));
    if(size(comment, 2) ~= 0)
        segment_element.setAttribute('comment', comment);
    end
    segment_element.setAttribute('sourcetrack', int2str(sourcetrack));
    
end
