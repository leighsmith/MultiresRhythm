function [ marker_times ] = read_ircam_marker_times( beat_marker_filepath )
%read_ircam_marker_times New version 20090226A just uses the same beattype markers, as
%annotations, named segments.
% $Id$

marker_document = xmlread(beat_marker_filepath);
marker_root = marker_document.getDocumentElement();
first_tagname = marker_root.getTagName();
     
if (strcmp(first_tagname, 'beatdescription'))
	marker_times = read_ircambeat_markers_v1(beat_marker_filepath);
elseif (strcmp(first_tagname, 'musicdescription'))
	%% (dom:get_attribute (dom:document_element marker_document) 'format')
	[marker_times, marker_indexes] = read_ircam_annotation(beat_marker_filepath, 'segment');
else
	fprintf('Unusual ircambeat format %s discovered', first_tagname);
end

%% Remove the clap times that are negative (!)
% marker_times = clap_times_in_seconds(find(clap_times_in_seconds > 0.0))

end

