function [ onset_observations ] = observe_onsets (analysed_rhythm)
%observe_onsets Determines which tatums correspond to onsets or rests.

rhythm = analysed_rhythm.odf;
% fixed subdivisions_of_beat to 16ths.
silence_observations = observe_downbeat_of(analysed_rhythm, 4, @silence_evidence);
dim = size(silence_observations);
onset_observations = metric_grid_from_probabilities(silence_observations);

figure();
colormap([50/255, 94/255, 174/255; 255/255, 254/255, 87/255]);
image(onset_observations);
%axes1 = axes('Layer','top','YDir','normal');
%image(onset_observations,'Parent',axes1);
title(sprintf('Onset observations of %s', analysed_rhythm.name));
% 	   :xlabel 'Time (measures)'
% 	   :ylabel 'Tatum location (beat)'
% 	   :aspect_ratio 0.666)
% close();

%     figure();
% image(silence_observations);
% 	   :title (format nil '~a observations of ~a' 'Silence' (name rhythm))
% 	   :xlabel 'Time (measures)'
% 	   :ylabel 'Tatum location (beat)'
% 	   :aspect_ratio 0.666)
%     close();

end

