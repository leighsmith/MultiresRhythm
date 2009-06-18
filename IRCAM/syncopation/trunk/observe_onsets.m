function [ onset_observations, silence_observations ] = observe_onsets (analysed_rhythm)
%observe_onsets Determines which tatums correspond to onsets or rests.
% $Id$

rhythm = analysed_rhythm.odf;
% TODO fixed subdivisions_of_beat to 16ths.
silence_observations = observe_evidence_of(analysed_rhythm, 4, @silence_evidence);
dim = size(silence_observations);
onset_observations = metric_grid_from_probabilities(silence_observations);

if (diag_plot('onset_observations'))
    figure();
    colormap([50/255, 94/255, 174/255; 255/255, 254/255, 87/255]);
    imagesc(onset_observations);
    title(sprintf('Onset observations of %s', analysed_rhythm.name),'Interpreter','none');
    xlabel('Time (measures)');
    ylabel('Tatum location (beat)');
    % 	   :aspect_ratio 0.666)
    % close();
end
    
if (diag_plot('silence_observations'))
     figure();
     imagesc(silence_observations);
     title(sprintf('Silence observations of %s', analysed_rhythm.name),'Interpreter','none');
     xlabel('Time (measures)')
     ylabel('Tatum location (beat)')
     % :aspect_ratio 0.666)
     % close();
end

end

