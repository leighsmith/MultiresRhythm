function [ onset_observations, silence_observations ] = observe_onsets (analysed_rhythm)
%observe_onsets Determines which tatums correspond to onsets or rests.
% $Id$

% TODO fixed subdivisions_of_beat to 16ths (dividing crotchet by 4).
silence_observations = observe_evidence_of(analysed_rhythm, 4, @silence_evidence);

% number of rows = number of tatums in measure.
% grid_length = size(silence_observations, 1);
threshold = 0.5; % Arbitrary & therefore probably wrong.
onset_observations = silence_observations < threshold;


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

