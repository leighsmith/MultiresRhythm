function plot_pattern(pattern)
    % Display the rhythm pattern instance.
    figure();
    numOfProfiles = size(pattern.metrical_profile, 1);
    for subbandIndex = 1 : numOfProfiles
        subplot(numOfProfiles + 1, 1, subbandIndex);
        % Number of subdivisions per beat for metrical profile, and syncopation.
        profile_to_syncopation_ratio = pattern.metric_tatums_per_beat / pattern.syncopation_tatums_per_beat;
        profile_length = length(pattern.metrical_profile);
        syncopation_plot = zeros(1, profile_length);
        syncopation_plot(1 : profile_to_syncopation_ratio : profile_length) = pattern.syncopation(subbandIndex, :);
        beat_labels = ((0 : profile_length - 1) / profile_to_syncopation_ratio) + 1;
        h = bar(beat_labels, [pattern.metrical_profile(subbandIndex, :); syncopation_plot]');
        set(h(1), 'FaceColor','b');
        set(h(2), 'FaceColor','g');
        % title(sprintf('Metric profile'),'Interpreter','none');
        title(sprintf('Metric profile of %s', pattern.name), 'Interpreter', 'none');
        legend('Beat Occurrence', 'Syncopation Intensity');
        axis([63/64 17 0 1.0]);
        xlabel('Metric Location (semiquavers 1/16)');
        ylabel('Relative Amplitude');
    end
    
    subplot(numOfProfiles + 1, 1, numOfProfiles + 1);
    % TODO hardwired.
    bar(1: 1/16 : 5-1/16, pattern.hypermetrical_profile);
    axis([15/16 5 0 1.0]);
    ylabel('Relative Amplitude');
    xlabel('Phrase Location (Bars)');
    title(sprintf('Hyper-metric profile of %s', pattern.name),'Interpreter','none');
    % close();
end

