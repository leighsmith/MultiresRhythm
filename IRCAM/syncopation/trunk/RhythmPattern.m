%RhythmPattern Holds a description of the rhythmic pattern defining a complete musical piece.
% (We may need more than one per piece for dramatically varying pieces). 
%
% $Id$       
%

% The pattern includes the metrical profile and the profile of syncopation
% per measure, and the hypermetrical profile over several measures.
% Factory method.
function new_pattern = RhythmPattern(name)
    if(nargin < 1)
        new_pattern.name = '';
    else
        new_pattern.name = name;
    % We use beats per measure as a pattern feature, rather than full meter
    % so that matching can still occur with some some compatible meters.
    new_pattern.beats_per_measure = 4; 
    new_pattern.syncopation = [];
    new_pattern.syncopation_tatums_per_beat = 4; % to 1/16th subdivisions.
    % Those tatums which are non-zero on at least one pattern in the corpus.
    new_pattern.principle_syncopation_tatums = []; 
    new_pattern.metrical_profile = [];
    new_pattern.metric_tatums_per_beat = 16;  % to 1/64th subdivisions.
    new_pattern.hypermetrical_profile = [];
    new_pattern.phrase_length = 4; % hypermetrical profile length in measures (bars)
    new_pattern.tempo = 0; % This makes the profiles tempo dependent.
    new_pattern.anacrusis = 0; % Phase of the beat the first downbeat starts on.
    new_pattern.periodicities = [];
    new_pattern.style = ''; % a rhythmic style classification.
end
    
function new_pattern = setSyncopation(pattern, syncopation)
    pattern.syncopation = syncopation;
    new_pattern = pattern;
end
        
function syncopation = syncopation_of(pattern)
    syncopation = pattern.syncopation;
end

function write_pattern( pattern, filename, pattern_filepath )
    %write_pattern Writes the syncopation and metrical profiles to XML file.

    dom = com.mathworks.xml.XMLUtils.createDocument('pattern-description');
    dom.createComment(sprintf('Pattern profile for %s', filename));
    pattern_description = dom.getDocumentElement();
    
    tempo_element = dom.createElement('tempo');
    tempo_element.setAttribute('bpm', sprintf('%f', pattern.tempo));
    pattern_description.appendChild(tempo_element);

    beats_element = dom.createElement('measure-duration');
    beats_element.setAttribute('beats', int2str(pattern.beats_per_measure));
    pattern_description.appendChild(beats_element);

    anacrusis_element = dom.createElement('anacrusis');
    anacrusis_element.setAttribute('beats', int2str(pattern.anacrusis));
    pattern_description.appendChild(anacrusis_element);
    
    for subbandIndex = 1 : size(pattern.syncopation, 1)
        syncopation_description = dom.createElement('syncopation-description');
        write_metrical_profile('syncopation', pattern.syncopation(subbandIndex,:), dom, syncopation_description, subbandIndex);
        pattern_description.appendChild(syncopation_description);
    end
    
    for subbandIndex = 1 : size(pattern.metrical_profile, 1)
        metrical_description = dom.createElement('metrical-description');
        write_metrical_profile('meter', pattern.metrical_profile(subbandIndex,:), dom, metrical_description, subbandIndex);
        pattern_description.appendChild(metrical_description);
    end
    
    hypermetrical_description = dom.createElement('hypermetrical-description');
    write_metrical_profile('meter', pattern.hypermetrical_profile, dom, hypermetrical_description, 1);
    pattern_description.appendChild(hypermetrical_description);

    xmlwrite(pattern_filepath, dom);
end

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

function plot_metrical_profile(pattern)
    % Display the reduced metrical pattern as a single bar graph.
    bar(reducedMetricalProfile(pattern));
    title(sprintf('Reduced metrical profile of %s', pattern.name));
end
    
function image_metrical_profile(pattern)    
    figure();
    imagesc(pattern.metrical_profile);
    title(sprintf('Subband metrical profile of %s', pattern.name));
end

function features = featureVector(pattern)
    % featureVector - Returns a single feature vector from the pattern.
    % A feature vector of the rhythm pattern is a concatenation of the
    % syncopation, metrical and hypermetrical profiles of each rhythm, with
    % the tactus beats of the syncopation removed since they will never
    % return a syncopation and only increase the dimensionality of the
    % distance measures.
    features = [syncopationProfile(pattern) metricalProfile(pattern) pattern.hypermetrical_profile pattern.tempo];
end

function beatLevelSync = beatLevelSyncopation(pattern)
%beatLevelSyncopation - Return the total amount of syncopation for each beat level.
    beatLevelSync = sum(reshape(pattern.syncopation', pattern.syncopation_tatums_per_beat, numel(pattern.syncopation) / pattern.syncopation_tatums_per_beat));
end

function tatums = mostSyncopatedTatum(pattern)
%mostSyncopatedTatum - Returns the tatum that has the highest syncopation for the pattern, for each subband range.
    [maxima, max_indices] = max(pattern.syncopation, [], 2);
    tatums = max_indices';
end

function features = syncPeriodicityFeatureVector(pattern)
% syncPeriodicityFeatureVector - Returns a feature vector from the pattern of syncopation and periodicities.
    features = [syncopationProfile(pattern) pattern.periodicities pattern.tempo];
end

function features = reducedFeatureVector(pattern)
% reducedFeatureVector - Returns a single feature vector from the pattern, reduced to
% semiquaver resolution. All feature values are guaranteed to be
% normalised if we don't return the raw tempo value.
    % features = [mostSyncopatedTatum(pattern) pattern.periodicities reducedMetricalProfile(pattern) pattern.hypermetrical_profile pattern.tempo];
    % features = [mostSyncopatedTatum(pattern) pattern.periodicities reducedMetricalProfile(pattern) pattern.hypermetrical_profile];
    features = [syncopationProfile(pattern) pattern.periodicities reducedMetricalProfile(pattern) pattern.hypermetrical_profile pattern.tempo];
end

% TODO the decimation blurs the profile too much, need to use a lower order
% filter.
% function features = reducedMetricalProfile(pattern)
% % reducedMetricalProfile - Returns a single metrical profile vector, reduced to
% % semiquaver resolution.
%     subbandCount = size(pattern.metrical_profile, 1);
%     decimationFactor = pattern.metric_tatums_per_beat / pattern.syncopation_tatums_per_beat;
%     decimatedMetricalProfile = zeros(subbandCount, size(pattern.metrical_profile, 2) / decimationFactor);
%     for subbandIndex = 1 : subbandCount
%         decimatedMetricalProfile(subbandIndex, :) = decimate(pattern.metrical_profile(subbandIndex, :), decimationFactor);
%     end
%     features = reshape(decimatedMetricalProfile', 1, numel(decimatedMetricalProfile));
% end

function downsampledMetricalProfile = reducedMetricalProfile(pattern)
% reducedMetricalProfile - Returns a single metrical profile vector, reduced to
% semiquaver resolution.
    downsampleFactor = pattern.metric_tatums_per_beat / pattern.syncopation_tatums_per_beat;    
    downsampledMetricalProfile = sum(reshape(pattern.metrical_profile', downsampleFactor, numel(pattern.metrical_profile) / downsampleFactor)) ./ downsampleFactor;
end

function singleMetricalProfile = widebandMetricalProfile(pattern)
% widebandMetricalProfile - Returns a single metrical profile vector, reduced to
% semiquaver resolution.
    numOfSpectralBands = size(pattern.metrical_profile, 1);
    downsampledMetricalProfile = reducedMetricalProfile(pattern);
    singleMetricalProfile = sum(reshape(downsampledMetricalProfile, numel(downsampledMetricalProfile) / numOfSpectralBands, numOfSpectralBands)') ./ numOfSpectralBands;
end


function beatLevelMetricalProfile = beatProfile(pattern)
% reducedMetricalProfile - Returns a single metrical profile vector, reduced to
% semiquaver resolution.
    numOfSpectralBands = size(pattern.metrical_profile, 1);
    tatumsPerBeat = reshape(sum(pattern.metrical_profile) ./ numOfSpectralBands, pattern.metric_tatums_per_beat, size(pattern.metrical_profile, 2) / pattern.metric_tatums_per_beat);
    beatLevelMetricalProfile = sum(tatumsPerBeat) ./ pattern.metric_tatums_per_beat;
end

function vector_length = featureVectorLength(pattern)
    vector_length = length(featureVector(pattern));
end

% TODO we should have the principle tatums assigned after finding only
% those tatums which are non-zero.
function principle_syncopation_tatums = syncopatedTatums(pattern)
% syncopatedTatums Returns the tatums that will be returned by syncopation
% profile. Not all tatums are returned since they will be zero at beats on
% the highest metrical levels.
   principle_syncopation_tatums = strip_beats(pattern.syncopation, pattern.syncopation_tatums_per_beat);
end

function features = syncopationProfile(pattern)
% syncopationProfile Returns the syncopation pattern, stripped of the beat locations which
% will always register 0 syncopation.
    strippedOfBeats = pattern.syncopation(:, syncopatedTatums(pattern));
    features = reshape(strippedOfBeats', 1, numel(strippedOfBeats));
end

function features = metricalProfile(pattern)
    features = reshape(pattern.metrical_profile', 1, numel(pattern.metrical_profile));
end

function features = singleMetricalProfile(pattern)
% singleMetricalProfile Just returns the first profile.
    features = pattern.metrical_profile(1,:);
end

function features = hypermetricalProfile(pattern)
    features = pattern.hypermetrical_profile;
end

function features = tempoMeasure(pattern)
    features = [pattern.tempo];
end
