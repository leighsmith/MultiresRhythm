function match_against_targets( )
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

corpus_directory = '~/Research/Data/IRCAM-Beat/QueryByTapping';

fprintf('Reading targets\n');
target_descriptions = all_target_descriptions(corpus_directory);

for target_index = 1 : length(target_descriptions)
    target_rhythm_descr = target_descriptions{target_index};
    query_fragment = target_rhythm_descr.wideband_odf(8000:8000+2574);
    % [ match_locations, segments, single_match_measure ] = match_rhythm_odf(query_fragment, target_rhythm_descr.wideband_odf, target_rhythm_descr.sample_rate);
    [ match_locations, segments, single_match_measure ] = match_no_segmentation(query_fragment, target_rhythm_descr.wideband_odf, target_rhythm_descr.sample_rate);
    match_locations
    segments
    single_match_measure
    
    figure()
    plot_correlation_match(query_fragment', target_rhythm_descr.wideband_odf', match_locations, segments);

end


end


