function process_ballroom ()
%process_ballroom Computes the rhythmic patterns of the Ballroom dancers
%dataset and writes an ARFF file.
% $Id:$
    corpus_dataset = make_dataset(ballroom('Analysis'), '.wav.markers.xml');
    corpus_patterns = pattern_for_corpus(corpus_dataset, ballroom('Audio'));
    style_map = read_rhythm_styles(ballroom('dance_classification.txt'));
    patterns_with_styles = assign_styles(corpus_patterns, style_map);
    write_corpus_as_arff(patterns_with_styles, ballroom('full_test.arff'));
end

% sim = rhythm_similarity_of_corpus('Ballroom Dancers', ballroom('Analysis'), '.wav.markers.xml', ballroom('Audio'));

