function process_ballroom () 
    corpus_dataset = make_dataset(ballroom('Analysis/'), '.wav.markers.xml');
    corpus_patterns = pattern_for_corpus(corpus_dataset, ballroom('Audio'));
    write_corpus_as_arff(corpus_patterns, ballroom('test.arff'));
end

% sim = rhythm_similarity_of_corpus('Ballroom Dancers', ballroom('Analysis/'), '.wav.markers.xml', ballroom('Audio/'));

