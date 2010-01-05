function ballroom () 
    corpus_dataset = make_quaero_dataset(tilde_expand('~/Research/Data/IRCAM-Beat/ISMIR2004_tempo_ballroom/Analysis/'), '.wav.markers.xml');
    corpus_patterns = pattern_for_corpus(corpus_dataset, tilde_expand('~/Research/Data/IRCAM-Beat/ISMIR2004_tempo_ballroom/Audio'));
    write_corpus_as_arff(corpus_patterns, tilde_expand('~/Research/Data/IRCAM-Beat/ISMIR2004_tempo_ballroom/test.arff'));
end

%    sim = rhythm_similarity_of_corpus('Ballroom Dancers', ...
%        tilde_expand('~/Research/Data/IRCAM-Beat/ISMIR2004_tempo_ballroom/Analysis/'), ...
%        '.wav.markers.xml', ...
%        tilde_expand('~/Research/Data/IRCAM-Beat/ISMIR2004_tempo_ballroom/Audio/'));

