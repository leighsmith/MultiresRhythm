# patterns_file = '~/Research/Data/IRCAM-Beat/RWC/Analysis/rhythm_patterns.txt'
patterns_file = '~/Research/Data/Arkaiv/Analysis/rhythm_patterns.txt'

periodicity_feature_matrix = load(tilde_expand(patterns_file));
distance = 'euclidean';
# distance = 'cosine';

similarity = pattern_similarity(periodicity_feature_matrix, distance);

imagesc(similarity);
title(sprintf('Similarity of Tracks using %s distance between periodicity measures', distance));
