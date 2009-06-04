function [ similarity ] = syncopation_similarity( syncopations )
%syncopation_similarity Return a similarity matrix from the syncopation measures.
%   All syncopation measures are wrt a single meter. n rhythms * t tatums.
%   Returns a n * n similarity matrix.

rhythm_count = length(syncopations);
similarity = zeros(rhythm_count);

similarity = squareform(pdist(syncopations));

% for i = 1 : rhythm_count
%     for j = 1 : rhythm_count
%         if (i ~= j)
%             % Choice of distance measures between syncopation measures:
%             % 1. Euclidean distance, since this measures the match of relative
%             % syncopation at each tatum location.
%             % 2. Weighted Euclidean distance, weighting the relative
%             % importance of the match at each tatum by it's relative
%             % measure of perceptual salience.
%             % 3. Vector dot product, measuring the degree of divergence
%             % between vectors in multidimensional space.
%             similarity(i, j) = 0.0;
%         end
%     end
% end

end

