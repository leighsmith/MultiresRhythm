function [ similarity ] = pattern_similarity( syncopations )
%syncopation_similarity Return a similarity matrix from the syncopation measures.
%   All syncopation measures are wrt a single meter. n rhythms * t tatums.
%   Returns a n * n similarity matrix.
% $Id$

% TODO should pass in an array of rhythm patterns instead of a matrix.
% Derive the input matrix here.

rhythm_count = length(syncopations);
similarity = zeros(rhythm_count);

% Choice of distance measures between syncopation measures:
% * Euclidean distance, since this measures the match of relative
% syncopation at each tatum location.
% metric = 'euclidean';
% * Weighted Euclidean distance, weighting the relative
% importance of the match at each tatum by it's relative
% measure of perceptual salience.
% metric = ''; TODO
% * Vector dot product, measuring the degree of divergence
% between vectors in multidimensional space.
% metric = ''; TODO
% * cityblock (Manhattan) distance, measuring the distance by a set of
% independent dimensions for each metrical location, i.e each doesn't
% interact.
metric = 'cityblock';
% * Mahalanobis
% metric = 'mahalanobis';
% produces: NaNs & Matrix is singular to working precision. This is because
% there are zeros in the diagonal of the covariance matrix making the
% mrdivide operator explode.
% * spearman Rank correlation between observations, since all data is
% ordered the same, so we are comparing correlation of each metrical
% position.
% metric = 'spearman';
% produces error: Some points have too many ties, making them effectively
% constant.

% fprintf('covariance across examples %s\n', sprintf('%f ', diag(cov(syncopations))));

similarity = squareform(pdist(syncopations, metric));

if (diag_plot('similarity_matrix'))
    figure();
    imagesc(similarity)
    title(sprintf('Dissimilarity of pattern of Quaero Selection by %s distance metric', metric))
end

if (diag_plot('corpus_dissimilarity'))
    figure();
    bar(sum(similarity));
    title(sprintf('Dissimilarity of Quaero Selection'));
end

end

