function [ similarity ] = pattern_similarity( rhythm_features, metric )
%pattern_similarity Return a similarity matrix from the rhythm feature vector measures.
%   All rhythm features are with respect to a single meter. n rhythms * t tatums.
%   Returns a n * n similarity matrix.
% $Id$

rhythm_count = length(rhythm_features);

if(nargin < 2)
    % Choice of distance measures between syncopation measures:
    % * Euclidean distance, since this measures the match of relative
    % syncopation at each tatum location.
    metric = 'euclidean';
    % * Weighted Euclidean distance, weighting the relative
    % importance of the match at each tatum by it's relative
    % measure of perceptual salience.
    % metric = 'seuclidean'; TODO
    % * Vector dot product, measuring the degree of divergence
    % between vectors in multidimensional space.
    % metric = ''; TODO
    % * cityblock (Manhattan) distance, measuring the distance by a set of
    % independent dimensions for each metrical location, i.e each doesn't
    % interact.
    % metric = 'cityblock';
    % * 'cosine' the normalised inner angle between vectors.
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
    % constant. Since we have data which exhibits normality, homoscedasticity and
    % linearity, we can just use correlation distance.
    % metric = 'correlation';
end

% fprintf('covariance across examples %s\n', sprintf('%f ', diag(cov(rhythm_features))));

similarity = squareform(pdist(rhythm_features, metric));

if (diag_plot('corpus_dissimilarity'))
    figure();
    bar(sum(similarity));
    title(sprintf('Dissimilarity of Quaero Selection'));
end

end

