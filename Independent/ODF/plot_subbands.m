function plot_subbands(subbands, plot_region)
%
% Author: Leigh M. Smith <leigh@imagine-research.com>
%
% $Id: odf_of_file.m 5684 2011-06-28 21:30:39Z leighsmi $
%
    figure()
    numOfPlots = size(subbands, 1)
    for plotIndex = 1 : numOfPlots
        subplot(numOfPlots, 1, plotIndex);
        plot(subbands(plotIndex, plot_region));
        title(sprintf('Subband %d ODF of %s', plotIndex, 'signal'));
        % axis([plot_region(1) plot_region(end) min(subbands_odf(1, plot_region))-1 ...
        %      max(subbands_odf(1, plot_region))+1]);
    end
end
