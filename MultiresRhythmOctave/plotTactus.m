## -*- Octave -*-
##
## $Id$
## strata plotting code
function plotTactus(comment, ridges, computedTactus, expectedTactus)
  printf("Plotting correlated ridge, computed tactus and expected tactus on figure 5\n");
  figure(5);

  xlabel('Time')
  ylabel('log2(Scale)')
  sz     = size(ridges);
  nScale = sz(1);
  nTime = sz(2);

  [nonzeroScales, nonzeroTimes] = find(ridges);
  xd = (nonzeroTimes-1);
  yd = nScale - nonzeroScales;
  combinedRidge = [xd+1, yd];
  if(nargin < 3)
    title(['Combined Ridge of ', comment]);
    ## plot(xd+1, yd, "r.");
    plot(xd+1, yd, "r.;Combined Ridge;");
  elseif(nargin < 4)
    title(['Combined Ridge and Computed Tactus of ', comment]);
    ## plot(xd+1, yd, "r.", nScale - computedTactus, "b.");
    ## gplot combinedRidge with dots 1 title 'Combined Ridge', \
    ## 	(nScale - computedTactus).' with dots 3 title 'Computed Tactus';
    plot(xd+1, yd, "r.;Combined Ridge;", \
    	(nScale - computedTactus).', "b.;Computed Tactus;");
  else
    title(['Combined Ridge, Computed Tactus and Expected Tactus of ', comment]);
    ## plot(xd+1, yd, "r.", nScale - computedTactus, "b.", nScale - expectedTactus, "m.");
    plot(xd+1, yd, "r.;Combined Ridge;", \
	(nScale - computedTactus).', "b.;Computed Tactus;", \
	(nScale - expectedTactus).', "m.;Expected Tactus;");
  endif
endfunction




