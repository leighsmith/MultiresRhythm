## -*- Octave -*-
##
## $Id$
##
function plotRidgesProfile(comment, timep, r1, r1Title, r2, r2Title, \
			   r3, r3Title, r4, r4Title, r5, r5Title) 
  printf("Plot of ridges in scale profile at time %d on figure 2\n", timep);
  figure(2);
  titleStr = sprintf("Ridges profile of %s at time %d", comment, timep);
  title(titleStr);
  
  sz = size(r1);
  nScale = sz(1);
  nTime = sz(2);

  profile_portion = nScale:-1:1;
  ##gset xtics 0,2,150
  ##gset ytics 0,0.2,1
  xlabel('Scale number')
  ylabel('Normalised ridge values')

  whatToPlot = "plot(";
  if(nargin > 2)
    r1AtT = r1(profile_portion, timep);
    whatToPlot = [whatToPlot, sprintf("r1AtT, \"gL;%s;\"", r1Title)];
  endif
  if(nargin > 4)
    r2AtT = r2(profile_portion, timep);
    whatToPlot = [whatToPlot, sprintf(", r2AtT, \"7L;%s;\"", r2Title)];
  endif
  if(nargin > 6)
    r3AtT = r3(profile_portion, timep);
    whatToPlot = [whatToPlot, sprintf(", r3AtT, \"6L;%s;\"", r3Title)];
  endif
  if(nargin > 8)
    r4AtT = r4(profile_portion, timep);
    whatToPlot = [whatToPlot, sprintf(", r4AtT, \"1^;%s;\"", r4Title)];
  endif
  if(nargin > 10)
    r5AtT = r5(profile_portion, timep);
    whatToPlot = [whatToPlot, sprintf(", r5AtT, \"b^;%s;\"", r5Title)];
  endif

  whatToPlot = [whatToPlot, ");"];

  eval(whatToPlot);
endfunction

