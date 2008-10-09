### -*- Octave -*-
###
### $Id$
###
### plotRidges -- plot the magnitude maxima of the Continuous Wavelet Transform
###
### assumes all ridge matrices are the same dimensions.

function plotRidges(comment, ridges1, r1Title, ridges2, r2Title, ridges3, r3Title, ridges4, r4Title) 
  global promptStream;

  fprintf(promptStream, "Plotting magnitude maxima (dilation derivative), stationary phase & localPC ridges\n");
  figure(2);

  titlestr = ['Ridges of ', comment];
  xlabel('Time')
  ylabel('log2(Scale)')
  if ~isempty(titlestr),
    title(titlestr);
  endif

  sz     = size(ridges1);
  nScale = sz(1)
  nTime  = sz(2)

  whatToPlot = "plot(";

  [nonzeroScales, nonzeroTimes] = find(ridges1);
  x1Axis = (nonzeroTimes-1);
  y1Axis = nScale - nonzeroScales;
  whatToPlot = [whatToPlot, sprintf("x1Axis, y1Axis, \"1.;%s;\"", r1Title)];
  if(nargin > 3)
    [nonzeroScales, nonzeroTimes] = find(ridges2);
    ## ugly hack: if we have massive amount of points to plot, only plot the bottom half
    portion = 0.5
    if(length(find(ridges2)) > 26000)
      [nonzeroScales, nonzeroTimes] = find(ridges2(nScale * (1 - portion):nScale,:));
      x2Axis = (nonzeroTimes-1);
      y2Axis = nScale * portion - nonzeroScales + 1;
    else
      x2Axis = (nonzeroTimes-1);
      y2Axis = nScale - nonzeroScales;
    endif
    whatToPlot = [whatToPlot, sprintf(", x2Axis, y2Axis, \"2.;%s;\"", r2Title)];
  endif
  if(nargin > 5)
    [nonzeroScales, nonzeroTimes] = find(ridges3);
    ridge3Locs = [(nonzeroTimes-1), nScale - nonzeroScales];
    whatToPlot = [whatToPlot, sprintf(", ridge3Locs, \"4.;%s;\"", r3Title)];
  endif
  if(nargin > 7)
    whatToPlot = [whatToPlot, sprintf(", ridge4Locs, \"1^;%s;\"", r4Title)];
  endif

  whatToPlot = [whatToPlot, ");"];

  eval(whatToPlot);

endfunction

  ##[nonzeroScales, nonzeroTimes] = find(ridges4);
  ##x4 = (nonzeroTimes-1);
  ##y4 = nonzeroScales;

  ## plot(xm, ym, "bx", xd, yd, "g.", xp, yp, "m*", pc .* nScale, "r");
  ## plot(xm, ym, "bx", xd, yd, "g.", xp, yp, "r*", xc, yc, "m@+");
  ## plot(xp, yp, "r*", xc, yc, "m@+");
  ## plot(xm, ym, "bx", xd, yd, "g.", xp, yp, "r+", xpd, ypd, "m@+", pc * nScale, "c");
#    if(nargin > 5)
#      plot(nScale - expectedTactus, "mx", x1, y1, "r.", x2, y2, "b.", \
#        x3, y3, "m.", nScale - computedTactus, "b+");
#    elseif(nargin > 4)
#      plot(x1, y1, "r.", x2, y2, "b.", \
#        x3, y3, "m.", nScale - computedTactus, "b+");
#    elseif(nargin > 3)
#      plot(x2, y2, "mo", x1, y1, "r.", x3, y3, "g.");
#    else
#      plot(x2, y2, "bo", x1, y1, "r.");
#    endif

## ridges1,ridges2,ridges3,computedTactus,expectedTactus

