function gaussian_envelope = gauss(width)
%%gauss Compute a gaussian envelope.
%% width is in the number of points to range an envelope over 3
%% standard deviations.
  % x = [ -0.5 : 1/width : 0.5 ];
  % x = [ -1.49 : 2/width : 0.5 ];
  % x = [ -0.49 : 2/width : 1.49 ];
  x = [ -0.99 : 2/width : 1.00 ];
  gaussian_envelope = exp(-((2.0 * x * (6 / sqrt(8))) .^ 2.0));
end
