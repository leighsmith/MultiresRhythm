# -*- Octave -*-
#
# generate a MorletWavelet kernel, scaled and translated to suit
# the period and k.

function mask = MorletWavelet(signalTimeExtent, period)
  k = 2.0;
  FREQ_SCALE = 1;		# make this omega to rescale linearly

  omega = 2.0 * pi / period;	# the radian frequency of the wavelet
  deltax = k * period;		# No of samples in k periods of the 
				# complex exponential 
  stdDeviation = deltax / sqrt(8.0); # No of samples in 1 std deviation
  width = 6.0 * stdDeviation;   # support of 3 std devs either side of 0

  if (!(width & 1))		# ensure No of mask elements is odd
    width++;
  endif
  
  x = [ (-width+1)/2:(width-1)/2 ];

  # create a Gaussian shape centered over our vector of length 'width'.
  gaussian_envelope = exp(-((2.0 * x / deltax) .^ 2.0));

  # Energy carried by a wavelet of constant shape ratio is
  # proportional to frequency, hence the scaling by omega if FREQ_SCALE
  # is defined.
  real = gaussian_envelope .* cos(omega * x); # * FREQ_SCALE;
  imag = gaussian_envelope .* sin(omega * x); # * FREQ_SCALE;
  mask = real + imag * 1i; 

  # need to eliminate any slight DC offset, which will be the mean value
  evensum = sum(mask);   # will this just be real only?
  evenmean = evensum / width;
  mask = mask - evenmean;

endfunction
