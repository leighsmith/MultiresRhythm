# Construct a Real-valued Gaussian filter in the Fourier domain
function ghat = GaussWaveletFFT(signalPeriod, waveletPeriod)

  # create vector 0->pi, -pi->0 discretised
  xi = [ (0: (signalPeriod/2)) (((-signalPeriod/2)+1):-1) ] .* (2*pi/signalPeriod);

  omega =  signalPeriod .* xi ./ waveletPeriod;

  ghat = exp(-omega.^2 ./2);

endfunction

