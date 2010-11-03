# simple function to plot out our kernel, enabling us to test scaling behaviour
function plotwavelet(ext, col1, col2)
  t = [-ext : ext * 2 / 2048.0 : ext];
  gaussian_envelope = exp(-(t .^ 2.0)/2);
  omega = 5;
  wavelet = gaussian_envelope .* exp(1i .* omega * t);
  plot(real(wavelet), col1, imag(wavelet), col2);
endfunction
