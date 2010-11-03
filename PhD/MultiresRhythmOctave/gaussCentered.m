### -*- Octave -*-
### Given a matrix with single impulses at each column, place a
### Gaussian at each column (across the rows) centered over the impulse.
function gaussians = gaussCentered(centered, voicesPerOctave)
  sz = size(centered);
  nScale = sz(1);

  gwidth = voicesPerOctave * 2;
  d = zeros(1,nScale);
  d(1:gwidth) = gauss(gwidth);
  f = shift(d, -gwidth/2).';

  c = zeros(sz);
  ## yuck, put each gaussian in the right place.
  ## surely this can be done in one expression, not a loop?
  for i=1:nScale
    c(i,:) = f(i);
  end

  ## fftconv(c, b)?
  gaussians = real(ifft(fft(c) .* fft(centered)));

endfunction
