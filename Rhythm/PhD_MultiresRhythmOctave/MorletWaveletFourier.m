## -*- Octave -*-
## Construct a MorletWavelet filter in the Fourier domain
## within the length of the signal (so we can multiply).
## Leigh Smith, 3/3/1998, borrowing heavily from Wavelab
##
## waveletScale = dilation parameter a
function ghat = MorletWaveletFourier(signalTimePeriod, waveletScale)
  ## Internal frequency achieving a minimal DC component, 
  ## see I. Daubechies "Ten Lectures on Wavelets" p76
  ## We use 6 in order to have pulse IOI match their theoretical time support.
  omega0 = 6.2;    ## was 5, 5.3364, 6 matches pulse trains with 8vpo

  ## create vector of radian frequencies, (0->pi, -pi->0) discretised
  ## over the signalTimePeriod.
  xi = [ (0: (signalTimePeriod/2)) (((-signalTimePeriod/2)+1):-1) ] .* (2*pi/signalTimePeriod);

  ## From Grossmann, Kronland-Martinet and Morlet we multiply the waveletScale
  ## by the discretised omega value when scaling in the Fourier domain.
  omega = xi .* waveletScale;

  ## plot(omega)

  ## See mallat:tour p76
  ## According to Holschneider sqrt(2 .* pi) .* 
  ## check with Daubechies pi ^ (-1/4)
  ghat = exp(-(omega - omega0) .^ 2 ./ 2) - exp(-(omega .^ 2 + omega0 .^ 2) / 2);

  ## plot(ghat);
  ## time_wavelet = ifft(ghat);
  ## plot(real(time_wavelet),"1",imag(time_wavelet),"2")

endfunction


