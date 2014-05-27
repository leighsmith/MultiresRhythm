## -*- Octave -*-
function plotTactusSamples(comment, signal, clapAt, clapIntensity, tactus)
  global promptStream;
  sz = size(tactus)
  nScale = sz(1);
  nTime  = sz(2);

  fprintf(promptStream, "Press a key for the tactus sample points\n");
  pause
  title(['Computed foot-tap and expected foot-tap of ', comment]);
  xlabel('Time')
  ylabel('Normalised Intensity')

  plot(signal, "m^", clapAt, clapIntensity, "g^", tactus ./ nScale, "r.");
endfunction
