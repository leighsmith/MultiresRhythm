mask = 1 ./ [1:8];
inData = [ 1.0, 2.0, 3.0, 2.0, 1.0, 0.5, 2.5, 3.0 ];
translate_convolve(inData, mask)

mask_fft =  fft([mask zeros(1, length(inData)-1)])
inData_fft = fft([inData zeros(1, length(mask)-1)])
ifft(mask_fft .* inData_fft)
