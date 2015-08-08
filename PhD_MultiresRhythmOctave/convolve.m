# -*- Octave -*-
#

function result = translate_convolve(inData, mask)
    mask_len = length(mask);
    data_len = length(inData);
    result = zeros(1, data_len);
    reflect = mask_len / 2;
    # reflect the data array about its ends in order to extend it
    # Extend the data to wrap around 
    # buffer = [inData(reflect : -1 : 1) inData inData(data_len : -1 : data_len - reflect+1)];
    # This is the original code, but it doesn't seem right to me...
    buffer = [inData(1:reflect) inData inData(data_len - 1 : -1 : data_len - reflect)]
    for i = 1:data_len
        result(i) = sum(mask .* buffer(i:i + mask_len - 1))
    endfor
endfunction

