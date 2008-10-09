# -*- Octave -*-
#
## Convolve the data (inData) with each translation of the filter
## (mask) over the length of inData, extending inData to avoid edge effects.

function result = translate_convolve(inData, mask)
    mask_len = length(mask);
    data_len = length(inData);
    result = zeros(1, data_len);
    reflect = mask_len / 2;

    # create the transpose of the filter for the inner product
    mask_t = mask.';

    # reflect the data array about its ends in order to extend it
    # Extend the data to wrap around 
    buffer = [inData(data_len-reflect+1:data_len) inData inData(1:reflect) ];

    # buffer = [inData(reflect : -1 : 1) inData inData(data_len : -1 : data_len - reflect+1)];
    # This is the original code, but it doesn't seem right to me...
    # buffer = [inData(1:reflect) inData inData(data_len - 1 : -1 : data_len - reflect)];

# a space hungry way
#    foo = zeros(data_len, mask_len);
#    foo(1, :) = buffer(1:mask_len);
#    foo(2, :) = buffer(2:mask_len + 1);
#    (foo * mask_t).'

# a slow way
    for i = 1:data_len
	# inner product
        result(i) = buffer(i:i + mask_len - 1) * mask_t;
    endfor

endfunction

