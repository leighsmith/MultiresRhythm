# -*- Octave -*-
#

function result = test_convo(inData, mask)
    mask_len = length(mask);
    data_len = length(inData);
    result = zeros(1, data_len);
    reflect = mask_len / 2;
    # buffer = [inData(reflect : -1 : 1) inData inData(data_len : -1 : data_len - reflect+1)];
    # This is the original code, but it doesn't seem right to me...
    buffer = [inData(1:reflect) inData inData(data_len - 1 : -1 : data_len - reflect)]
    for i = 1:data_len
        result(i) = 0.0;
        for j = 1:mask_len
#	    printf("[%d](mask = %f) * (buffer = %f) = %f\n", j, mask(j),
               buffer(i + j - 1), intermediate);
            result(i) = result(i) + mask(j) * buffer(i + j - 1);
        endfor
    endfor
endfunction

