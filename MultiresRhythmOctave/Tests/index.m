mag = [1,2,3,4,5,6;7,8,9,0,1,2;3,4,5,6,7,1;1,1,1,1,1,1]
sz = size(mag);
nScale = sz(1)
nTime = sz(2)
tactusMag = zeros(sz);
foo = ones(sz);
tactusIndices = [2,1,2,4,1,2]
fortran_indexes = ((0:nTime-1) .* nScale) + tactusIndices
tactusMag(fortran_indexes) = mag(fortran_indexes)
do_fortran_indexing = 1
tactusMag(fortran_indexes) = foo(fortran_indexes)
tactusMag(fortran_indexes) = 3

