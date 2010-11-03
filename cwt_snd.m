#!/usr/local/bin/octave -qf
# -*- Octave -*-
# Perform the conversion on each of the files named on the command line.
# no extensions are supplied.

for i = 1:nargin
  filename = argv(i,:);
  [mag, phase, pc] = multiresFile("File", filename);
endfor
