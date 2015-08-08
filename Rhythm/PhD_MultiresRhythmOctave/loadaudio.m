## -*- Octave -*-
## Copyright (C) 1996 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## usage:  X = loadaudio (name [, ext [, bit]])
##
## Loads audio data from the file "name.ext" into the data vector X
## as normalised values (-1 -> +1).
## Default value for the "ext" argument, which has to be written
## without the initial ".", is "lin".
## Currently, the following audio formats are supported:
## *) NeXT/Sun encoding with extension "au" or "snd"
## *) linear encoding with extension "lin" or "raw"
## *) muLaw encoding without the NeXT/Sun format "mu"
##
## The `bit' argument can be either 8 (default) or 16.
## Depending on the value of bit, linearly encoded files are
## interpreted as being in 8 and 16 bit format, respectively, and
## mu-law encoded files are transformed to 8 and 16-bit linear
## format, respectively.

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Created: 10 April 1994
## Adapted-By: jwe
## revised by Leigh Smith <leigh@cs.uwa.edu.au> to interpret the NeXT/Sun
## header properly.

function [X, soundheader] = loadaudio (name, ext, bit)

  if (nargin == 0 || nargin > 3)
    usage ("loadaudio (name [, ext [, bit]])");
  endif

  if (nargin == 1)
    ext = "lin";
    bit = 8;
  endif

  if (nargin == 3 && bit != 8 && bit != 16)
    error ("loadaudio: bit must be either 8 or 16");
  endif

  name = [name, ".", ext];
  num = fopen (name, "r");
  if (num == -1)
    error(["Unable to load ", name]);
  endif

  if (strcmp (ext, "lin") || strcmp (ext, "raw"))
    if (bit == 8)
      [Y, c] = fread (num, inf, "uchar");
      X = Y - 127;
    else
      [X, c] = fread (num, inf, "short");
    endif
  elseif (strcmp (ext, "au") || strcmp (ext, "snd"))
    [header, headerSize] = fread(num, 6, "integer*4", 0, "ieee-be");
    ## Each field 4 bytes:
    ## header field 1 = magic
    ## header field 2 = offset from header to sample data
    ## header field 3 = data size in bytes
    ## header field 4 = sample format enumerator
    ## header field 5 = sample rate
    ## header field 6 = channel count
    ## header
    ## check we have a NeXT/Sun header, which could be byte swapped
    if (header(1) == 779316836)
        ## are there TRUE/FALSE constants?
        switch(header(4))
        case 1
            soundheader.readFormat = "uchar";
            bit = 8;
            muLaw = 1;
        case 2
            soundheader.readFormat = "char";
            bit = 8;
            muLaw = 0;
        case 3
            soundheader.readFormat = "short";
            bit = 16;
            muLaw = 0;
        case 5
            soundheader.readFormat = "int";
            bit = 32;
            muLaw = 0;
        case 6
            soundheader.readFormat = "float";
            bit = 0;
            muLaw = 0
        case 7
            soundheader.readFormat = "double";
            bit = 0;
            muLaw = 0
        default
            error("loadaudio: Unsupported NeXT/Sun format");
        endswitch
        ## eat rest of header, which will be the info string. 
        soundheader.info = setstr(fread(num, header(2) - 24, "char").');
        soundheader.channels = header(6);
        soundheader.samplerate = header(5);
	## num of samples from all channels.
	numOfFrames = (header(3) / (bit / 8)) / soundheader.channels;
        [Y, c] = fread (num, [soundheader.channels, numOfFrames], soundheader.readFormat, 0, "ieee-be");
	## size(Y)
    else
        error("loadaudio: This doesn't seem to be a NeXT/Sun soundfile");
    endif
  elseif strcmp (ext, "mu")
    muLaw = 1;
    if(bit == 16)
      readFormat = "short";
    else
      readFormat = "uchar";
    endif
    [Y, c] = fread (num, inf, readFormat);
  else
    fclose (num);
    error ("loadaudio does not support given extension");
  endif

  if(muLaw)
     X = mu2lin (Y, bit);
  else
    ## Convert to normalized values.
     X = Y ./ (2 ^ (bit-1));
  endif
  fclose (num);

endfunction
