MultiresRhythm V2.8 README
Leigh M. Smith <leigh@leighsmith.com>
2008/05/15

This is the Common Lisp implementation of MultiresRhythm, a representation of musical
rhythm using continuous wavelet transforms. The distribution consists of a library
(MultiresRhythm) and a test suite (DORYS). 

Requirements:

1. GSL - Gnu Scientific Library, used by NLISP.
2. SBCL (http://www.sbcl.org) - A Common Lisp implementation that has been tested and is recommended.
3. ASDF - Another System Definition Facility (included in SBCL).
4. CFFI - C foreign function library for Common Lisp.
5. NLISP - (http://www.nlisp.info) Numerical modelling and plotting library.

Installation:

1. Install GSL

1.1 On Linux install libgsl using Debian apt or RedHat rpm.

1.2 On MacOS X install gsl with fink:

% fink install gsl

2. Install SBCL

2.1 On Linux install sbcl using Debian apt or RedHat rpm.

2.2 On MacOS X install sbcl with fink:

% fink install sbcl

3. Install MultiresRhythm packages

3.1 Unarchive the rhythm package:

% unzip MultiresRhythm-2.7.zip

3.2 Create symbolic links to it in .sbcl/systems:

% ln -s ~/your_dir/MultiresRhythm_2.7/multiresrhythm.asd ~/.sbcl/systems/multiresrhythm.asd

4. Install libraries.

Run SBCL, load ASDF and ASDF-Install, install CFFI and then NLISP.

% sbcl
CL-USER> (require 'asdf)
CL-USER> (require 'asdf-install)
CL-USER> (asdf-install:install 'cffi)
CL-USER> (asdf-install:install 'nlisp)
CL-USER> (require 'multiresrhythm)

If you don't want to (or can't) create the symlinks, you can tell ASDF where to find them
prior to running (require 'multiresrhythm):

CL-USER> (pushnew "/path/to/your/dorys.asd" "/path/to/your/multiresrhythm.asd" asdf:*central-registry*)

Testing with DORYS:

% unzip DORYS-1.1.zip
% unzip LHL_BeatInduction-1.0.zip
% ln -s ~/your_dir/DORYS_0.1/dorys.asd ~/.sbcl/systems/dorys.asd
% ln -s ~/your_dir/LHL_BeatInduction_1.0/shoe.asd ~/.sbcl/systems/shoe.asd

Test routines are in test-multires-rhythm.lisp. You will need to change to the MULTIRES-RHYTHM
package in order to test these:

CL-USER> (in-package :multires-rhythm)
MULTIRES-RHYTHM> (clap-to-iois "local-timing" '(17 17 20.5 13.5 17 17))

To generate a binary:

CL-USER> (require 'multiresrhythm)
CL-USER> (multires-rhythm:generate-beat-track-executable)

Distribution:

Copyright (c) 2006-2014 All rights reserved. Leigh M. Smith
Please contact me before distributing beyond the EmCAP web site.
