MultiresRhythm V1.0 README

This is the Common Lisp implementation of MultiresRhythm, a representation of musical
rhythm using continuous wavelet transforms.

Requirements:

1. SBCL (http://www.sbcl.org) - A Common Lisp implementation that has been tested and is recommended.
2. ASDF - Another System Definition Facility.
3. CFFI - C foreign function library.
4. GSL - Gnu Scientific Library, used by NLISP.
5. NLISP - (http://www.nlisp.info) Numerical modelling and plotting library.

Installation:

With all required libraries and executables installed (follow their individual
installation instructions), execute:

CL-USER> (asdf:oos 'asdf:load-op 'cffi)
CL-USER> (asdf:oos 'asdf:load-op 'nlisp)
CL-USER> (asdf:oos 'asdf:load-op 'multiresrhythm)

Testing:

Test routines are in test-examples.lisp. You will need to change to the MULTIRES-RHYTHM
package in order to test these:

CL-USER> (in-package :multires-rhythm)
MULTIRES-RHYTHM> (clap-to-iois "local-timing" '(17 17 20.5 13.5 17 17))

