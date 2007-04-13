;;; National Anthems data set, as described in:
;;; Desain, P. and Honing, H. (1999). Computational Models of Beat Induction: 
;;; The Rule-Based Approach. Journal of New Music Research, 28(1), 29-42.

;;; last edited 29 March 2007, added remarks of LH (honing@uva.nl)


#|

_______________________________________________________________________

The format we use for our encoding of a score is a list of two lists, 
the first of which contains the name of the country (symbol), the time 
signature (a string), name/title of the anthem (a list of strings in UTF-8 encoding),
duration of a quarter-note (in grid units), duration of the beat (in grid units), 
and start-at (i.e. the position in the bar where the upbeat starts, in grid units).


As an example:

((AMERICA "3/4" ("The Star-Spangled Banner") 
        :QUARTER-NOTE 4 :BAR-DURATION 12 :START-AT 8)
  (3 1 4 4 4 8 3 1 4 4 4 8 2 2 6 2 4 8 2 2 4 4 4 4 4 3 1 4 4 4 8 2 2 
        4 4 4 8 4 6 2 4 8 2 2 4 4 4 8 4 4 4 2 2 4 4 4 4 2 2 2 2 4 4 
        2 2 6 2 2 2 8 2 2 6 2 4))

or in an alternative notation (with barlines represented by |):

     3 1 | 4 4 4 | 8 ...

since a bar lasts 12 units, the upbeat starts at 8 units, 
a quarter-note equals 4.

Another example showing how we encode more complex anthems. 
Nepal contains dotted eigth-notes and a triplet:

((NEPAL "4/4" ("National anthem for H.M. the Maharaja Dhiraja") 
        :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 36)
 (12 24 24 18 6 12 12 9 3 12 9 3 12 12 9 3 12 12 24 24 18 6 12 12 9 
        3 9 3 9 3 9 3 12 9 3 12 12 8 6 9 3 9 3 18 6 12 12 12 9 3 12 
        9 3 12 9 3 12 12 24 24 24 24 12 4 4 4 24 24))

or in an alternative notation (with barlines represented by |):

     12 | 24 24 | 18 ...

with a quarter-note equals 12.

We used this quarter-note information to adjust (scale) the parameters of the 
rule=-based models. For instance, in the later case (Nepal) the parameters of the 
model were multiplied by 3, in the first case (America).

|#

(in-package :multires-rhythm)

(defparameter *national-anthems* '(

((AFGHANISTAN "2/4" ("") :QUARTER-NOTE 12 :BAR-DURATION 24 :START-AT 21) 
(3 18 3 3 18 3 3 18 3 3 18 3 3 18 3 3 18 3 3 18 6 18 3 3 24 24 18 3 3 18 6 18 3 3 18 3 3 18 6 18 6 36 4 4 4)) 

((AFRICA "4/4" ("Nkosi Sikeleli Africa") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 0) 
(1 1 1 1 2 2 2 2 4 1 1 1 1 2 2 1 1 2 4 1 1 1 1 2 2 2 2 4 4 4 1 1 2 4 4 4 1 1 2 4 2 1 1 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 1 1 4 1 1 2 4 1 1 2 4 4 4 1 1 2 4 4 3 1 1 1 2)) 

((ALBANIA "4/4" ("Hymni i Flamurit") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(4 6 2 2 2 3 1 4 6 2 2 2 6 2 4 4 12 4 6 2 2 2 3 1 4 6 2 2 2 6 2 4 4 12 4 6 2 2 2 2 2 6 2 2 2 2 2 4 4 4 4 12 4 6 2 4 4 3 1 3 1 4 4 6 2 2 2 2 2)) 

((AMERICA "3/4" ("The Star-Spangled Banner") :QUARTER-NOTE 4 :BAR-DURATION 12 :START-AT 8) 
(3 1 4 4 4 8 3 1 4 4 4 8 2 2 6 2 4 8 2 2 4 4 4 4 4 3 1 4 4 4 8 2 2 4 4 4 8 4 6 2 4 8 2 2 4 4 4 8 4 4 4 2 2 4 4 4 4 2 2 2 2 4 4 2 2 6 2 2 2 8 2 2 6 2 4)) 

((ANDORRA "4/4" ("Himne Andorra") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(4 4 3 1 4 4 6 2 4 3 1 4 4 4 4 12 3 1 4 4 4 4 6 2 4 4 4 4 4 3 1 8 4 4 4 3 1 4 4 6 2 4 3 1 4 4 4 4 6 2 4 4 4 4 4 3 1 6 2 4 3 1 4 4 4 4 12 4 6 2 3 1 3 1 6 2 4 4 4 4 4 3 1 16 4 3 1 4 3 1 12 3 1 4 4 4 4 8 7 1 7 1 7 1)) 

((ARGENTINE "4/4" ("Himno Nacional") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 10) 
(2 2 2 4 3 1 2 2 3 1 6 2 4 3 1 4 3 1 4 3 1 11 1 3 1 4 3 1 2 2 3 1 4 8 3 1 4 3 1 4 3 1 4 2 2 2 2 4 3 1 2 2 3 1 8 4 3 1 4 3 1 4 3 1 12 3 1 4 3 1 4 3 1 4 8 3 1 4 3 1 4 3 1 4 8 3 1 2 2 2 2 2 2 2 2 10 2 2 2 4 3 1 2 2 2 2 4 8 3 1 2 2 2 2 3 1 2 2 10 2 2 2 4 3 1 2 2 3 1 4 8 3 1 2 2 2 2 3 1 2 2 9 2 3 1 4 4 4 4 4 8 3 1 4 4 4 4 12 3 1 4 4 4 4 12 3 1 4 3 1 2 2 3 1 3 1 1 1 1 1 2 2 2 2 4 2 2 4 2 2 8 6 2 2 2 2 2 4 2 2 8 6 2 2 2 2 2 4 2 2)) 

((AUSTRALIA "4/4" ("Advance, Australia Fair") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 6) 
(2 2 2 2 2 3 1 2 2 2 2 2 2 6 2 2 2 2 2 3 1 2 2 2 2 2 2 6 2 3 1 2 2 3 1 2 2 2 2 2 2 6 2 3 1 2 2 3 1 2 2 3 1 3 1 6 2 2 2 2 2 2 2 2 2 3 1 3 1)) 

((AUSTRIA "3/4" ("Oesterreichische Bundeshymne") :QUARTER-NOTE 2 :BAR-DURATION 6 :START-AT 0) 
(4 2 4 1 1 4 2 2 4 4 2 4 1 1 4 2 1 1 4 4 2 4 2 2 2 2 6 4 2 2 2 2 4 1 1 2 4 4 2 2 2 2 4 1 1 2 4 4 2 4 2 1 1 2 2 6 4 2 4 1 1 1 1 2 2)) 

((BAHREIN "4/4" ("") :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 0) 
(30 3 3 6 6 30 3 3 6 6 36 12 12 12 6 6 6 6 24 12 12 6 3 3 4 4 4 6 3 3 4 4 4 6 3 3 4 4 4)) 

((BELGIUM "4/4" ("La brabanconne") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 11) 
(1 3 1 4 3 1 3 1 3 1 6 1 1 3 1 3 1 7 1 3 1 3 1 10 2 3 1 4 3 1 3 1 3 1 6 1 1 3 1 3 1 3 1 7 1 3 1 11 1 3 1 4 3 1 3 1 3 1 8 3 1 3 1 7 1 3 1 3 1 11 1 3 1 6 2 3 1 3 1 7 1 3 1 3 1 7 1 3 1 3 1 11 1 3 1 7 1 3 1 3 1 4 3 1 3 1 3 1 7 1 3 1 3 1 11 1 3 1 7 1 3 1 3 1 11 1 3 1 7 1 3 1 3 1)) 

((BOLIVIA "4/4" ("Himno Nacional") :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 36) 
(9 3 12 9 3 12 9 3 24 12 9 3 9 3 9 3 9 3 9 3 24 12 9 3 12 9 3 12 9 3 24 12 4 4 4 12 9 3 6 6 9 3 36 9 3 12 9 3 9 3 9 3 24 12 9 3 9 3 9 3 9 3 9 3 18 6 12 9 3 9 3 9 3 9 3 9 3 18 6 12 9 3 12 9 3 9 3 4 4 4 36 9 3 9 3 9 3 9 3 9 3 18 3 3 6 6 9 3 12 9 3 9 3 4 4 4 36 9 3 24 9 3 9 3 24 12 9 3 9 3 9 3 9 3 9 3 24 12 9 3 18 6 9 3 4 4 4 24 12 9 3 12 9 3 12 9 3 36 9 3 12 9 3 12 9 3 36 9 3 12 9 3 12 9 3)) 

((BRAZIL "4/4" ("himno Nacional") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(4 3 1 3 1 3 1 3 1 6 2 4 4 3 1 3 1 3 1 3 1 4 8 4 3 1 3 1 3 1 3 1 6 2 4 4 3 1 3 1 3 1 3 1 4 8 3 1 4 6 2 3 1 4 8 2 2 2 2 2 2 2 2 2 2 4 8 3 1 4 6 2 3 1 4 8 2 2 2 2 2 2 2 2 2 2 4 6 2 2 2 2 2 2 2 2 2 2 2 12 4 3 1 3 1 3 1 3 1 6 2 4 4 3 1 3 1 3 1 3 1 4 8 4 3 1 3 1 3 1 3 1 6 2 4 4 3 1 3 1 3 1 3 1 4 11 1 3 1 3 1 3 1 3 1 6 2 4 3 1 3 1 3 1 3 1 3 1 6 2 7 1 3 1 3 1 3 1 3 1 4 6 2 2 2 4 6 2 2 2 2 2 2 2 2 2 2 2 4 6 2 2 2 2 2 2 2 6 2 4 3 1 4 3 1)) 

((BULGARIA "4/4" ("Bulgaria mela, zemya na gheroi") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 14) 
(2 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 6 2 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 2 2 8 8 4 4 6 2 8 8 4 4 8 8 4 3 1 4 3 1 4 3 1 8 4 3 1 4 3 1 4 3 1 4 4 8 4 4 8 4 4 4 4 4 4)) 

((BURMA "2/2" ("Gba majay") :QUARTER-NOTE 6 :BAR-DURATION 24 :START-AT 0) 
(9 3 12 9 3 12 6 3 3 6 3 3 6 3 3 12 6 3 3 6 3 3 6 3 3 12 6 3 3 6 3 3 6 3 3 12 12 12 12 12 12 12 4 4 4 12 12 4 4 4 12 12 4 4 4 12 12 12)) 

((CAMBODIA "4/4" ("Mokoreach") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 8) 
(4 2 2 8 2 2 2 2 10 1 1 2 2 3 1 2 4 2 4 6 2 2 2 2 2 2 2 2 2 4 4 10 1 1 2 2 4 4 1 1 1 1 2 2 10 1 1 2 2 4 4 1 1 1 1 2 2 12 4 4 4 3 1 2 2)) 

((CAMEROON "4/4" ("Chant de Ralliement") :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 33) 
(3 9 3 12 12 9 3 9 3 24 12 9 3 9 3 9 3 9 3 9 3 33 3 9 3 12 9 3 12 9 3 24 12 9 3 9 3 9 3 9 3 9 3 24 12 4 4 4 9 3 9 3 9 3 9 3 36 4 4 4 12 9 3 9 3 9 3 33 3 9 3 9 3 9 3 9 3 9 3 36 4 4 4 9 3 9 3 9 3 9 3 36 9 3 24 12 9 3 24 12 12 12 9 3 9 3 9 3 36 9 3 24 12 9 3 24 9 3 9 3 12 9 3 12 12)) 

((CANADA "4/4" ("O Canada") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 0) 
(8 6 2 12 4 4 4 4 4 16 8 6 2 12 4 4 4 4 4 12 3 1 6 2 4 3 1 6 2 4 3 1 4 4 4 4 12 3 1 6 2 4 3 1 6 2 4 4 4 4 2 2 2 2 16 8 6 2 16 8 6 2 16 8 6 2 4 4 4 4 8 8 16 8 6 2 4 4 4 4 8 8)) 

((CEYLON "4/4" ("Namo Namo Matha") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 0) 
(1 2 1 2 1 1 6 1 1 1 1 1 1 1 1 1 1 3 1 3 1 3 1 3 1 2 1 1 4 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 2 1 1 1 8 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 2 1 1 1 1 8 1 1 1 1 1 1 1 1 1 1 1 1 4 2 1 1 2 1 1 8 1 1 1 1 1 1 1 1 2 2 2 2 1 2 1 2 1 1 6 1 1 1 1 1 1 1 1 1 1 3 1 3 1 3 1 3 1 2 1 1 2 1 1 1 1 1 1 1 1 1 1 3 1 3 1 3 1 3 1 2 1 1)) 

((CHILE "4/4" ("himno nacional") :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 36) 
(9 3 9 3 9 3 9 3 9 3 18 6 12 9 3 9 3 4 4 4 9 3 4 4 4 36 9 3 9 3 9 3 9 3 9 3 18 6 12 9 3 9 3 9 3 9 3 9 3 36 9 3 12 9 3 6 6 6 6 18 6 12 9 3 12 9 3 6 6 6 6 36 9 3 12 9 3 4 4 4 4 4 4 36 9 3 9 3 9 3 9 3 9 3 12 24 9 3 9 3 9 3 9 3 9 3 12 12 9 3 4 4 4 9 3 9 3 9 3 4 4 4 12 12 9 3 4 4 4 9 3 4 4 4 4 4 4 4 4 4 48 36 12 24 12 12 24 9 3 9 3 12 36 36 12 24 12 12 24 9 3 9 3 36 9 3 12 9 3 12 9 3 12 24 9 3 12 9 3 12 9 3 36 9 3 9 3 9 3 9 3 9 3 18 6 12 9 3 9 3 9 3 9 3 9 3 12 6 6 6 6 6 6 24 12 12 12 24 9 3 12 9 3 12 9 3 36 9 3 12 9 3 12 9 3 36 9 3 12 9 3 12 9 3)) 

((CHINA "4/4" ("San min chu I") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(4 12 4 12 4 12 4 12 4 12 2 2 12 4 12 2 2 12 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 12 2 2 12 4 12 2 2 12 4 12 3 1 12 4 12 3 1)) 

((COLOMBIA "4/4" ("Himno nacional") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(3 1 4 4 4 3 1 8 4 3 1 4 4 4 4 12 3 1 4 4 4 3 1 8 4 4 4 3 1 4 3 1 12 3 1 4 3 1 4 3 1 8 8 4 6 2 2 2 8 8 4 6 2 2 2 12 3 1 4 3 1 4 3 1 4 8 3 1 4 3 1 4 3 1 16 4 3 1 3 1 3 1 8 8 4 3 1 3 1 3 1 7 1 8 4 3 1 3 1 3 1 8 8 4 3 1 6 2 12 4 6 2 4 4 8 4 4 6 2 4 4 8 4 4 8 4 3 1 8 4 4 6 2 6 2)) 

((COSTA-RICA "4/4" ("himno nacional") :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 36) 
(9 3 12 9 3 12 9 3 12 24 9 3 12 9 3 12 9 3 36 9 3 12 9 3 12 9 3 12 24 9 3 12 9 3 12 9 3 36 9 3 12 6 6 12 9 3 12 6 6 12 9 3 12 9 3 12 9 3 28 4 4 4 4 4 12 9 3 12 9 3 12 9 3 12 9 3 12 9 3 6 6 6 6 12 9 3 12 9 3 12 9 3 6 6 6 6 36 9 3 36 9 3 36 9 3 36 9 3 28 4 4 4 4 4 12 9 3 12 9 3 12 9 3 12 9 3 12 9 3 6 6 6 6 12 9 3 12 9 3 12 12 6 6 6 6 36 9 3 12 9 3 12 9 3 12 24 9 3 12 9 3 12 9 3 36 9 3 12 9 3 12 9 3 12 24 9 3 12 9 3 12 9 3)) 

((CUBA "2/4" ("La Bayamesa") :QUARTER-NOTE 4 :BAR-DURATION 8 :START-AT 4) 
(3 1 4 3 1 4 3 1 8 4 3 1 4 3 1 3 1 3 1 8 4 3 1 4 3 1 4 3 1 8 4 3 1 4 3 1 4 3 1 12 3 1 4 3 1 4 3 1 8 4 3 1 4 3 1 3 1 3 1 8 4 3 1 4 3 1 4 3 1 8 4 3 1 4 3 1 4 3 1 8)) 

((CZECHOSLOVAKIA-1 "4/4" ("part 1: Kde Domoj Muj?") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 2) 
(4 1 1 2 4 1 1 6 1 1 1 1 4 1 1 1 1 4 1 1 1 1 1 1 2 1 1 2 4 1 1 5 1 1 1 6 1 1 5 1 1 1 6 1 1 3 1 2 1 1 1 1 4 1 1 2 4 1 1 2 4 1 1 2 4 1 1)) 

((CZECHOSLOVAKIA-2 "4/4" ("part 2: Nad Tatru sa blyska") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 0) 
(1 1 2 2 1 1 1 1 1 1 2 2 1 1 2 2 1 1 1 1 1 1 2 2 1 1 2 2 1 1 1 1 2 2 1 1 1 1 1 1 2 2 1 1 2 2 1 1 1 1 2 2 1 1 1 1 1 1 2)) 

((DENMARK "4/4" ("Kong Kristian") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 6) 
(2 2 2 2 1 1 3 1 2 2 4 4 6 2 1 1 1 1 1 1 1 1 3 1 2 2 2 2 2 2 3 1 2 2 2 1 1 2 2 2 1 1 2 2 4 4 6 2 2 2 2 2 3 1 2 2 3 1 3 1 2 2 2 2 4 4 3 1 2 2 4 4)) 

((DOMINICAN-REPUBLIC "4/4" ("himno nacional") :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 36) 
(4 4 4 12 9 3 12 9 3 24 12 9 3 12 4 4 4 12 4 4 4 36 4 4 4 12 9 3 12 9 3 24 12 9 3 12 4 4 4 12 4 4 4 36 9 3 12 9 3 12 9 3 24 12 9 3 12 9 3 12 9 3 36 9 3 12 9 3 12 9 3 24 12 9 3 12 9 3 12 9 3 180 9 3 12 9 3 12 9 3 24 12 9 3 12 9 3 12 9 3 24 18 6 12 9 3 12 9 3 18 6 12 9 3 12 6 6 12 9 3 36 4 4 4 12 9 3 6 6 6 6 24 12 9 3 12 6 6 12 6 6 36 9 3 12 9 3 12 9 3 24 12 9 3 12 9 3 12 9 3)) 

((ECUADOR "4/4" ("hino nacianal") :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 36) 
(9 3 12 9 3 12 4 4 4 24 12 9 3 36 9 3 36 9 3 12 9 3 12 4 4 4 24 12 9 3 12 9 3 12 4 4 4 24 12 9 3 12 4 4 4 12 9 3 24 12 9 3 12 4 4 4 12 9 3 24 12 9 3 12 9 3 12 9 3 24 12 9 3 12 9 3 12 9 3 36 9 3 12 9 3 9 3 9 3 24 12 9 3 12 9 3 12 9 3 12 12 12 9 3 9 3 9 3 9 3 9 3 24 12 9 3 12 9 3 12 9 3 36 9 3 12 9 3 12 9 3 9 3 9 3 12 9 3 9 3 9 3 9 3 9 3 9 3 9 3 12 9 3 12 9 3 9 3 9 3 18 6 12 9 3 12 9 3 9 3 9 3 48 12 9 3 9 3 9 3 48 12 9 3 9 3 9 3)) 

((EIRE "4/4" ("Amhran na bhFiam") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 6) 
(2 2 1 1 3 1 1 1 1 1 3 1 2 2 2 2 3 1 2 2 2 1 1 3 1 1 1 1 1 3 1 3 1 3 1 2 4 2 3 1 1 1 1 1 3 1 2 2 3 1 2 2 2 2 2 2 2 1 1 3 1 1 1 1 1 2 1 1 1 2 1 3 1 8 3 1 2 2 6 2 3 1 2 2 4 4 4 4 6 1 1 3 1 2 2 8 3 1 2 2 6 2 3 1 2 2 4 2 2 4 3 1 2 2 3 1 6 2 3 1 1 1 1 1 3 1 2 2 3 1 2 2 2 2 2 2 2 1 1 3 1 1 1 1 1 2 1 1 1 2 1 3 1)) 

((EL-SALVADOR "4/4" ("himno nacional") :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 24) 
(12 12 12 9 3 12 9 3 12 12 12 9 3 4 4 4 9 3 12 9 3 4 4 4 12 12 12 12 9 3 12 9 3 12 12 12 9 3 12 9 3 12 4 4 4 120 12 12 12 9 3 12 9 3 12 12 12 9 3 4 4 4 9 3 12 9 3 4 4 4 12 12 12 12 9 3 12 9 3 12 12 12 9 3 8 4 8 4 8 4 4 4 4 60 12 12 12 60 12 12 12 33 3 9 3 33 3 9 3 132 9 3 24 8 4 4 4 4 24 12 4 4 4 9 3 20 4 4 6 2 12 24 9 3 24 4 4 4 4 4 4 24 12 4 4 4 16 4 4 16 4 4 36 9 3 9 3 9 3 9 3 9 3 12 24 9 3 12 9 3 12 9 3 12 9 3 12 9 3 9 3 9 3 9 3 9 3 9 3 24 9 3 16 4 4 12 4 4 4 36 9 3 24 4 4 4 4 4 4 24 12 9 3 24 4 4 4 4 4 4 36 9 3 24 4 4 4 4 4 4 24 12 9 3 16 4 4 16 4 4 16 4 4 16 4 4 16 4 4 16 4 4)) 

((ETHIOPIA "2/4" ("hymne national") :QUARTER-NOTE 4 :BAR-DURATION 8 :START-AT 0) 
(4 3 1 4 4 6 2 4 4 4 3 1 4 4 6 2 3 1 4 1 7 3 1 2 2 4 3 1 4 3 1 8 3 1 2 2 4 3 1 8 8 8 4 4 2 2 2 2 2 2 4 8 8 4 3 1 4 4 4 3 1 3 1 3 1 4 8 3 1 6 1 1 4 3 1)) 

((FAROE-ISLANDS "4/4" ("Tu alfagra land mitt") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 14) 
(2 4 3 1 4 3 1 4 3 1 6 2 4 3 1 4 3 1 4 3 1 6 2 4 3 1 4 3 1 4 3 1 6 2 4 3 1 4 3 1 4 3 1 6 2 4 2 2 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1)) 

((FINLAND "3/4" ("maamme" "Estonia, Eesti Humn") :QUARTER-NOTE 4 :BAR-DURATION 12 :START-AT 6) 
(2 2 2 6 2 3 1 8 4 3 1 4 4 8 4 3 1 2 2 2 2 2 2 4 4 3 1 2 2 2 2 2 2 6 2 3 1 2 2 2 2 8 4 3 1 4)) 

((FRANCE "4/4" ("la marseillaise") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 11) 
(1 3 1 4 4 4 4 6 2 3 1 3 1 4 8 3 1 11 1 3 1 4 4 4 3 1 4 8 3 1 4 4 4 3 1 12 3 1 4 3 1 4 3 1 11 1 3 1 4 4 4 3 1 4 4 8 4 3 1 4 3 1 14 2 6 2 2 2 2 2 12 2 2 6 2 2 2 2 2 4 11 1 11 1 3 1 15 1 11 1 3 1 12 4 12 4 16 8 4 4 12 4 11 1 3 1)) 

((GERMANY "4/4" ("deutschland lied") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 4)
 (3 1 2 2 2 2 1 1 2 2 2 4 3 1 2 2 2 2 1 1 2 2 2 2 2 2 1 1 4 2 2 1 1 2 2 2 1 1 2 2 2 3 1 2 1 1 4 3 1 1 1 2 3))

((GHANA "12/8" ("Lift high the flag of Ghana") :QUARTER-NOTE 2 :BAR-DURATION 12 :START-AT 0) 
(3 2 1 3 2 1 6 5 1 3 3 3 3 3 2 1 6 3 2 1 3 2 1 6 5 1 3 3 1 1 1 1 1 1 6 3 1 1 1 3 2 1 3 2 1 3 2 1 3 1 1 1 3 2 1 3 2 1 3 2 1 5 1 3 3 3 3 2 1 2 1 2 1 2 1 3 3 1 1 1 1 1 1 12)) 

((GREAT-BRITAIN "3/4" ("God Save The Queen" "Northern Ireland, God Save The Queen" "Switzerland, Rufst du, mein Vaterland" "Lichtenstein, Oben am Deutschen Rein") :QUARTER-NOTE 2 :BAR-DURATION 6 :START-AT 0) 
(2 2 2 3 1 2 2 2 2 3 1 2 2 2 2 6 2 2 2 3 1 2 2 2 2 3 1 2 2 1 1 1 1 3 1 2 1 1 2 2)) 

((GREECE "3/4" ("Hymne National") :QUARTER-NOTE 4 :BAR-DURATION 12 :START-AT 8) 
(3 1 7 1 3 1 4 4 3 1 7 1 3 1 8 3 1 7 1 3 1 4 4 3 1 7 1 3 1 8 3 1 7 1 3 1 4 4 3 1 7 1 3 1 8 3 1 4 4 3 1 4 4 3 1 4 3 1 3 1 8 3 1 4 4 3 1 4 4 3 1 7 1 3 1 4 4 3 1 4 4 3 1 4 4 3 1 7 1 3 1)) 

((GREENLAND "4/4" ("NangmimeK Erinalik") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 14) 
(2 4 3 1 4 3 1 4 3 1 6 2 4 3 1 3 1 2 2 8 6 2 4 3 1 4 3 1 3 1 3 1 6 2 3 1 2 2 3 1 2 2 4 4)) 

((GUATEMALA "4/4" ("Himno Nacional") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT -4) 
(2 2 4 2 2 4 2 2 6 2 4 2 2 4 2 2 4 2 2 4 2 2 4 2 2 4 2 2 4 2 2 6 2 4 2 2 4 2 2 4 2 2 4 6 2 2 2 4 2 2 4 2 2 6 2 4 2 2 4 2 2 2 2 2 2 12 2 2 4 2 2 4 2 2 6 2 4 2 2 4 2 2 4 2 2 4 6 2 2 2 6 2 4 2 2 4 2 2 2 2 2 2 12 2 2 4 2 2 4 2 2 8 4 2 2 4 2 2 4 2 2 12 2 2 4 2 2 4 2 2 6 2 4 2 2 4 2 2 4 2 2)) 

((GUINEA "2/4" ("Liberte") :QUARTER-NOTE 12 :BAR-DURATION 24 :START-AT 0) 
(12 6 6 24 4 4 4 9 3 24 12 6 6 24 4 4 4 4 4 4 12 3 3 3 3 12 6 6 12 6 6 6 6 12 4 4 4 9 3 24 12 12 12 6 6 12 12 12 6 6 12 12 12 12 24 24 2 2 2 2 2 2 2 2 2 2 2 2 12 3 3 3 3 24 24 2 2 2 2 2 2 2 2 2 2 2 2 12 3 3 3 3 12 6 6 24 4 4 4 9 3 12 12 3 3 3 3 12 12)) 

((HAITI "4/4" ("La Dessalinienne") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 11) 
(1 3 1 7 1 4 4 6 2 2 2 2 2 4 4 6 2 12 3 1 4 4 7 1 4 8 3 1 7 1 4 4 8 4 4 7 1 4 4 7 1 2 2 2 2 4 4 6 2 4 8 4 7 1 6 2 6 2 3 1 3 1 4 4 4 4 8)) 

((HONDURAS "4/4" ("Himno Nacional") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(3 1 6 2 4 3 1 6 2 4 3 1 4 4 4 4 4 8 3 1 6 2 4 3 1 6 2 4 2 2 4 4 4 4 4 8 3 1 4 3 1 4 3 1 4 6 2 2 2 4 3 1 3 1 3 1 10 2 2 2 4 2 2 2 2 2 2 4 8 2 2 2 2 2 2 2 2 2 2 12 3 1 6 2 4 3 1 6 2 4 3 1 4 4 4 4 4 8 3 1 6 2 4 3 1 6 2 4 2 2 4 4 4 4 12 3 1 4 2 2 4 2 2 4 8 2 2 4 2 2 4 2 2 4 6 2 2 2 3 1 2 2 3 1 2 2 4 6 2 2 2 3 1 2 2 2 1 1 2 2 10 2 2 2 4 2 2 2 2 1 1 1 1 4 6 2 2 2 4 2 1 1 2 2 2 2 4 8 4 8 4 4 12 2 2 4 4 4 4 4 8 3 1 4 2 2 4 2 2)) 

((HUNGARY "4/4" ("Himnusz") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 0) 
(1 1 1 1 2 1 1 1 1 1 1 2 2 5 1 1 1 1 1 1 1 4 3 1 2 2 2 2 4 2 2 2 2 2 2 4 3 1 2 2 2 2 4 2 2 2 2 2 2 4 3 1 2 2 2 2 4 3 1 2 2 2 2 4 3 1 2 2 3 1 2 2 2 2 2 1 1)) 

((ICELAND "4/4" ("Lofsögur") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 6) 
(2 2 2 3 1 2 2 3 1 1 1 1 1 2 1 1 1 1 1 1 3 1 2 1 1 2 1 1 1 1 1 1 2 1 1 2 1 1 2 1 1 6 1 1 2 1 1 2 1 1 2 1 1 3 1 2 1 1 2 1 1 7 1 2 1 1 2 1 1 2 1 1 3 1 2 1 1 2 1 1 6 1 1 2 2 2 1 1 2 2 3 1 2 1 1 2 1 1 2 1 1 3 1 2 1 1 2 1 1 4)) 

((INDIA "4/4" ("Jana Gana Mana" "gracenotes removed") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 0) 
(1 1 1 1 1 1 1 1 2 1 1 1 1 2 2 1 1 2 1 1 1 1 4 2 1 1 1 2 1 1 2 1 1 1 1 1 2 2 1 1 2 1 1 1 1 6 2 1 1 2 1 1 1 1 1 1 2 2 2 1 1 1 1 1 1 1 1 6 1 1 1 1 2 2 1 1 6 1 1 1 1 2 1 1 1 1 6 2 2 1 1 2 2 1 1 4 1 1 1 1 2 1 1 2 1 1 1 1 2 2 1 1 2 1 1 1 1 4 1 1 6 1 1 6 1 1 8 1 1 1 1 1 1 1 1 8 1 1 1 1 1 1 1 1 2 1 1 2 1 1 1 1 1 1 1 2 1 1 1 6 2 1 2 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 2 1 1 6 2 1 1 2 1 1 1 1 6 1 1 2 2 1 1 1 1 6 2 1 2 1 2 1 1 6 1 1 1 1 2 1 1 2 1 1 1 1 2 2 1 1 2 1 1 1 1 4 1 1 6 1 1 6 1 1 8 1 1 1 1 1 1 1 1 8 2 1 1 2 1 1 1 1)) 

((INDONESIA "4/4" ("indonesia raya") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(3 1 4 7 1 3 1 4 8 3 1 4 4 4 4 12 3 1 4 4 3 1 3 1 4 8 3 1 4 4 4 4 12 3 1 4 7 1 3 1 4 8 3 1 4 4 4 4 8 4 3 1 4 4 4 4 8 4 3 1 4 4 4 4 12 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 8 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 4 4 3 1 12 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 8 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1)) 

((IRAN "4/4" ("Imperial Salute") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(4 6 2 4 4 6 2 4 4 6 2 2 2 2 2 4 3 1 4 4 6 2 4 4 6 2 4 4 6 2 2 2 2 2 4 3 1 8 4 3 1 8 2 2 2 2 8 4 3 1 8 2 2 2 2 8 4 3 1 8 2 2 2 2 6 1 1 4 4 6 1 1 2 2 2 2)) 

((IRAQ "4/4" ("") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(4 4 3 1 4 3 1 4 3 1 4 3 1 12 4 4 3 1 4 3)) 

((ISLE-OF-MAN "3/4" ("Manx national anthem") :QUARTER-NOTE 4 :BAR-DURATION 12 :START-AT 8) 
(3 1 4 4 4 8 3 1 4 4 4 8 3 1 4 4 4 4 4 4 8 3 1 4 4 4 8 3 1 4 4 4 8 3 1 4 2 2 2 2 4 4 4)) 

((ISRAEL "4/4" ("Hatikvah") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 0) 
(2 2 2 2 4 4 2 2 2 2 8 4 2 2 4 4 2 2 2 2 6 2 2 2 2 2 4 4 2 2 2 2 8 4 2 2 4 4 2 2 2 2 8 4 4 4 4 2 2 2 2 8 4 4 4 4 2 2 2 2 8 4 2 2 4 4 2 2 2 2 4 2 2 4 4 4 3 1 2 2 2 2 8 4 2 2 4 4 2 2 2 2 4 2 2 4 4 4 3 1 2 2 2 2)) 

((ITALY "4/4" ("Inno di Mameli") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(4 3 1 8 4 3 1 8 4 3 1 8 4 3 1 8 4 3 1 8 4 3 1 8 4 3 1 8 4 3 1 8 4 3 1 8 4 4 8 3 1 3 1 8 4 3 1 8 4 8 4 4 3 1 8 4 3 1 8 4 3 1 6 2 2 2 39 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 7 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 7 1 4 3 1 4 3 1 4 3 1 4 3 1 3 1 3 1 4 3 1 4 3 1 2)) 

((JAPAN "4/4" ("Kimi ga jo") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 0) 
(2 2 2 2 2 2 4 2 2 2 1 1 2 2 2 2 2 2 4 2 2 2 2 2 2 2 2 3 1 4 2 2 2 2 2 2 2 2 2 1 1))

((JOHORE "4/4" ("Lago Bangsa Johore") :QUARTER-NOTE 8 :BAR-DURATION 32 :START-AT 0) 
(8 4 4 4 4 4 4 8 16 8 12 4 6 2 2 2 2 2 12 4 4 4 4 4 4 4 4 4 4 4 4 4 32 8 6 2 8 4 4 8 8 12 4 4 4 4 4 12 3 1 8 6 2 12 4 4 2 2 6 2 4 4 4 4 4 2 2 2 2 4 16 4 4 2 2 4 16)) 

((JORDAN "4/4" ("Ashaal Maleek") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 0) 
(4 3 1 8 2 2 2 2 2 2 4 2 1 1 2 1 1 2 1 1 2 1 1 4 4 1)) 

((KENYA "4/4" ("Land of the lion") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 0) 
(8 8 4 3 1 4 4 8 4 4 4 3 1 8 8 8 4 3 1 4 4 8 8 4 3 1 4 4 8 8 4 3 1 8 8 8 4 3 1 8 8 8 4 3 1 4 4 8 8)) 

((KOREA "4/4" ("Tong-hai Mool Kwa") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 0) 
(2 3 1 2 2 2 2 2 2 1 1 2 2 8 2 3 1 2 2 2 2 2 2 1 1 3 1 8 3 1 2 2 2 1 1 2 1 1 2 2 1 1 2 8 3 1 2 2 3 1 2 2 2 2 2 2 8 3 1 2 1 1 2 1 1 2 2 2 1 1 1 1 2)) 

((KUWAIT "2/4" ("") :QUARTER-NOTE 4 :BAR-DURATION 8 :START-AT 4) 
(3 1 12 3 1 12 3 1 4 3 1 4 3 1 4 3 1 4 3 1 12 3 1 12 3 1 4 3 1 4 3 1 4 3 1)) 

((LAOS "2/4" ("Hymne National") :QUARTER-NOTE 4 :BAR-DURATION 8 :START-AT 6) 
(2 3 1 2 2 3 1 2 2 3 1 2 2 6 2 3 1 2 2 3 1 2 2 2 2 1 1 2 6 2 3 1 2 2 6 2 3 1 2 2 6 2 2 2 2 2 3 1 2 2 2 2 2 2 6 2 3 1 2 2 3 1 2 2 2 2 2 2 6 1 1 3 1 2 2 3 1 2 2 2 2 2 2 6 1 1 3 1 2 2 3 1 2 2 3 1 2 2)) 

((LATVIA "4/4" ("Nacionala Himna") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 0) 
(4 2 2 3 1 4 4 2 2 3 1 4 4 2 2 3 1 2 2 4 2 2 8 4 2 2 3 1 4 4 2 2 3 1 4 4 2 2 3 1 2 2 4 2 2)) 

((LEBANON "4/4" ("Hymne National") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(3 1 12 3 1 4 3 1 4 3 1 12 3 1 12 3 1 4 3 1 4 3 1 12 3 1 4 3 1 2 2 3 1 4 3 1 4 3 1 4 3 1 2 2 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 12 3 1))
((LIBERIA "4/4" ("L'inno Nazionale") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 6) 
(2 3 1 2 2 6 2 3 1 2 2 6 2 3 1 2 2 3 1 2 2 4 4 6 2 2 2 2 2 2 2 2 2 2 2 2 2 38 2 2 2 2 2)) 

((LIBYA "4/4" ("Ya Biladi") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(3 1 8 4 3 1 8 4 3 1 3 1 3 1 8 4 3 1 2 2 3 1 8 4 3 1 8 4 3 1 12 3 1 12 3 1 4 2 2 2 2 2 2 4 3 1 2 2 3 1 4 4 4 4 4 4 4 3 1 2 2 3 1 2 2 3 1 12 2 2 2 2 3 1 2 2 3 1 12 3 1 2 2 3 1 2 2 3 1 12 3 1 2 2 3 1 2 2 3 1 12 3 1 2 2 3 1 2 2 3 1 12 3 1 2 2 3 1 4 3 1 12 3 1 4 3 1 4 3 1 2 2 3 1 2 2 3 1 4 4 4 4)) 

((LITHUANIA "4/4" ("lietuvos Himnas") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 0) 
(3 1 2 2 2 2 2 2 3 1 2 2 2 2 4 3 1 2 2 2 2 2 2 3 1 2 2 4 4 3 1 2 2 3 1 4 3 1 2 2 3 1 4 2 2 4 2 2 4 3 1 2 2 2 2)) 

((LUXEMBOURG "4/4" ("Ons Hemecht") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 6) 
(2 3 1 2 2 2 2 3 1 2 1 1 3 1 6 2 3 1 3 1 2 2 3 1 2 2 3 1 6 2 3 1 2 2 2 2 3 1 2 2 2 2 6 2 2 1 1 2 2 3 1 2 2 3 1 2 2 6 2 2 1 1 2 2 3 1 2 2 3 1 2 2)) 

((MALAYA "4/4" ("Negara Ku") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 2) 
(2 2 2 6 2 2 2 2 2 3 1 6 2 2 2 6 2 2 2 3 1 10 2 2 2 6 2 3 1 2 2 10 2 2 2 6 2 2 2 3 1 10 2 2 2 6 2 3 1 2 2 10 2 2 2 6 2 2 2 3 1)) 

((MALTA "4/4" ("Innu Malti") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 0) 
(4 2 2 2 4 1 1 4 1 1 1 1 2 6 4 2 2 4 2 2 3 1 1 1 1 1 4 4 4 2 2 4 2 2 3 1 1 1 1 1 4)) 

((MEXICO "2/2" ("Hymno nacional") :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 36) 
(9 3 12 9 3 12 4 4 4 24 12 9 3 12 9 3 12 9 3 12 24 9 3 12 9 3 12 9 3 12 24 9 3 12 9 3 12 4 4 4 12 24 9 3 12 9 3 12 9 3 18 6 12 9 3 12 9 3 12 4 4 4 12 24 9 3 12 6 6 6 6 6 6 24 12 9 3 9 3 6 6 6 6 9 3 21 3 12 9 3 12 6 6 6 6 6 6 6 6 24 6 6 6 3 3 6 6 12 9 3 36 9 3 12 9 3 12 9 3 12 24 9 3 9 3 6 6 6 6 9 3 21 3 12 9 3 6 12 6 6 12 6 18 6 12 9 3 6 6 6 6 12 9 3 12 12 12 12 6 6 6 6 12 9 3)) 

((MONACO "2/2" ("Hymne Monegasque") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 0) 
(4 3 1 4 3 1 4 3 1 4 4 4 3 1 4 3 1 3 1 3 1 8 4 3 1 4 3 1 4 3 1 4 4 4 3 1 6 2 3 1 3 1 8 4 3 1 6 2 3 1 3 1)) 

((MOROCCO "2/4" ("Hymne Cherifien") :QUARTER-NOTE 4 :BAR-DURATION 8 :START-AT 0) 
(3 1 2 1 1 8 3 1 2 1 1 8 3 1 2 2 1 1 1 1 4 1 1 1 1 12 3 1 2 1 1 8 3 1 3 1 8 3 1 2 1 1 8 2 1 1 2 1 9 2 1 1 4 2 1 1 2 2 1 1 1 1 10 2 3 1 2 2 1 1 1 1 1 1 1 1 1 1 6 3 1 2 1 1 8 3 1 3 1 8 3 1 2 1 1 8 2 1 1 2 1 9 8)) 

((NEPAL "4/4" ("National anthem for H.M. the Maharaja Dhiraja") :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 36) 
(12 24 24 18 6 12 12 9 3 12 9 3 12 12 9 3 12 12 24 24 18 6 12 12 9 3 9 3 9 3 9 3 12 9 3 12 12 18 6 9 3 9 3 18 6 12 12 12 9 3 12 9 3 12 9 3 12 12 24 24 24 24 12 4 4 4 24 24)) 

;;; Extended to make the rhythm long enough to be able to be compared against the bar
;;; duration. However, since there are meter changes across the piece, the bar-duration is
;;; not a reliable measure.
((NETHERLANDS "4/4" ("Wilhelmus van Nassouwe" "a") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 6) 
(2 2 2 1 1 1 1 2 1 1 2 2 1 1 2 6 2 2 2 1 1 1 1 2 1 1 2 2 1 1 2 6 1 1 4 2 4 2 2 1 1 2 2 2 2
 4 2 1 1 1 1 2 4 2 2 2 1 1 2 2 2 4))

;;; The alternative is to use the duple meter section which still has measures in 2/4 in
;; addition to 4/4
;; (2 2 2 1 1 1 1 2 1 1 2 2 1 1 2 6 2 2 2 1 1 1 1 2 1 1 2 2 1 1 2 6 1 1)) 

;;; The original
;;(2 2 2 1 1 1 1 2 1 1 2)) 

((NEWFOUNDLAND "4/4" ("When sunrays crown thy pineclad hills") :QUARTER-NOTE 2 :BAR-DURATION 8 :START-AT 6) 
(2 2 2 3 1 2 2 2 2 2 2 2 1 1 6 2 2 2 2 2 2 2 2 2 2 2 2 2 6 2 4 2 2 4 2 2 2 2 2 2)) 

((NEW-ZEALAND "4/4" ("God defend New Zealand") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 8) 
(4 4 6 2 4 4 8 4 4 4 4 4 4 8 6 2 2 2 2 2 4 2 2 4 4 4 4 4 4 4 4 8 4 8 2 2 4 4 8 4 4 4 4 2 2 2 2 8 6 2 4 4 4 4 8 6 2 4 2 2 4 4 8 4 4 4 4 4 3 1 8 4 4 4 4 2 2 2 2 8 4 4 4 4 4 4 8 4 4 4 4 4 4 8 4 4 4 4 4 2 2 8 4 4 2 2 2 2 2 2 2 2 8 4 4 4 4 4 4 8 4 4 4 4 4 4)) 

((NICARAGUA "4/4" ("Himno Nacional") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(2 1 1 4 3 1 4 3 1 6 2 8 4 4 4 3 1 3 1 3 1 4 1 1 1 1 4 3 1 4 3 1 6 2 4 3 1 3 1 3 1 3 1 3 1 2 2 2 2 2 2 2 2 4 3 1 2 2 3 1 4 3 1 2 2 2 2 12 3 1 3 1 3 1 3 1 3 1 2 2 2 2 2 2 2 2 4 3 1 4 3 1 4 3 1 2 2 2 2 4 1 1 1 1 9 1 1 1 1 1 1 1 10 2 2 2 2 2 2 2)) 

((NORWAY "4/4" ("Ja, vi elsker dette landet") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 0) 
(6 2 4 4 4 4 4 4 6 2 4 4 16 6 2 4 4 4 4 4 4 6 2 4 4 16 6 2 3 1 3 1 8 8 6 2 4 4 12 3 1 4 4 4 4 8 8 6 2 4 4 12 3 1 4 4 4 4 8 4 3 1 6 2 4 4)) 

((PAKISTAN "4/4" ("Pak sarzamin shadbad") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 0) 
(4 3 1 4 3 1 16 3 1 3 1 4 3 1 16 2 2 2 2 2 2 2 2 4 2 2 2 2 4 3 1 3 1 4 3 1 16 4 3 1 4 3 1 16 3 1 3 1 3 1 3 1 16 4 4 3 1 4 2 2 2 2 2 2 4 4 3 1 3 1 3 1 16 3 1 3 1 3 1 3 1 16 2 2 2 2 2 2 2 2 4 2 2 2 2 4 3 1 3 1 4 3 1)) 

((PANAMA "4/4" ("Himno Nacional") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(3 1 4 3 1 2 2 3 1 8 4 3 1 4 3 1 4 3 1 12 3 1 4 3 1 4 3 1 8 4 3 1 4 3 1 2 2 2 2 8 4 3 1 4 3 1 4 3 1 8 4 3 1 4 3 1 2 2 2 2 12 3 1 4 3 1 4 3 1 8 4 3 1 4 3 1 4 3 1 8 4 3 1 4 3 1 4 3 1 8 4 3 1 4 3 1 4 3 1 12 3 1 4 3 1 4 3 1 8 4 3 1 4 3 1 4 3 1 12 3 1 4 3 1 4 3 1 8 4 3 1 4 3 1 4 3 1)) 

((PARAGUAY "4/4" ("Himno Nacional") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 8) 
(4 4 8 4 4 8 4 4 8 4 4 8 4 4 4 4 4 4 4 4 4 4 24 4 4 8 4 4 8 4 4 12 4 8 4 4 4 4 4 4 6 2 2 2 2 2 24 4 4 4 4 4 4 4 4 4 4 8 4 4 8 4 4 4 4 4 4 4 4 4 4 24 4 4 4 4 4 4 4 4 4 4 16 4 4 4 4 4 4 4 4 4 4 4 4 16 8 4 4 4 4 4 4 4 4 4 4 16 4 4 4 4 4 4 4 4 4 4 4 4 28 3 1 4 3 1 4 2 2 2 2 8 3 1 4 3 1 2 2 2 2 2 2 8 3 1 4 3 1 4 3 1 4 8 3 1 2 2 2 2 2 2 2 2 8 4 3 1 4 3 1 4 3 1 4 8 3 1 2 2 2 2 2 2 2 2 6 2 2 2 2 2 6 2 2 2 2 2)) 

((PERU "4/4" ("Himno Nacional") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(3 1 8 7 1 8 7 1 4 3 1 4 3 1 4 8 3 1 8 7 1 8 7 1 4 3 1 4 3 1 8 4 3 1 4 3 1 2 2 2 2 4 8 3 1 4 3 1 2 2 2 2 4 8 3 1 4 3 1 2 2 2 2 4 6 2 2 2 4 3 1 2 2 2 2 12 3 1 4 3 1 2 2 2 2 4 6 2 2 2 4 3 1 2 2 2 2 12 3 1 4 3 1 2 2 2 2 6 2 4 3 1 4 3 1 2 2 2 2 12 3 1 4 3 1 4 3 1 6 2 4 3 1 2 10 3 1 2 10 3 1 4 3 1 4 3 1 12 3 1 4 3 1 2 2 3 1 2 10 3 1 4 3 1 2 2 3 1 2 10 3 1 4 3 1 2 2 3 1 8 4 3 1 2 10 3 1 2 10 3 1 4 3 1 4 3 1 10 2 2 2 4 3 1 2 2 2 2 6 2 2 2 3 1 12 3)) 

((PHILIPPINES "2/4" ("Lupang hinirang") :QUARTER-NOTE 12 :BAR-DURATION 24 :START-AT 0) 
(12 9 3 12 12 4 4 4 9 3 12 12 12 9 3 12 12 4 4 4 9 3 18 6 12 9 3 12 12 4 4 4 9 3 12 12 12 9 3 12 12 4 4 4 9 3 12 4 4 4 9 3 9 3 9 3 9 3 9 3 9 3 12 4 4 4 9 3 9 3 9 3 9 3 4 4 4 4 4 4 12 4 4 4 9 3 9 3 9 3 9 3 9 3 9 3 12 4 4 4 9 3 9 3 9 3 9 3 4 4 4 4 4 4 12 4 4 4 12 12 12 9 3 9 3 9 3 12 4 4 4 12 12 12 9 3 36 4 4 4 12 12 12 6 6 9 3 9 3 12 4 4 4 12 12 12 9 3)) 

((POLAND "3/4" ("Hymn Polski") :QUARTER-NOTE 4 :BAR-DURATION 12 :START-AT 0) 
(3 1 4 4 3 1 2 2 2 2 3 1 6 2 4 8 3 1 4 4 3 1 2 2 2 2 3 1 6 2 4 8 4 6 2 2 2 8 3 1 4 4 4 8 3 1 6 2 2 2 8 3 1 6 2 4)) 

((PORTUGAL "4/4" ("Himno Nacional a Portuguesa") :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 33) 
(3 9 3 24 21 3 12 12 9 3 9 3 24 9 3 9 3 36 9 3 24 12 6 6 24 12 9 3 30 6 6 6 48 18 6 12 12 18 6 12 6 6 18 6 6 12 6 12 30 6 12 12 12 6 6 42 6 12 9 3 12 6 6 21 3 21 3 24 21 3 24 12 6 6 12 12 12 12 45 3 24 21 3 24 16 4 4 24 21 3 30 6 6 6 12 12 18 6)) 

((QATAR "2/4" ("") :QUARTER-NOTE 4 :BAR-DURATION 8 :START-AT 6) 
(2 2 1 1 2 2 3 1 12 1 1 1 1 2 2 3 1 12 2 1 1 2 2 1 1 1 1 2 2 2 1 1 2 2)) 

((RUMANIA "4/4" ("Te sl~avim Romanie") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(2 2 4 2 2 4 2 2 4 3 1 4 3 1 4 2 2 4 2 2 4 3 1 4 3 1 4 2 2 4 2 2 4 3 1 4 2 2 4 2 2 4 2 2 4 2 2 4 2 2 2 2 2 2 4 2 2 4 2 2 4 4 8 4 4 3 1 4 4 4 6 2 4 4 4 8 4 6 2 4 4 6 2 4 2 2 4 4 4 4 4 4 4 4 10 2 2 2 3 1 4 4 2 2 10 2 3 1 8)) 

((SAN-MARINO "2/2" ("Onore a te onore") :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 0) 
(24 24 24 12 12 24 24 24 24 18 6 8 8 8 12 12 12 12 24 24 8 8 8 24 24 24 24 12 12 36 12 24 24 24 24 24 24 36 12 12 12 12 12 24 12 12 24 24 24 8 8 8 24 24 24 24 8 8 8 12 249 3 24 24 48 48)) 

((SAUDI-ARABIA "2/2" NIL :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 0) 
(24 18 2 2 2 9 3 6 6 24 9 3 6 6 6 6 6 6 12 9 3 24 12 3 3 3 3 12 6 6 18 2 2 2 24 12 3 3 3 3 6 6 6 6 18 3 3)) 

((SOUTH-AFRICA "4/4" ("De stem van Zuid Afrika") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(3 1 4 4 4 4 8 4 2 2 4 2 2 4 4 12 3 1 4 4 4 4 8 4 2 2 4 2 2 4 4 12 2 2 4 4 4 4 8 4 2 2 4 4 4 4 12 3 1 4 4 4 4 8 4 3 1 4 2 2 6 2 12 2 2 4 4 4 4 8 4 2 2 4 4 4 4 12 3 1 4 4 4 4 8 4 3 1 4 4 4 4)) 

((SPAIN "2/2" ("Himno Nacional") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 0) 
(4 4 4 2 2 2 2 2 2 2 2 2 2 4 4 6 2 2 2 2 2 4 4 3 1 4 3 1 4 3 1 2 2 2 2 4 4 3 1 2 2 4 4 8 4 4 4 2 2 2 2 2 2 2 2 2 2 4 4 6 2 2 2 2 2 8 4 3 1 4 3 1 4 3 1 2 2 2 2 4 4 3 1 2 2 4 4)) 

((SUDAN "4/4" ("Nahnu djundul'l^ah") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 0) 
(2 1 1 2 1 1 8 4 3 1 8 3 1 1 3 4 3 1 4 3 1 10 1 1 2 2 8 4 3 1 8 3 1 2 2 6 2 4 3 1 8 3 1 4 3 1 4 3 1 2 2 3 1 4 2 2 4 4 3 1 16 3 1 2 2 8 3 1 3 1 8 3 1 3 1 6 2 3 1 2 2)) 

((SURINAM "4/4" ("Het Surinaamse Volkslied") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(3 1 4 4 4 4 8 4 3 1 4 4 4 4 12 3 1 4 4 4 4 8 4 3 1 4 4 4 4 12 3 1 4 4 4 4 6 2 4 3 1 4 4 4 4 12 3 1 4 4 4 4 6 2 4 2 2 6 2 4 2 2)) 

((SWEDEN "4/4" ("Du gamla du fria") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 14) 
(2 4 2 2 4 2 2 4 2 2 6 2 4 2 2 2 2 3 1 8 6 2 4 2 2 4 2 2 3 1 2 2 6 2 3 1 2 2 2 2 3 1 8 6 2 3 1 2 2 2 2 3 1 8)) 

((SWITZERLAND "3/4" ("Swiss Psalm") :QUARTER-NOTE 4 :BAR-DURATION 12 :START-AT 0) 
(3 1 4 4 3 1 8 3 1 4 4 3 1 8 6 2 2 2 6 2 4 8 4 12 3 1 4 4 6 2 2 2 12 12 3 1 4 4 6 2 4 8 4 12 3 1 4 4 3 1 8 3 1 4 4 3 1 8 6 2 2 2 8 4 8 4 6 2 2 2 8 8 2 2 4)) 

((THAILAND "4/4" ("Sanrasoen Phra Barami") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 0) 
(8 2 2 2 2 16 6 2 2 2 2 2 16 2 2 2 2 4 1 1 1 1 16 6 2 2 2 2 2 8 2 2 2 2 6 2 2 2 4 16 6 2 2 2 2 1 1 16 6 2 2 2 4 16 6 2 2 2 2 2 8 6 2 8 2 2 2 2 12 3 1 8 8 2 1 1 2 2 8 2 2 2 2 6 2)) 

((TUNISIA "4/4" ("Ala Khallid^i Ya Dimanalghawali") :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 42) 
(6 12 9 3 12 9 3 12 4 4 4 12 9 3 12 9 3 18 6 12 9 3 12 9 3 12 4 4 4 12 9 3 12 9 3 18 6 12 9 3 12 9 3 12 9 3 18 6 12 9 3 18 6 12 9 3 18 6 12 9 3 18 6 12 9 3 18 6 12 9 3 18 6 12 9 3 18 6 12 9 3 12 9 3 12 9 3 18 6 12 9 3 12 9 3 12 4 4 4 18 6 12 9 3 12 9 3 12 9 3 18 6 12 9 3 12 9 3 12 4 4 4 18 6 12 9 3 12 9 3 12 9 3 18 6 12 9 3 12 9 3 12 9 3 18)) 

((TURKEY "4/4" ("Istikla^l Marsi") :QUARTER-NOTE 12 :BAR-DURATION 48 :START-AT 36) 
(12 12 12 12 9 3 36 12 12 12 9 3 9 3 36 4 4 4 12 21 3 9 3 9 3 9 3 9 3 9 3 12 4 4 4 12 12 36 4 4 4 12 12 9 3 4 4 4 12 21 3 9 3 9 3 9 3 12 12)) 

((UNITED-ARAB-REPUBLIC "4/4" ("Egypt" "Syria") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(2 1 1 4 3 1 4 2 2 2 2 2 2 4 1 1 1 1 4 3 1 4 3 1 2 2 2 2 4 2 1 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 1 1 1 1 6 1 1 4 4 4 4 4 2 1 1 6 1 1 4 4 4 4 4 2 1 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 4 3 1 7 1 4 3 1 4 3 1 4 3 1 7 1 4 3 1 4 3 1 4 3 1 7 1 4 3 1 4 3 1 4 3 1 7 1 4 3 1 4 3 1 4 3 1 7 1 4 3 1 4 3 1 4 3 1 7 1 4 3 1 4 3 1 4 3 1 7 1 4 3 1 4 3 1 4 3 1 7 1 4 3 1 4 3)) 

((URUGUAY "4/4" ("Himno Nacional") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 12) 
(3 1 4 3 1 3 1 3 1 4 8 3 1 4 3 1 4 3 1 12 3 1 4 3 1 3 1 3 1 4 8 3 1 4 3 1 4 3 1 12 3 1 3 1 3 1 3 1 3 1 6 2 4 3 1 3 1 3 1 3 1 3 1 8 6 2 4 2 2 4 2 2 4 4 6 2 4 2 2 4 2 2 8 4 4 8 4 4 8 6 2 4 2 2 4 2 2 4 4 6 2 4 2 2 4 2 2 8 4 4 8 4 4 20 4 4 3 1 20 4 4 3 1 4 4 4 3 1 44 3 1 4 3 1 4 3 1 6 1 1 4 3 1 4 3 1 4 3 1 12 3 1 4 3 1 4 3 1 4 8 3 1 4 3 1 4 3 1 12 3 1 6 2 2 2 2 2 6 2 4 3 1 6 2 2 2 2 2 6 2 4 2 2 6 2 4 2 2 6 2 4 2 2 4 8 3 1 3 1 3 1 3 1 3 1 6 2 4 3 1 4 2 2 2 1 1 2 2 12 3 1 3 1 3 1 3 1 3 1 4 2 2 2 1 1 2 2 12 3 1 3 1 3 1 3 1 3 1 6 1 1 6 1 1 6 1 1 6 1 1 6 1 1 6 1 1 6 1 1 6 1 1 20 4 4 3 1 20 4 4 3 1 44 3)) 

((USSR "4/4" ("Soyus nero'ushimyi") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 6) 
(4 4 2 4 3 1 4 2 2 4 3 1 4 3 1 4 3 1 4 2 2 4 3 1 6 2 4 3 1 4 2 2 4 3 1 4 2 2 4 3 1 4 3 1 4 3 1 8 8 2 2 2 2 6 2 8 8 2 2 2 2 6 2 8 4 3 1 4 3 1 4 3 1 8 8 4 2 2 6 2 8 8 4 2 2 6 2 8 4 2 2 4 3 1 8 4 4)) 

((VATICAN "4/4" ("Marcia Pontificale" "first 18 bars") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 0) 
(8 7 1 8 7 1 4 4 4 4 6 2 8 8 4 3 1 4 4 4 4 7 1 8 8 7 1 8 7 1 4 4 4 4 7 1 8 8 7 1 8 7 1 4 4 4 4 7 1 8 8 7 1 12 2 2 4 2 2 4)) 

((VENEZUELA "2/4" ("Himno Nacional") :QUARTER-NOTE 4 :BAR-DURATION 8 :START-AT 0) 
(2 2 2 2 4 2 2 4 2 2 6 2 2 2 3 1 4 4 2 2 3 1 6 2 4 3 1 4 3 1 2 2 2 2 6 1 1 2 2 2 2 4 4 2 2 2 2 8 2 2 2 2 4 4 2 2 2 2 6 2 4 3 1 4 4 2 2 2 2 6 2 2 2 2 2 4 4 2 2 2 2 8 2 2 2 2 4 4 2 2 2 2 8 2 2 2 2 4 4 2 2 2 1 1)) 

((VIETNAM "4/4" ("Qu^oc Thi^e'u Vi^et-Nam") :QUARTER-NOTE 4 :BAR-DURATION 16 :START-AT 11) 
(1 3 1 6 2 3 1 3 1 11 1 3 1 4 4 3 1 3 1 12 4 4 3 1 4 4 4 3 1 4 4 4 3 1 4 4 4 3 1 7 1 4 4 4 3 1 8 4 4 6 2 4 3 1 12 4 6 2 4 4 6 2 4 4 6 2 4 4 6 2 4 4 4 3 1 4 3 1 8 4 4 4 3 1 4 3 1 8 4 3 1 11 1 3 1 8 4 3 1 11 1 3 1 8 4 4 6 2 4 4 6 2 4 4 4 4 3 1 3 1)) 

((WALES "3/4" ("Mae Hen Wlad fy Nhadau") :QUARTER-NOTE 2 :BAR-DURATION 6 :START-AT 4) 
(2 2 2 2 2 2 2 2 2 1 1 4 2 2 2 2 2 2 2 2 2 2 4 2 2 2 1 1 2 2 2 2 2 1 1 4 2 2 2 2 2 2 2 12 12 2 2 2 2 2 2 4 2 4 1 1 4 2 4 1 1 4 2 2 2 2 2 2 2)) 

((YEMEN "2/4" ("Salimta Imaman" "removed gracenotes") :QUARTER-NOTE 12 :BAR-DURATION 24 :START-AT 18) 
(3 3 9 3 9 3 12 6 3 3 9 3 9 3 12 6 3 3 9 3 9 3 12 6 3 3 6 3 3 3 3 3 3 24 12 12 12 6 6 9 3 9 3 9 3 12 6 3 3 9 3 9 3 4 4 4 9 3 3 3 3 3 24 6 3 3 6 6 3 3 3 3 9 3 6 12 6 3 3 3 3 12 12 6 6 6 6 3 3 3 3 6 3 3 6 6 6 6 12 12 12 12 6 6 12 9 3 24 12 6 6 6 6 6 6 12 6 6 24 12 12 12 6 6 12 6 6 24 12 6 6 6 6 6 6 12 6 6)) 

((YUGOSLAVIA "3/4" ("Hej Slaveni") :QUARTER-NOTE 4 :BAR-DURATION 12 :START-AT 0) 
(3 1 4 4 2 2 2 2 2 2 2 2 6 2 4 8 3 1 4 4 2 2 2 2 2 2 2 2 6 2 4 8 3 1 4 4 3 1 4 4 3 1 4 4 4 8 3 1 2 2 2 2 3 1 2 2 2 2 2 2 6 2 4)) 
))
