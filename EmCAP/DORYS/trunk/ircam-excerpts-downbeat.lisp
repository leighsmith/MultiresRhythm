;;; Holds the ground truth for the anacrusis (number of beats to shift to find the
;;; downbeat) or each example.
(defparameter *ircam-downbeats*
  '(("rwc_p_81_excerpt" :anacrusis 1)
    ("rwc_p_82_excerpt" :anacrusis 0) ; There is an anacrusis which IRCAM beat skips
    ("rwc_p_83_excerpt" :anacrusis 0)
    ("rwc_p_84_excerpt" :anacrusis 0)
;    ("rwc_p_85_excerpt" :anacrusis 0) ; This isn't correct, off by a quaver
    ("rwc_p_86_excerpt" :anacrusis 0)
    ("rwc_p_87_excerpt" :anacrusis 0)
    ("rwc_p_88_excerpt" :anacrusis 3)
    ("rwc_p_89_excerpt" :anacrusis 3)
    ("rwc_p_90_excerpt" :anacrusis 1)
    ("rwc_p_91_excerpt" :anacrusis 0)
    ("rwc_p_92_excerpt" :anacrusis 2)
    ("rwc_p_93_excerpt" :anacrusis 0)
    ("rwc_p_94_excerpt" :anacrusis 0)
;    ("rwc_p_95_excerpt" :anacrusis 2) ;This is hard to identify, it seems to change.
    ("rwc_p_96_excerpt" :anacrusis 0)
    ("rwc_p_97_excerpt" :anacrusis 0)
    ("rwc_p_98_excerpt" :anacrusis 1)
    ("rwc_p_99_excerpt" :anacrusis 1)
    ("rwc_p_100_excerpt" :anacrusis 3)))

