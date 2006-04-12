load "pk24_musician_perceptual_hierarchy.text"
load "pk44_musician_perceptual_hierarchy.text"
plot(pk24_new, "-@", pk24, "", pk44)
pk24_double = [pk24_new; pk24_new]
plot(pk24_double)
