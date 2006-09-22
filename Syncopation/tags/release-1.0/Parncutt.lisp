;;; Parncutt's Pulse Salience examples
(defparameter parncutt-pulse '(1))
(defparameter parncutt-waltz '(2 1))
(defparameter parncutt-march '(2 1 1))
(defparameter parncutt-swing '(3 2 1))
(defparameter parncutt-skip '(3 1 2))
(defparameter parncutt-cross '(2 1 1 2))

(play-rhythm (repeat-rhythm parncutt-swing 5) :tempo 150)
