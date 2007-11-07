(in-package :shoe)

(defun make-patterns-tree-from-file (file 
                                     &key 
                                     (mode :gcd-multiple)  ; :sequential-ratio :absolute
                                     (order :tree-size)) ; value
  (with-open-file (stream file)
    (loop with tree = (empty-tree)
          as input = (read stream nil nil)
          while input
          as (name pattern) = input
          do (setf tree
                   (add-pattern-tree (normalize pattern mode name)
                                     tree
                                     (normalise-name name pattern mode)))
          finally (return (sort-tree tree :order order)))))

(defun make-patterns-tree-in-file (file 
                           to
                           &key (mode :gcd-multiple)  ; :sequential-ratio
                           (order :tree-size)) ; value
  (with-open-file (out to :direction :output :if-exists :supersede)
    (with-open-file (stream file)
      (loop with tree = (empty-tree)
            as input = (read stream nil nil)
            while input
            as (name pattern) = input
            do (setf tree
                     (add-pattern-tree (normalize pattern mode)
                                       tree
                                       (normalise-name name pattern mode)))
            finally (pprint (sort-tree tree :order order)
                            out)))))

(defun search-tree (tree fun)
  (if (eql (first tree) '-)
    (loop for (item sub) in (rest tree)
          thereis (search-tree sub fun))
    (apply fun (second tree))))

(defun count-tree (tree fun)
  (if (eql (first tree) '-)
    (loop for (item sub) in (rest tree)
          sum (count-tree sub fun))
    (if (apply fun (second tree))
      1
      0)))

(defun collect-tree (tree fun)
  (if (eql (first tree) '-)
    (loop for (item sub) in (rest tree)
          append (collect-tree sub fun))
    (when (second tree)
      (list (apply fun (second tree))))))

(defun there-is-tree (tree fun)
  (if (eql (first tree) '-)
    (loop for (item sub) in (rest tree)
          thereis (there-is-tree sub fun))
    (and (second tree)
         (apply fun (second tree)))))

(defun tree-leafs (tree)
  (if (eql (first tree) '-)
    (loop for (item sub) in (rest tree)
          sum (tree-leaves sub))
    1))

(defun select-tree (tree pattern)
  (cond ((null pattern) tree)
        ((eql (first tree) '-)
         (loop for (item sub) in (rest tree)
	    when (eql item (first pattern))
	    do (return (select-tree sub (rest pattern)))))
	((not (mismatch (first tree) pattern)) tree)
        (t nil)))

;; Assumes the pattern is not "normalized".
(defun find-terminals (tree pattern fun &key (mode :gcd-multiple))
  (let ((sub (if pattern (select-tree tree (normalize pattern mode)) tree)))
    ;; (format t "find-terminals sub = ~a~%" sub)
    (search-tree sub fun)))

;(correct-tree-beat? *anthem-tree* '() 0 1)

(defun collect-terminals (tree pattern fun &key (mode :gcd-multiple))
  (let ((sub (select-tree tree (normalize pattern mode))))
    (collect-tree sub fun)))

(defun there-is-terminals (tree pattern fun &key (mode :gcd-multiple))
  (let ((sub (select-tree tree (normalize pattern mode))))
    (there-is-tree sub fun)))

(defun count-terminals (tree pattern fun &key (mode :gcd-multiple))
  (let ((sub (select-tree tree (normalize pattern mode))))
    (list (count-tree sub fun)
          (tree-leafs sub))))
          
#|
(defun find-time-sigs (tree pattern &key (mode :gcd-multiple))
  (find-terminals *result* pattern
                  #'(lambda (country time-sig name &key bar-duration start-at)
                      (print (list country time-sig 
                                   :bar-duration (* (first pattern) bar-duration)
                                   :start-at (* (first pattern) start-at)))
                      nil) ; continue search
                  :mode :sequential-ratio))
|#

(defun correct-tree-beat? (tree pattern phase dur 
                                &optional allow-multiples (allow-sub-beats t))
  (find-terminals tree pattern
                  (lambda (country time-sig name &key bar-duration start-at)
		    ;; The first element of the pattern is the normalizing divisor
		    ;; calculated by normalize :sequential-ratio.
		    (format t "in lambda, from tree: country ~a time-sig ~a name ~a bar duration ~a start at ~a first pattern ~a~%pattern ~a~%"
			    country time-sig name bar-duration start-at (first pattern) pattern)
		    (format t "testing against: phase ~a dur ~a~%" phase dur)
		    (when (or
			   (and allow-multiples	; accept beat multiples
				(> dur (* (first pattern) bar-duration))
				(zerop (mod dur (* (first pattern) bar-duration)))
				(= (mod phase (* (first pattern) bar-duration))
				   (* (first pattern) start-at)))
			   (member (list phase dur)
				   (fitting-beats time-sig 
						  (* (first pattern) bar-duration)
						  (* (first pattern) start-at)
						  :allow-sub-beats allow-sub-beats)
				   :test #'equal))
		      country))
                  :mode :sequential-ratio))


(defun count-tree-beat? (tree pattern phase dur)
  ; returns (n out-of m) with correct beat
  (count-terminals tree pattern
                   #'(lambda (country time-sig name &key bar-duration start-at)
                       (and (member (list phase dur)
                                    (fitting-beats time-sig 
                                                   (* (first pattern) bar-duration)
                                                   (* (first pattern) start-at))
                                    :test #'equal)
                            country))
                   :mode :sequential-ratio))



(defun new-level-tree-beat? (tree pattern phase dur max &optional multiples)
  ; returns list of correct beat levels
  ; 0 = bar
  ; 1 = multiple bar
  ;(print (list 'new-level-tree-beat? pattern phase dur max multiples '->))
  (let ((results
         (delete nil 
                 (collect-terminals tree pattern
                                    #'(lambda (country time-sig name &key bar-duration start-at)
                                        ;(print country)
                                        (if (> dur bar-duration)
                                          (when (and (zerop (mod dur bar-duration))
                                                     (= (mod (- bar-duration
                                                                (mod phase bar-duration))
                                                             bar-duration)
                                                        start-at))
                                            1)
                                          (new-beat-level dur phase bar-duration start-at
                                                          (TIME-SIG-DIVS time-sig))))
                                    :mode :absolute))))
    ;(print results)
    results))

(defun new-correct-tree-beat? (tree pattern phase dur max &optional multiples sub-beats)
  ; returns list of correct beat levels
  ; 0 = bar
  ; 1 = multiple bar
  ;(print (list 'new-correct-tree-beat? pattern phase dur max multiples '->))
  (let ((results
         (there-is-terminals tree pattern
                            #'(lambda (country time-sig name &key bar-duration start-at)
                                ;(print country)
                                (if (> dur bar-duration)
                                  (and multiples
                                       (zerop (mod dur bar-duration))
                                       (= (mod (- bar-duration
                                                  (mod phase bar-duration))
                                               bar-duration)
                                          start-at))
                                  (new-beat-correct dur phase bar-duration start-at
                                                  (TIME-SIG-DIVS time-sig) sub-beats)))
                            :mode :absolute)))
    ;(print results)
    results))

#|

met prints 

(defun new-correct-tree-beat? (tree pattern phase dur max &optional multiples sub-beats)
  ; returns list of correct beat levels
  ; 0 = bar
  ; 1 = multiple bar
  (print (list 'new-correct-tree-beat? pattern phase dur max multiples '->))
  (let ((results
         (there-is-terminals tree pattern
                            #'(lambda (country time-sig name &key bar-duration start-at)
                                (print country)
                                (if (> dur bar-duration)
                                  (and multiples
                                       (zerop (mod dur bar-duration))
                                       (= (mod (- bar-duration
                                                  (mod phase bar-duration))
                                               bar-duration)
                                          start-at))
                                  (new-beat-correct dur phase bar-duration start-at
                                                  (TIME-SIG-DIVS time-sig) sub-beats)))
                            :mode :absolute)))
    (print results)
    results))

(defun new-level-tree-beat? (tree pattern phase dur max &optional multiples)
  ; returns list of correct beat levels
  ; 0 = bar
  ; 1 = multiple bar
  (print (list 'new-level-tree-beat? pattern phase dur max multiples '->))
  (let ((results
         (delete nil 
                 (collect-terminals tree pattern
                                    #'(lambda (country time-sig name &key bar-duration start-at)
                                        (print (list (list country phase dur)
                                                     time-sig name :bar bar-duration :start start-at))
                                        (if (> dur bar-duration)
                                          (when (and (zerop (mod dur bar-duration))
                                                     (= (mod (- bar-duration
                                                                (mod phase bar-duration))
                                                             bar-duration)
                                                        start-at))
                                            1)
                                          (new-beat-level dur phase bar-duration start-at
                                                          (TIME-SIG-DIVS time-sig))))
                                    :mode :absolute))))
    (print results)
    results))
|#

(defun new-beat-level (dur phase bar-dur start-at divs &optional (level 0))
  (cond ((= dur bar-dur)
         (when (= (mod (- bar-dur (mod phase bar-dur)) bar-dur)
                  start-at)
           level))
        ((> dur bar-dur) nil)
        (t (new-beat-level dur phase 
                           (/ bar-dur (or (first divs) 2)) 
                           (mod start-at (/ bar-dur (or (first divs) 2)))
                           (rest divs) (1- level)))))

(defun new-beat-correct (dur phase bar-dur start-at divs sub-beats &optional (level 0))
  (cond ((and (null sub-beats)
              (< level -1))
         nil)
        ((= dur bar-dur)
         (= (mod (- bar-dur (mod phase bar-dur)) bar-dur)
                  start-at))
        ((> dur bar-dur) nil)
        (t (new-beat-correct dur phase 
                           (/ bar-dur (or (first divs) 2)) 
                           (mod start-at (/ bar-dur (or (first divs) 2)))
                           (rest divs) 
                           sub-beats
                           (1- level)))))

;(new-LEVEL-TREE-BEAT?  *result* '() 1/4 3/4 6 T)
;(correct-anthem-level '(3/4 1/4 1 1 1 1) 4 16)
;->(NEW-LEVEL-TREE-BEAT? (3/16 1/16 1/4 1/4 1/4 1/4) 1 4 6 T ->)
;(trace new-beat-level)

#|
(defun beat-level (dur bar min)
  ;(print (list dur bar))
  (cond ((= dur bar) 0)
        ((> dur bar) 1)
        (t (max min (- (power-2-3 (/ bar dur)))))))
|#

(defun power-2-3 (x)
  (+ (powers x 2)
     (powers x 3)))

(defun powers (x n)
  (if (zerop (mod x n))
    (1+ (powers (/ x n) n))
    0))

;(beat-level 24 12)

#|
shablone

(defun remake-tree (tree)
  (if (eql (first tree) '-)
    (cons '-
          (loop for (item sub) in (rest tree)
                collect (list item (remake-tree sub))))
    tree))

|#

(defun sort-tree (tree &key (order :tree-size))
  (if (eql (first tree) '-)
    (cons '-
          (sort (loop for (item sub) in (rest tree)
                      collect (list item (sort-tree sub)))
                #'>
                :key (case order 
                       (:tree-size #'sub-tree-size)
                       (:value #'first))))
    tree))

(defun sub-tree-size (sub)
  (tree-size (second sub)))

(defun tree-size (tree)
  (if (eql (first tree) '-)
    (loop for sub in (rest tree)
          sum (sub-tree-size sub))
    1))

#|

(setf *anthems* (or (probe-file 
                     "analyses;data:National Anthems:anthem.data")
  (choose-file-dialog)))

(setf *result* (make-patterns-tree-from-file 
                *anthems* :mode :absolute
                :order :tree-size))

(with-open-file (stream "analyses;data:National Anthems:anthems absolute.tree"
                        :direction :output :if-does-not-exist :create :if-exists :supersede)
  (pprint *result* stream))

(let ((tree (with-open-file (stream "analyses;data:National Anthems:anthems absolute.tree") 
              (read stream))))
  (make-correct-fun-from-tree  correct-anthem-all-levels tree t t)
  
  (make-correct-fun-from-tree correct-anthem-bar-or-beat tree nil nil)
  
  (MAKE-correct-level-fun-from-tree correct-anthem-level tree 6 t))

(correct-anthem-level '(1/4 1/4 1/4 1/4) 0 1/4)

((wales "3/4" "Hen Wlad fy Nhadau")
 ((/4)(/4 /4 /4)(/4 /4 /4)(/2 /4)(/4 /4 /4)(/4 /4 /8 /8)(/2 /4)
  (/4 /4 /4) (/4 /4 /4) (/4 /4 /4) (/2 /4)(/4 /4 /8 /8)(/4 /4 /4)(/4 /4 /8 /8)(/2 /4)
  (/4 /4 /4) (/4 /4 /4)(3/4 +)( 3/4)
  (3/4 +)( 3/4)(/4 /4 /4) (/4 /4 /4) (/2 /4)(/2 /8 /8)(/2 /4)(/2 /8 /8)(/2 /4)(/4 /4 /4)
  (/4 /4 /4)(/2)))

;(new-LEVEL-TREE-BEAT?  *result* '(1/4 1/4 1/4 1/4) 1/4 3/4 6 T)

;(NEW-BEAT-LEVEL 3/4 1/2 3/4 1/4 '(3 2))

;(FITTING-BEATS "3/4" 3/4 1/2 :MULTIPLES nil :MAX-BEAT 15)

(untrace)

(trace new-beat-level)

(find-terminals *result* '(2 2 1 1 3 1 1 1)
                #'(lambda(country time-sig name &key bar-duration start-at)
                    (print country) nil)
                :mode :sequential-ratio)

(find-time-sigs *result* '(2 2 2))

(correct-anthem-beat? *result* '(2 2 2) 0 3)

|#

#|
;; LMS tested
(setf *anthem-tree* (with-open-file (stream "/Volumes/iDisk/Research/Data/DesainHoning/anthems.tree") (read stream)))

(find-terminals *anthem-tree* '()
                #'(lambda(country time-sig name &key bar-duration start-at)
                    (print country) nil)
                :mode :sequential-ratio)

(correct-tree-beat? *anthem-tree* '() 0 1)

(defun find-anthem (pattern) 
  (collect-terminals *anthem-tree* 
                     pattern
                     #'(lambda (country time-sig name &key bar-duration start-at)
                         (list country time-sig name))
                     :mode :sequential-ratio))

(find-anthem '(3 1 6)) ; 2 3 1 6 2 3 1 ;((HONDURAS "4/4" ("Himno Nacional")))
(find-anthem '(1 1 1 2 1))
(find-anthem '(1 1 3 3))
(find-anthem '(3 2 4 1))
(find-anthem '(3 3 2 4))
(find-anthem '(1 3 2 2 1))
(find-anthem '(3 3 2 4))
(find-anthem '(2 2 1)) ((QATAR "2/4" ("")) (EIRELAND "4/4" ("Amhran na bhFiam")) (USSR "4/4" ("Soyus nero'ushimyi")) (KENYA "4/4" ("Land of the lion")))


|#


#|
(defun make-patterns-tree (patterns &optional (mode :gcd-multiple) (tree (empty-tree)))
  (if patterns
    (make-patterns-tree (rest patterns)
                        mode
                        (add-pattern-tree (normalize (caar patterns) mode)
                                          tree
                                          (third (first patterns))))
    tree))
|#

;;; Replacement by LMS to generate tree structure from the anthem patterns.
(defun make-patterns-tree (patterns &key (mode :gcd-multiple) (tree (empty-tree)) (order :tree-size))
  (loop for (name pattern) in patterns
     do (setf tree
	      (add-pattern-tree (normalize pattern mode)
				tree
				(normalise-name name pattern mode)))
     finally (return (sort-tree tree :order order))))

(defun normalize (l &optional (mode :sequential-ratio) name)
  (when l
    (case mode
      (:absolute 
       (loop with div = (if name 
                          (destructuring-bind
                            (country time-sig name &key bar-duration start-at)
                            name
                            (/ bar-duration (read-from-string time-sig)))
                          1)
             for x in l
             collect (/ x div)))
      (:gcd-multiple
       (loop with div = (apply #'gcd l)
             for x in l
             collect (/ x div)))
      (:sequential-ratio
       (loop for x in l
             for next in (rest l)
             collect (/ next x))))))

(defun normalise-name (terminal pattern &optional (mode :sequential-ratio))
  (destructuring-bind
    (country time-sig name &key bar-duration start-at quarter-note)
    terminal
    (let* ((bar (read-from-string time-sig))
           (div (case mode
                  (:absolute (/ bar-duration bar))
                  (:gcd-multiple (apply #'gcd pattern))
                  (:sequential-ratio (first pattern)))))
      (list country time-sig name 
            :bar-duration (/ bar-duration div) :start-at (/ start-at div)))))
            

(defun ADD-pattern-tree (pattern tree name)
  (cond ((terminal? tree)
         (ADD-pattern-tree pattern (push-down tree) name))
        ((null pattern)
         (cons name (rest tree)))
        (t (cons (first tree)
                 (add-pattern-subtree pattern (rest tree) name)))))

(defun terminal? (tree)
  (listp (first tree)))

(defun empty-tree ()
  '(-))

(defun push-down (terminal)
  (destructuring-bind (rest name) terminal
    (if (null rest)
      (list name)
      (list '- 
            (list (first rest)
                  (list (rest rest) name))))))

(defun make-terminal (rest name)
  (list rest name))

(defun add-pattern-subtree (pattern subs name)
  (cond ((null subs) (list (list (first pattern) 
                                 (make-terminal (rest pattern) name))))
        ((= (first (first subs)) (first pattern))
         (cons (list (first pattern)
                     (ADD-pattern-tree (rest pattern)
                                       (second (first subs))
                                       name))
               (rest subs)))
        (t (cons (first subs)
                 (add-pattern-subtree pattern (rest subs) name)))))
                       
