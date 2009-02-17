;;; Solving Countdown by Bruteforce
;;; jeff.foster@acm.org

(ns countdown
  (:use clojure.contrib.combinatorics)
  (:use clojure.contrib.seq-utils))

(def *operators* {'+ + '- - '/ / '* *})

(defn is-valid [op a b]
  (cond 
   (= + op) true
   (= - op) (> a b)
   (= * op) true
   (= / op) (= (mod a b) 0)))

(defstruct node :expression :value)

(defn value 
  [x]
  (if (map? x)
    (x :value)
    x))

(defn expression
  [x]
  (if (map? x)
    (x :expression)
    x))

(defn expr 
  "A list of expressions for a and b"
  [a b]
  (let [nodea (map? a) nodeb (map? b)]
    (filter (fn [x] (not (nil? x))) 
	    (map (fn [x] (when (is-valid (second x) (value a) (value b))
			   (struct node 
				   (list (first x) (expression a) (expression b)) 
				   ((second x) (value a) (value b)))))
		 *operators*))))

(defn make-expressions-helper 
  "Given a lst, build up all valid Countdown expressions"
  [x]
  (cond
   (< (count x) 2) (list (struct node (first x) (first x)))
   (= 2 (count x)) (apply expr x)
   :else
     (let [exps (apply expr (take 2 x))
	   remd (drop 2 x)]
       (mapcat make-expressions-helper (map (fn [x] (cons x remd)) exps)))))

(defn drop-one 
  "All combinations of a lst without one element"
  [lst]
  (let [v (into [] lst) size (count v)]
    (map (fn [x] (concat (subvec v 0 (dec x)) (subvec v x)))  (range 1 (inc size)))))
  

(defn make-expressions [lst]
  (if (nil? lst)
    nil
    (lazy-cat
     (mapcat make-expressions-helper (permutations lst))
     (mapcat make-expressions (drop-one lst)))))

(defn solve 
  "Solve the countdown problem"
  [numbers target]
  (filter (fn [x] (= (x :value) target)) (make-expressions numbers)))
  