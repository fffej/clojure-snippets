;;; Solving Countdown by Bruteforce
;;; jeff.foster@acm.org

(ns countdown
  (:use clojure.contrib.combinatorics)
  (:use clojure.contrib.seq-utils))

(def *operators* [+ - / *])

(defn is-valid [op a b]
  (cond 
   (= '+ op) true
   (= '- op) (> a b)
   (= '* op) true
   (= '/ op) (= 0 (mod a b))))

(defn expr 
  "A list of expressions for a and b"
  [a b]
  (map (fn [x] (x a b)) *operators*))

(defn expressions-helper 
  "Given a lst, build up all valid Countdown expressions"
  [x]
  (cond
   (< (count x) 2) x
   (= 2 (count x)) (apply expr x)
   :else
     (let [exps (apply expr (take 2 x))
	   remd (drop 2 x)]
       (mapcat expressions-helper (map (fn [x] (cons x remd)) exps)))))

(defn expressions [lst]
  (if (nil? lst)
    nil
    (lazy-cat
     (mapcat expressions-helper (permutations lst))
     (expressions (rest lst)))))
  