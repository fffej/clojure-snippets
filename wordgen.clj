;;; Henley example program from ANSI Common Lisp
;;; jeff.foster@acm.org

(defn file-as-wordlist [f]
  (filter (fn [x] (> (count x) 0)) (into '[] (.split (slurp f) "\n|[ ]|[.]|[,]"))))

(defn build-frequency-map [words]
  (let [word-pairs (mapcat (fn [x y] (list [x y])) (cons "" words) words)]
    (reduce (fn [accum v]
	      (let [w1 (first v) w2 (second v) val (get accum w1)]
		(if (nil? val)
		  (assoc accum w1 {w2 1})
		  (let [currentVal (get val w2)]
		    (if (nil? currentVal)
		      (assoc accum w1 (conj val {w2 1}))
		      (assoc accum w1 (conj val {w2 (inc currentVal)})))))))
	    (hash-map) word-pairs)))

(defn frequency-map-count [m word]
  (let [v (get m word)]
    (if (nil? v)
      0
      (reduce (fn [x y] (+ x (second y))) 0 v))))

(defn next [m word]
  (let [following (get m word) p (rand-int (frequency-map-count m word))]
    ((fn [words prob]
       (let [word-count (second (first words))]
	 (if (>= word-count prob)
	   (first (first words))
	   (recur (rest words) (- prob word-count))))) following p)))

(defn generate-text [example n & [start]]
  (let [words (file-as-wordlist example) fm (build-frequency-map words)
        start-word (if (nil? start) "the" start)]
    (take n (iterate (fn [x] (next fm x)) start-word))))
    
  