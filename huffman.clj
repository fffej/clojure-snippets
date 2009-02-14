;;; Huffman encoding
;;; jeff.foster@acm.org
(defn inc-weight 
  [weight symbol]
  (if (contains? weight symbol)
    (assoc weight symbol (inc (get weight symbol)))
    (assoc weight symbol 1)))

(defn symbol-weights
  "Given a list produce a set of symbol weights"
  [s]
  ((fn [p weights]
     (if (nil? p)
       weights
       (recur (rest p) (inc-weight weights (first p))))) s {}))

(defn- sort-by-second [a b]
  (< (second a)(second b)))

(defn node-sum [a b]
  [\* (+ (second a) (second b))])

(defn tree-node [a b]
  (list (node-sum (first a) (first b)) a b))

(defn add-to-queue
  "Insert a new node, n, into an ordered list open"
  [n open]
  (if (nil? open)
    (list n)
    (let [nval (second (first n))
	  oval (second (first (first open)))]
      (if (>= nval oval)
	(lazy-cons (first open) (add-to-queue n (rest open)))
	(lazy-cons n open)))))

;   1. Create a leaf node for each symbol and add it to the priority queue.
;   2. While there is more than one node in the queue:
;         1. Remove the node of highest priority (lowest probability) twice to get two nodes.
;         2. Create a new internal node with these two nodes as children and with probability equal to the sum of the two nodes' probabilities.
;         3. Add the new node to the queue.
;   3. The remaining node is the root node and the tree is complete.
(defn coding-tree 
  "Given an ordered frequency list, create an encoding tree"
  [open]
  (if (> (count open) 1)
    (let [new-node (apply tree-node (take 2 open))]
      (recur (add-to-queue new-node (drop 2 open))))
    (first open)))

(defn left-node [tree]
  (second tree))

(defn right-node [tree]
  (if (= (count tree) 3)
    (nth tree 2)
    nil))

(defn- lookup-helper
  [tree path]
  (if (nil? tree)
    nil
    (let [v (first (first tree))]
      (lazy-cat (if (= v \*) nil (list [v path] ))
		(lookup-helper (left-node tree)  (cons 0 path)) 
		(lookup-helper (right-node tree) (cons 1 path))))))

(defn lookup
  [tree]
  (into {} (lookup-helper tree nil)))
  
(defn huffman-coding-table
  "Huffman encode the given sequence and return the huffman coding tree"
  [s]
  (let [fl (map list (sort sort-by-second (map (fn [x] [(first x) (second x)]) (symbol-weights s))))]
    (lookup (coding-tree fl))))

(defn compress 
  [s table]
  (mapcat (partial get table) s))