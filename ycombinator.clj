;;; Y Combinator

(defn Y-banana [r]
  ((fn [f] (f f))
   (fn [f]
     (r (fn [x] ((f f) x))))))


(defn sum-seq [x]
  (if (empty? x)
    0
    (+ (first x) (sum-seq (rest x)))))

(defn sum-seq-fn-gen [func]
  (fn [s]
    (if (empty? s)
      0
      (+ (first s) (func (rest s))))))

((Y 
  (fn [func]
    (fn [s] 
      (if (empty? s)
	0
	(+ (first s) (func (rest s))))))) [1 2 3 4 5])
