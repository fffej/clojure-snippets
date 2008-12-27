;; Not so good quick sort
(defn qsort [lst]
  (if (nil? lst)
    lst
    (concat
     (qsort (filter (partial > (first lst)) (rest lst)))
     (list (first lst))
     (qsort (filter (partial <= (first lst)) (rest lst))))))


(defn bubble [lst]
  (if (or (nil? lst) (nil? (second lst)))
    lst
    (if (> (first lst) (second lst))
      (lazy-cons (second lst) (lazy-cons (first lst) (bubble (nthrest lst 2))))
      (lazy-cons (first lst) (bubble (rest lst))))))

;; Bubble sort
(defn bubble-sort [lst]
   (last (take (count lst) (iterate bubble lst))))