(ns bitarray
  (:use clojure.test))

(defstruct bit-field :element-width :array-data)

(defn bit-array
  [n]
  (struct bit-field 31 (int-array (inc (int (/ n 31))))))

(defn set-bit!
  [bitfield bit val]
  (let [r (mod bit (bitfield :element-width))
	n (int (/ bit (bitfield :element-width)))
	x (aget (bitfield :array-data) n)]
    (if (not (zero? val))
      (aset (bitfield :array-data) n (bit-or x (bit-shift-left 1 r)))
      (aset (bitfield :array-data) n (bit-xor x (bit-shift-left 1 r))))
    bitfield))

(defn get-bit
  [bitfield bit]
  (let [r (mod bit (bitfield :element-width))
	x (aget (bitfield :array-data) (int (/ bit (bitfield :element-width))))]
    (if (= 0 (bit-and x (bit-shift-left 1 r))) 0 1)))

(deftest test-bits
  (let [n 32
	f (bit-array n)]
    (is (= 31 (f :element-width)))
    (doseq [x (range 0 n)]
      (is (= 0 (get-bit f x))))
    (doseq [x (reverse (range 0 n))]
      (set-bit! f x 1)
      (is (= 1 (get-bit f x))))
    (doseq [x (range 0 n)]
      (set-bit! f x 0)
      (is (= 0 (get-bit f x))))))