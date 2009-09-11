(ns bloom
  (:use bitarray)
  (:use clojure.test)
  (:import (java.security MessageDigest)))

(defn pad [n s]
  (let [padding (- n (count s))]
    (apply str (concat (apply str (repeat padding "0")) s))))

(defn md5-hash [s]
  (let [m (MessageDigest/getInstance "MD5")]
    (.update m (.getBytes (str s)) 0 (count s))
    (let [x (.toString (BigInteger. 1 (.digest m)) 16)]
      (pad 32 x))))

(def md5-hashes 
     (list
      (fn [x] (BigInteger. (apply str (take 3 (md5-hash x))) 16))
      (fn [x] (BigInteger. (apply str (take 3 (drop 4 (md5-hash x)))) 16))
      (fn [x] (BigInteger. (apply str (take 3 (drop 8 (md5-hash x)))) 16))
      (fn [x] (BigInteger. (apply str (take 3 (drop 12 (md5-hash x)))) 16))))

(defstruct bloom-filter :hashfns :value)

(defn make-bloom-filter
  ([n] (struct bloom-filter md5-hashes (bit-array n)))
  ([n fns] (struct bloom-filter fns (bit-array n))))

(defn add!
  [bloom n]
  (let [hashes (map (fn [x] (x n)) (bloom :hashfns))]
    (doseq [x hashes] (set-bit! (bloom :value) x 1))
    bloom))

(defn query
  [bloom n]
  (let [hashes (map (fn [x] (x n)) (bloom :hashfns))]
    (reduce bit-and (map (fn [z] (get-bit (bloom :value)  z)) hashes))))

(deftest test-bloom
  (let [teststrs (map (fn [x] (str x)) (range 0 1000))
	bloom (make-bloom-filter 0xFFF)]
    (doseq [x teststrs]
      (is (= 0 (query bloom x)))
      (add! bloom x)
      (is (= 0 (query bloom (str "not" x))))
      (is (query bloom x)))))
     