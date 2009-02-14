;;; Base 64 encoding
;;; jeff.foster@acm.org

(def *encode-table*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")

; Daft and way too slow
(defn encode-num 
  [num]
  (let [a (bit-and num 63)
	b (bit-shift-right (bit-and num 4032) 6)
	c (bit-shift-right (bit-and num 258048) 12)
	d (bit-shift-right (bit-and num 16515072) 18)]
    (map (fn [x] (nth *encode-table* x )) (list d b c a))))

(defn str-pad [x size padchar]
  (let [d (rem (count x) size)]
    (if (zero? d)
      x
      (concat x (take (- size d) (repeat padchar))))))

(defn encode 
  "Lazily encode a sequence as base64"
  [s]
  (when-not (nil? s)
    (let [x (map int (str-pad (take 3 s) 3 \=))
	  num (+ (nth x 2) (* 256 (nth x 1)) (* 256 256 (first x)))]
      (lazy-cat (encode-num num) (encode (drop 3 s))))))

(defn decode-num
  [num]
  (let [a (bit-and num 255)
	b (bit-shift-right (bit-and num 65280) 8)
	c (bit-shift-right (bit-and num 16711680) 16)]
    (list (char c) (char b) (char a))))
    
(defn decode
  "Lazily decode a sequence from base64"
  [s]
  (when-not (nil? s)
    (let [x (map (fn [x] (.indexOf *encode-table* (int x))) (take 4 s))
	  num (+ (nth x 3) (bit-shift-left (nth x 1) 6) (bit-shift-left (nth x 2) 12) (bit-shift-left (nth x 0) 18))]
      (lazy-cat (decode-num num) (decode (drop 4 s))))))