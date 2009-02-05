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
		   
; Need to implement padding
(defn encode 
  "Lazily encode a sequence as base64"
  [seq]
  (if (nil? seq)
    nil
    (let [x (map int (take 3 seq))
	  num (+ (last x) (* 256 (second x)) (* 256 256 (first x)))]
      (lazy-cons (encode-num num) (encode (drop 3 seq))))))
