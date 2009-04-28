
(def days 
     {0 'Saturday
      1 'Sunday
      2 'Monday
      3 'Tuesday
      4 'Wednesday
      5 'Thursday
      6 'Friday})

(def months
     ['January 
      'Febuary 
      'March 
      'April 
      'May 
      'June 
      'July 
      'August 
      'September
      'October
      'November
      'December])

(def month-to-number
     (zipmap months [13 14 3 4 5 6 7 8 9 10 11 12]))
	   
(defn zeller
  "Zeller's congruence"
  [day-of-month month year century]
  (get days 
       (mod (+ day-of-month 
		    (int (/ (* 26 (inc month)) 10))
		    year 
		    (int (/ year 4))
		    (int (/ century 4))
		    (- (* 2 century))) 7)))

(defn day-of-week 
  [day month year]
  (let [month-num (get month-to-number month)
	year-int (if (or (= month-num 13) (= month-num 14)) (dec year) year)]
    (zeller day month-num (mod year-int 100) (int (/ year-int 100)))))