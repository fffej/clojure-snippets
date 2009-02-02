;;; A macro for debug printing
;;; Copied from http://arclanguage.org/item?id=8726
;;; jeff.foster@acm.org

(defmacro dbg-prn
  "Debugging form that prints out results"
  [& more]
  `(let [start# ~more]
     (print '~more "==>" start# "\n")
     start#))