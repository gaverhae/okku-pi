(ns okku-pi.core
  (use okku.core))

(defn m-compute []
  {:type :compute})
(defn m-work [start n-elem]
  {:type :work :start start :n-elem n-elem})
(defn m-result [value]
  {:type :result :value value})
(defn m-approx [pi dur]
  {:type :approx :pi pi :dur dur})

(defn calculate-pi-for [^long st ^long n]
  (let [limit (* (inc st) n)]
    (loop [i (* st n) tot 0.0]
      (if (= i limit)
        tot
        (recur (unchecked-inc i) (+ tot
                                    (* 4.0 (/ (double (unchecked-add 1 (unchecked-negate (unchecked-multiply 2 (unchecked-remainder-int i 2)))))
                                              (double (unchecked-add 1 (unchecked-multiply 2 i)))))))))))

(defactor worker []
  (onReceive [{t :type s :start n :n-elem}]
    (dispatch-on t
      :work (! (m-result (calculate-pi-for s n))))))

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
