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

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
