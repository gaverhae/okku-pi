(ns okku-pi.core
  (:use okku.core))

; Defining message templates
(defn m-compute []
  {:type :compute})
(defn m-work [start n-elem]
  {:type :work :start start :n-elem n-elem})
(defn m-result [value]
  {:type :result :value value})
(defn m-approx [pi dur]
  {:type :approx :pi pi :dur dur})

; Mathematical formula for pi - nothing to do with actors
(defn calculate-pi-for [^long st ^long n]
  (binding [*unchecked-math* true]
    (let [limit (* (inc st) n)]
      (loop [i (* st n) tot 0.0]
        (if (= i limit)
          tot
          (recur (inc i) (+ tot
                            (* 4.0 (/ (double (+ 1 (- (* 2 (rem i 2)))))
                                      (double (+ 1 (* 2 i))))))))))))

; worker actor - computes part of the sum
(def worker
  (actor (onReceive [{t :type s :start n :n-elem}]
           (dispatch-on t
             :work (! (m-result (calculate-pi-for s n)))))))

; master actor - coordinates the whole computation
; it is a function so we can choose some parameters
; furthermore, this allows to close over the parameters in the let form
(defn master [number-of-workers number-of-messages number-of-elements result-listener]
  (let [workerRouter (atom nil)
        res (atom {:pi 0 :nr 0})
        start (System/currentTimeMillis)]
    (actor
      (preStart [] (reset! workerRouter (spawn worker :name "workerRouter"
                                               :router (round-robin-router number-of-workers))))
      (onReceive [{t :type v :value}]
                 (dispatch-on t
                              :compute (dotimes [n number-of-messages]
                                         (! @workerRouter (m-work n number-of-elements)))
                              :result (do (swap! res #(merge-with + % {:pi v :nr 1}))
                                        (when (= (:nr @res) number-of-messages)
                                          (! result-listener (m-approx (:pi @res)
                                                                       (- (System/currentTimeMillis) start)))
                                          (stop))))))))

; simply logs the final result
(def listener
  (actor
    (onReceive [{t :type pi :pi dur :dur}]
               (dispatch-on t
                            :approx (do (println (format "\n\tPi approximation: \t\t%1.8f\n\tCalculation time: \t%8d millis"
                                                         pi dur))
                                      (shutdown))))))

(defn -main [& args]
  (let [workers (if args (Integer/parseInt (first args)) 4)
        elements 10000 messages 10000
        system (actor-system "PiSystem" :local true)
        result-printer (spawn listener :in system :name "listener")
        coordinator (spawn (master workers messages elements result-printer)
                           :in system :name "master")]
    (println "Number of workers: " workers)
    (.tell coordinator (m-compute) nil)
    (.awaitTermination system)))
