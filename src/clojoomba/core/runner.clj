(ns clojoomba.core.runner (:gen-class)
    [:use
     clojoomba.core.evolution
     clojoomba.core.generation])

(declare run-for)


(defn -main [generations-str & args]
  (let [generations (Integer/parseInt generations-str)]
       (time (run-for generations))))

(defn run-for [generations]
  (let [best-generation (last (take generations (evolutions {:agents (gen-agents 200) :steps 200 :room-size 10 :num-rooms 100})))
        rooms (gen-rooms 100 10)
        scored (score-agents best-generation rooms 200)
        sorted (sort #(> (last %) (last %2)) scored)
        best (first (first sorted))
        best-score (last (first sorted))]
    (println best-score)))
