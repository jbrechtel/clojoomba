(ns clojoomba.core.runner (:gen-class)
    [:use
     clojoomba.core.evolution
     clojoomba.core.generation])


(defn -main [& args]
  (let [best (last (take 1000 (evolutions {:agents (gen-agents 200) :steps 200 :room-size 10 :num-rooms 100})))
        best-score (score-agent best (gen-room 10) 200)]
    (println best-score)
    (println best)))
