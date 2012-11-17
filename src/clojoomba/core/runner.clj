(ns clojoomba.core.runner (:gen-class)
    [:use
     clojoomba.core.evolution
     clojoomba.core.generation
     clojoomba.core.logging])

(defn run-for [generations]
  (init-log)
  (let [best-generation (nth (evolutions {:agents (gen-agents 200) :steps 200 :room-size 10 :num-rooms 100}) generations)
        rooms (gen-rooms 100 10)
        scored (score-agents best-generation rooms 200)
        sorted (sort #(> (last %) (last %2)) scored)
        best (first (first sorted))
        best-score (last (first sorted))]
    (println best-score)
    (println best)))

(defn -main [generations-str & args]
  (let [generations (Integer/parseInt generations-str)]
       (run-for generations)
       (. clojure.lang.Agent shutdown)))
