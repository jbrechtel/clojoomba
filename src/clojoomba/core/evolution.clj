(ns clojoomba.core.evolution
    [:use
     clojoomba.core.generation
     clojoomba.core.scoring
     clojoomba.core.agent-execution])



(defn score-agent [agent room steps] (:score (last (take steps (agent-time-series agent room)))))

(defn average [nums] (/ (reduce + nums) (count nums)))
(defn score-agents [agents rooms steps]
  (pmap
    (fn [agent] [agent,(average (map
                                  (fn [room] (score-agent agent room steps))
                                  rooms))])
    agents))

(defn mutate-action [old-action] (rand-nth (remove possible-actions #(= % old-action))))

(defn mutate-agent [agent] (let
                             [agent-size       (count agent)
                              mutation-percent (+ 1 (rand-int 3))
                              mutation-total   (* (/ mutation-percent 100) agent-size)
                              mutation-indexes (take mutation-total (repeatedly #(rand-int agent-size)))]
                             (reduce #(update-in % [%2] mutate-action) agent mutation-indexes)))


(defn breed [parent-a parent-b]
  (let [breed-index (rand (count parent-a))
        transmission-a (take breed-index parent-a)
        transmission-b (drop breed-index parent-b)
        pure-child (concat transmission-a transmission-b)]
    mutate-agent pure-child))

(defn evolve [{:keys [agents steps room-size num-rooms]}]
  (let [rooms  (gen-rooms num-rooms room-size)
        agent-count (count agents)
        scores (score-agents agents rooms steps)
        agents-to-breed (repeatedly #(rand-nth (take (+ 1 (rand-int (count scores))) (map first scores))))
        all-parents (take (* 2 agent-count) agents-to-breed)
        parents-a (take agent-count all-parents)
        parents-b (drop agent-count all-parents)
        children (map breed parents-a parents-b)
        ]
      children))

(defn evolutions [{:keys [agents steps room-size num-rooms]}]
  (let [evolve-with-params (fn [current-agents] (evolve {:agents current-agents
                                                        :steps steps
                                                        :room-size room-size
                                                        :num-rooms num-rooms}))]
        (iterate evolve-with-params agents)))
