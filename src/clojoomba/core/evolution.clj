(ns clojoomba.core.evolution
    [:use
     clojoomba.core.generation
     clojoomba.core.scoring
     clojoomba.core.agent-execution
     clojoomba.core.states
     clojoomba.core.weighted-random-selection
     clojoomba.core.logging])

(defn score-agent [agent initial-room total-steps]
  (loop [room initial-room
         step 0
         score 0
         x-pos 0
         y-pos 0]
    (if (= step total-steps)
      score
      (let [state         (agent-state room x-pos y-pos)
            action        (nth agent (states state))
            new-room      (update-room room action x-pos y-pos)
            [new-x new-y] (update-pos action room x-pos y-pos)
            new-score     (+ score (score-action action state))
            next-step     (+ 1 step)]
        (recur new-room next-step new-score new-x new-y)))))

(defn average [nums] (/ (reduce + nums) (count nums)))
(defn score-agents [agents rooms steps]
  (pmap
    (fn [agent] [agent,(average (map
                                  (fn [room] (score-agent agent room steps))
                                  rooms))])
    agents))

(defn mutate-action [old-action] (rand-nth (remove #(= % old-action) possible-actions)))

(defn mutate-agent [agent] (let
                             [agent-size       (count agent)
                              mutation-total   (* (/ (rand) 100) agent-size)
                              mutation-indexes (vec (take mutation-total (repeatedly #(rand-int agent-size))))]
                             (vec (reduce #(update-in % [%2] mutate-action) (vec agent) mutation-indexes))))


(defn breed [[parent-a parent-b]]
  (let [size (count parent-a)
        breed-index (rand size)
        child-a-transmission-a (subvec parent-a 0 breed-index)
        child-a-transmission-b (subvec parent-b breed-index size)
        child-b-transmission-a (subvec parent-b 0 breed-index)
        child-b-transmission-b (subvec parent-a breed-index size)
        child-a (into child-a-transmission-a child-a-transmission-b)
        child-b (into child-b-transmission-a child-b-transmission-b)]
    [(mutate-agent child-a) (mutate-agent child-b)]))

(defn evolve [{:keys [agents steps room-size num-rooms]}]
  (let [rooms  (gen-rooms num-rooms room-size)
        agent-count (count agents)
        scores (score-agents agents rooms steps)
        scores-only (map last scores)
        max-score (apply max scores-only)
        avg-score (average scores-only)
        min-score (apply min scores-only)
        min-score-abs (if (> 0 min-score) (* -1 min-score) min-score)
        normalized-scores (map (fn [[agent score]] [agent (+ min-score-abs score)]) scores)
        agents-to-breed (take (/ agent-count 2) (random-pairs-weighted normalized-scores))
        children (apply concat (map breed agents-to-breed))]
      (log-stats max-score avg-score)
      children))

(defn evolutions [{:keys [agents steps room-size num-rooms]}]
  (let [evolve-with-params (fn [current-agents] (evolve {:agents current-agents
                                                        :steps steps
                                                        :room-size room-size
                                                        :num-rooms num-rooms}))]
        (iterate evolve-with-params agents)))
