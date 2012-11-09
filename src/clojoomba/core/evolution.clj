(ns clojoomba.core.evolution
    [:use
     clojoomba.core.generation
     clojoomba.core.scoring
     clojoomba.core.agent-execution
     clojoomba.core.states
     clojoomba.core.weighted-random-selection])



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

(defn mutate-action [old-action] (rand-nth (remove possible-actions #(= % old-action))))

(defn mutate-agent [agent] (let
                             [agent-size       (count agent)
                              mutation-percent (+ 1 (rand-int 10))
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
        scores-only (map last scores)
        max-score (apply max scores-only)
        avg-score (average scores-only)
        min-score (apply min scores-only)
        min-score-abs (if (> 0 min-score) (* -1 min-score) min-score)
        normalized-scores (map (fn [[agent score]] [agent (+ min-score-abs score)]) scores)
;       normalized-scores (map #([(first %) (+ min-score (last %))]) scores)
        agents-to-breed (take agent-count (random-pairs-weighted normalized-scores))
        children (map breed (map first agents-to-breed) (map last agents-to-breed))]
      (println (str "max score: " (float max-score) " avg score: " (float avg-score)))
      children))

(defn evolutions [{:keys [agents steps room-size num-rooms]}]
  (let [evolve-with-params (fn [current-agents] (evolve {:agents current-agents
                                                        :steps steps
                                                        :room-size room-size
                                                        :num-rooms num-rooms}))]
        (iterate evolve-with-params agents)))
