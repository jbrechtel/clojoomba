(ns clojoomba.core.generation
    [:use clojoomba.core.states])

(def cell-states [:clean :dirty])
(def possible-actions [:north :south :east :west :random :stop :clean])

(defn row [size] (vec (take size (repeatedly #(rand-nth cell-states)))))
(defn gen-room [size] (vec (take size (repeatedly #(row size)))))
(defn gen-rooms  [room-count room-size]  (take room-count (repeatedly #(gen-room room-size))))

(defn gen-agent [] (vec (take (count state-permutations) (repeatedly #(rand-nth possible-actions)))))
(defn gen-agents [agent-count] (take agent-count (repeatedly gen-agent)))
