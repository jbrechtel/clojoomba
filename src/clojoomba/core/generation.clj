(ns clojoomba.core.generation
    [:use clojoomba.core.states])

(def cell-states [:clean :dirty])

(defn row [size] (vec (map
                     (fn [_] (rand-nth cell-states))
                     (range size))))

(def possible-actions [:north :south :east :west :random :stop :clean])
(defn gen-agent [] (vec (map (fn [_] (rand-nth possible-actions)) state-permutations)))
(defn gen-room [size] (vec (map row (repeat size size))))

(defn gen-agents [agent-count] (map (fn [_] (gen-agent)) (range agent-count)))
(defn gen-rooms  [room-count room-size]  (map (fn [_] (gen-room room-size))  (range room-count)))
