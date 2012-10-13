(ns clojoomba.core)

(defn row [size] (vec
                   (map
                     (fn [_] (= 1 (rand-int 2)))
                     (range size))))

(defn gen-room [size] (vec
                        (map row (repeat size size))))

(def possible-states [:clean :dirty :wall])

(def state-permutations (for [north possible-states,
                              south possible-states,
                              east  possible-states,
                              west  possible-states,
                              curr  possible-states] (list north south east west curr)))

(def states (zipmap (vec state-permutations) (range (count state-permutations))))
