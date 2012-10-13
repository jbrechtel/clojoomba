(ns clojoomba.core)

(defn row [size] (vec
                   (map
                     (fn [_] (= 1 (rand-int 2)))
                     (range size))))

(defn room [size] (vec
                    (map row (repeat size size))))
