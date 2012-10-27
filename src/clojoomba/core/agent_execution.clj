(ns clojoomba.core.agent-execution [:use clojoomba.core.states clojoomba.core.scoring])

(defn cell-state [room x-pos y-pos]
  (cond
    (> 0 x-pos)             :wall
    (> 0 y-pos)             :wall
    (<= (count room) x-pos) :wall
    (<= (count room) y-pos) :wall
    :else                   ((room x-pos) y-pos)))

(defn agent-state [room, x-pos, y-pos]
  (let [current (cell-state room x-pos y-pos)
        north   (cell-state room x-pos (- y-pos 1))
        south   (cell-state room x-pos (+ y-pos 1))
        east    (cell-state room (+ x-pos 1) y-pos)
        west    (cell-state room (- x-pos 1) y-pos)]
    {:current current :north north :south south :east east :west west}))

(defn update-room [room action x-pos y-pos]
  (if (= action :clean)
    (assoc-in room [x-pos y-pos] :clean)
    room))

(def possible-directions [:north :south :east :west])
(defn desired-pos [action x-pos y-pos]
  (cond
    (= :north  action) [x-pos (- y-pos 1)]
    (= :south  action) [x-pos (+ y-pos 1)]
    (= :east   action) [(+ x-pos 1) y-pos]
    (= :west   action) [(- x-pos 1) y-pos]
    (= :random action) (desired-pos (rand-nth possible-directions) x-pos y-pos)
    :else [x-pos y-pos]))

(defn update-pos [action room x-pos y-pos]
  (let [[new-x new-y] (desired-pos action x-pos y-pos)]
    (cond
      (> 0 new-x) [x-pos y-pos]
      (> 0 new-y) [x-pos y-pos]
      (<= (count room) new-x) [x-pos y-pos]
      (<= (count room) new-y) [x-pos y-pos]
      :else [new-x new-y])))
