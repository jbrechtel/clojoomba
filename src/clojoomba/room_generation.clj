(ns clojoomba.core)

(def cell-states [:clean :dirty])

(defn row [size] (vec
                   (map
                     (fn [_] (rand-nth cell-states))
                     (range size))))

(defn gen-room [size] (vec
                        (map row (repeat size size))))

(def possible-states [:clean :dirty :wall])
(def possible-actions [:north :south :east :west :random :stop :clean])
(def possible-directions [:north :south :east :west])

(def state-permutations (for [north possible-states,
                              south possible-states,
                              east  possible-states,
                              west  possible-states,
                              curr  possible-states]
                             {:north north :south south :east east :west west :current curr}))

(def states (zipmap (vec state-permutations) (range (count state-permutations))))


(defn score-action [action, state] (cond
                                     (and (= action :clean) (= (state :current) :dirty)) 10
                                     (and (= action :clean) (= (state :current) :clean)) -1
                                     (= :wall (state action)) -5
                                     :else 0))

;unfinished, next steps
(defn update-room [room, x-pos, y-pos, action] room)

(defn cell-state [room x-pos y-pos] (cond
                                         (> 0 x-pos)             :wall
                                         (> 0 y-pos)             :wall
                                         (<= (count room) x-pos) :wall
                                         (<= (count room) y-pos) :wall
                                         :else                   ((room x-pos) y-pos)))

(defn agent-state [room, x-pos, y-pos] (let [current (cell-state room x-pos y-pos)
                                             north   (cell-state room x-pos (- y-pos 1))
                                             south   (cell-state room x-pos (+ y-pos 1))
                                             east    (cell-state room (+ x-pos 1) y-pos)
                                             west    (cell-state room (- x-pos 1) y-pos)]
                                         {:current current :north north :south south :east east :west west}))

(defn random-direction [] (possible-directions (rand-int (count possible-directions))))

(defn resolve-action [state genome] (let [gene-index   (states state)
                                          action-index (genome gene-index)
                                          action       (possible-actions action-index)]
                                      (if (= action :random) random-direction action)))

(defn update-room [room action x-pos y-pos] (if (= action :clean)
                                              (assoc-in room [x-pos y-pos] :clean)
                                              room))

;score an arbitrary genome against an arbitrary room for some number of steps
;start at 0,0
;at each step
;-- find the agent's state
;-- find action for that state
;-- calculate new position given action and current position
;-- calculate score for action and position
;-- calculate new room given action and position
;-- repeat
(defn score-genome [genome room steps] 10)
