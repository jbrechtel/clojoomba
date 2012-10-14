(ns clojoomba.core)

(def cell-states [:clean :dirty])
(defn row [size] (vec (map
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

(defn gen-agent [] (vec (map (fn [_] (rand-nth possible-actions)) state-permutations)))

(def states (zipmap (vec state-permutations) (range (count state-permutations))))


(defn score-action [action, state] (cond
                                     (and (= action :clean) (= (state :current) :dirty)) 10
                                     (and (= action :clean) (= (state :current) :clean)) -1
                                     (= :wall (state action))                            -5
                                     :else 0))

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

(defn resolve-action [state agent] (let [action-index (states state)
                                         action       (agent action-index)]
                                      (if (= action :random) random-direction action)))

(defn update-room [room action x-pos y-pos] (if (= action :clean)
                                              (assoc-in room [x-pos y-pos] :clean)
                                              room))

(defn desired-pos [action x-pos y-pos] (cond
                                         (= :north action) [x-pos (- y-pos 1)]
                                         (= :south action) [x-pos (+ y-pos 1)]
                                         (= :east  action) [(+ x-pos 1) y-pos]
                                         (= :west  action) [(- x-pos 1) y-pos]
                                         :else [x-pos y-pos]))

(defn update-pos [action room x-pos y-pos] (let [[new-x new-y] (desired-pos action x-pos y-pos)]
                                             (cond
                                               (> 0 new-x) [x-pos y-pos]
                                               (> 0 new-y) [x-pos y-pos]
                                               (<= (count room) new-x) [x-pos y-pos]
                                               (<= (count room) new-y) [x-pos y-pos]
                                               :else [new-x new-y])))


(defn step-agent [{:keys [room agent score x-pos y-pos]}]
  (let [state         (agent-state room x-pos y-pos)
        action        (resolve-action state agent)
        new-room      (update-room room action x-pos y-pos)
        [new-x new-y] (update-pos action room x-pos y-pos)
        new-score     (+ score (score-action action state))]
    {:agent agent :room new-room :x-pos new-x :y-pos new-y :score new-score}))

(defn agent-time-series [agent room] (lazy-cat [{:agent agent :room room :x-pos 0 :y-pos 0 :score 0}]
                                               (map step-agent (agent-time-series agent room))))

(defn score-agent [agent room steps] (:score (last (take steps (agent-time-series agent room)))))

(def james (gen-agent))
(def home (gen-room 10))
(def agents (map (fn [_] (gen-agent)) (range 20)))
(println (map #(score-agent % home 10) agents))

(def agent-count 30)
(def initial-agents (map (fn [_] (gen-agent)) (range agent-count)))

;score an arbitrary genome against an arbitrary room for some number of steps
;start at 0,0
;at each step
;-- find the agent's state
;-- find action for that state
;-- calculate new position given action and current position
;-- calculate score for action and position
;-- calculate new room given action and position
;-- repeat
