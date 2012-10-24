(ns clojoomba.core.scoring)

(defn score-action [action, state] (cond
                                     (and (= action :clean) (= (state :current) :dirty)) 10
                                     (and (= action :clean) (= (state :current) :clean)) -1
                                     (= :wall (state action))                            -5
                                     :else 0))

