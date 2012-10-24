(ns clojoomba.core.runner (:gen-class)
    [:use
     clojoomba.core.evolution
     clojoomba.core.generation])

;steps are off by 1
;taking n from this yields n-1 actual steps since the first is equal to the input


(defn -main [& args]
; (println (evolve {:agents (gen-agents 10) :steps 10 :num-rooms 1 :room-size 10}))
  (let [best (last (take 10 (evolutions {:agents (gen-agents 10) :steps 200 :room-size 10 :num-rooms 10})))
        best-score (score-agent best (gen-room 10) 200)]
    (println best-score)
    (println best)))

;(println (take 20 (repeatedly gen-agent)))

;this is the lazy seq, needs to call evolve once then map the results against evolutions for subsequent items

; num-agents 200
; num-generations 1000
; num-rooms 100
; num-actions 200
;


;(evolve {:agents (gen-agents 12)
;        :steps 200
;        :room-size 10
;        :num-rooms 10})


;score an arbitrary genome against an arbitrary room for some number of steps
;start at 0,0
;at each step
;-- find the agent's state
;-- find action for that state
;-- calculate new position given action and current position
;-- calculate score for action and position
;-- calculate new room given action and position
;-- repeat
