(ns clojoomba.core.states)

(def possible-states [:clean :dirty :wall])

(def state-permutations
  (for [north possible-states,
        south possible-states,
        east  possible-states,
        west  possible-states,
        curr  possible-states]
    {:north north :south south :east east :west west :current curr}))

(def states (zipmap state-permutations (range (count state-permutations))))
