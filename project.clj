(defproject clojoomba "1.0.0-SNAPSHOT"
  :description "Robby the Robot implementation in Clojure"
  :main clojoomba.core.runner
  :aot [clojoomba.core.runner]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [criterium "0.3.0"]]
  )
