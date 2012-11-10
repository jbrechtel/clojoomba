(ns clojoomba.core.logging [:use clojure.java.io])

(defn log-stats [max-score average-score]
  (with-open [log (writer "results.tsv" :append true)]
    (.write log (str (float max-score) "\t" (float average-score) "\n"))))

(defn init-log []
  (with-open [log (writer "results.tsv" :append false)]
    (.write log "max\taverage\n")))
