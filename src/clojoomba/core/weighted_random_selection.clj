(ns clojoomba.core.weighted-random-selection)

(defn without [items index] (into (subvec items 0 index) (subvec items (+ index 1) (count items))))

(defn select-random-item-weighted [items]
  (let [weight-sum (reduce + (map last items))]
    (loop [remaining items
           weight (* (rand) weight-sum)
           index 0]
          (let [remaining-weight (- weight (last (first remaining)))]
            (if (<= remaining-weight 0)
              [(first (first remaining)) index]
              (recur (rest remaining) remaining-weight (+ 1 index)))))))

(defn select-random-pair-weighted [items]
  (let [[first-item first-item-index] (select-random-item-weighted items)
        items-without-first (without items first-item-index)
        [second-item _] (select-random-item-weighted items-without-first)]
    [first-item second-item]))

(defn random-pairs-weighted [items-and-weights]
  (let [sorter #(> (last %) (last %2))
        sorted-items (vec (sort sorter items-and-weights))]
    (repeatedly #(select-random-pair-weighted sorted-items))))
