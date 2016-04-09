(ns povlo.core)

(def src ["hello" "my" "dear" "celine" "and" "hello" "dear" "charlie" "and" "you"])

(defn pairs-of
  [coll]
  (concat (partition 2 coll)
          (partition 2 (rest coll))))

(defn add-to
  [m pair]
  (let [k (first pair)
        v (second pair)]
    (update-in m [k] #(conj % v))))

(defn weigh
  [coll]
  (reduce add-to {} (pairs-of coll)))

(defn produce
  [weighted-src n]
  (loop [w weighted-src
         pos 0
         acc []]
    (if (= n pos) acc
        (let [random-key (rand-nth (keys w))
              new-acc (conj acc (if (= 0 pos) random-key
                                    (if-let [random-value-for-last-node (rand-nth (get w (last acc)))]
                                      random-value-for-last-node
                                      random-key)))]
          (recur w (inc pos) new-acc)))))

(defn povlo
  [coll n]
  (produce (weigh coll) n))

(defn povlo-inlined
  [coll n]
  (loop [w (reduce (fn add-to
                     [m pair]
                     (let [k (first pair)
                           v (second pair)]
                       (update-in m [k] #(conj % v))))
                   {}
                   (concat (partition 2 coll) (partition 2 (rest coll))))
         pos 0
         acc []]
    (if (= n pos) acc
        (let [random-key (rand-nth (keys w))
              new-acc (conj acc (if (= 0 pos) random-key
                                    (if-let [random-value-for-last-node (rand-nth (get w (last acc)))]
                                      random-value-for-last-node
                                      random-key)))]
          (recur w (inc pos) new-acc)))))
