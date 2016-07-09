(ns povlo.core)

(def src ["hello" "my" "dear" "celine" "and" "hello" "dear" "charlie" "and" "you"])

;;================================
;;==== Markov data generation ====
;;================================

(defn- pairs-of
  "Creates a seq of two-element seqs representing each element in the coll and its succeeding element."
  [coll]
  (concat (partition 2 coll)
          (partition 2 (rest coll))))

(defn- add-to
  "Takes a map where each value is a coll as the first argument and a preceder/succeeder pair as the second argument and returns a map where the succeeder is added to the coll value connected to the preceder key."
  [m [preceder succeeder]]
  (update-in m [preceder] #(conj % succeeder)))

(defn- weigh
  "Creates a map where each element from coll is the value and all of the succeeding element occurrences are seqed together as the value."
  [coll]
  (reduce add-to {} (pairs-of coll)))
ii

;;===================================
;;==== Markov product generation ====
;;===================================

(defn- make-markov-adding-fn
  "Creates an fn which utilizes the weighted map arg to select a new last element to coll."
  [weighted-src]
  (fn [coll]
    (let [random-key (rand-nth (keys weighted-src))]
      (conj coll (if (empty? coll)
                  random-key
                  (if-let [random-value-for-last-node (rand-nth (get weighted-src (last coll)))]
                    random-value-for-last-node
                    random-key))))))

(defn- produce
  "Creates a markov-generated seq of size n based on weighted-src."
  [weighted-src n]
  (nth (iterate (make-markov-adding-fn weighted-src)
        [])
       n))

(defn povlo
  "Creates a markov-generated seq of size n based on the source coll."
  [coll n]
  (produce (weigh coll) n))
