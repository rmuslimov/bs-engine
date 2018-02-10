(ns bs-engine.core
  (:gen-class)
  (:require [clojure.set :as set]))

(def query-terms [:operand :operator :open :close])
(def query-syntax #"(\w+)|(\||\&)|(\()|(\))")

(def gin (atom {}))

(defn with-term
  "Add more information to regex group returned by re-seq-with-term"
  [groups]
  (loop [[term & terms] query-terms
         [v & rest] groups]
    (if (or v (empty? rest))
      {:term term :value v}
      (recur terms rest))))

(defn re-seq-with-term
  [query]
  (for [[v & groups] (re-seq query-syntax query)]
    (with-term groups)))

(defn naive-split-atom
  "Swaps atom value to (swap-fn a) and returns (take-fn a)."
  [a n]
  (let [take-fn (partial (if (>= n 0) take take-last) (Math/abs n))
        swap-fn (partial (if (>= n 0) drop drop-last) (Math/abs n))
        v (take-fn @a)]
    (swap! a #(into [] (swap-fn %)))
    v))

;; Operation of & and | for queries

(defmulti -perform (fn [op & r] op))
(defmethod -perform "&" [_ treeA treeB]
  (set
   (for [a treeA b treeB]
     (into [] (concat a b)))))

(defmethod -perform "|" [_ treeA treeB]
  (set/union treeA treeB))

;; Main query parsing function

(defn canonize-query
  "Read string and returns set of queries.

  Each query is list of tokens. Resultset for query is just intersection of resultset for each token.
  Result for all queries is trivial union."
  [query]
  (let [elements (atom (re-seq-with-term (str "(" query ")")))
        oprts (atom [])
        opnds (atom [])]
    (while (seq @elements)
      (let [[{:keys [term value end]}] (naive-split-atom elements 1)]
        (case term
          :operand (swap! opnds conj #{[value]})
          :operator (swap! oprts conj value)
          :open (swap! oprts conj value)
          :close
          (let [[open-op op] (naive-split-atom oprts -2)
                [tokenA tokenB] (naive-split-atom opnds -2)
                v (-perform op tokenA tokenB)]
              (swap! opnds conj v)))))
    (first @opnds)))

;; Operations with index (indexing, searching)

(defn index
  "Update global gin index."
  [doc-id & tokens]
  (let [index @gin
        pairs (mapcat
               (fn [t] [t (conj (get index t #{}) doc-id)])
               tokens)]
    (apply swap! gin assoc pairs)))

(defn -join-query
  "Assuming that query is list of tokens."
  [tokens]
  (reduce set/intersection (map #(get @gin %) tokens)))

(defn search
  "Run and check result in db."
  [query-string]
  (let [canonized (canonize-query query-string)]
    (reduce set/union (map -join-query canonized))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment

  (def q "(butter | potato) & salt")
  (canonize-query q)
  (search q)

  )
