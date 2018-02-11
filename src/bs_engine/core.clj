(ns bs-engine.core
  (:gen-class)
  (:require [bs-engine.syntax :refer [re-seq-with-term validate-expr validate-index]]
            [clojure.set :as set]
            [clojure.string :as str]))

(def gin (atom {}))
(def raw-data (atom {}))

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
  (let [elements (atom (re-seq-with-term query))
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
                [tokenA tokenB] (naive-split-atom opnds -2)]
            (when op
              (swap! opnds conj (-perform op tokenA tokenB)))))))
    (first @opnds)))

;; Operations with index (indexing, searching)

(defn index
  "Update global gin index."
  [doc-id & tokens]
  (let [prev-pairs (mapcat
                    (fn [t] [t doc-id])
                    (get @raw-data doc-id))
        new-pairs (mapcat
                   (fn [t] [t (conj (get @gin t #{}) doc-id)])
                   tokens)]

    ;; Delete previous entries and update actual doc in raw-data
    (apply swap! gin dissoc prev-pairs)
    ;; Update latest doc-id
    (swap! raw-data assoc doc-id tokens)
    ;; Update gin index with new information
    (apply swap! gin assoc new-pairs))
  (println (format "index ok %s" doc-id)))

(defn -join-query
  "Assuming that query is list of tokens."
  [tokens]
  (reduce set/intersection (map #(get @gin %) tokens)))

(defn search
  "Run and check result in db."
  [query-string]
  (let [canonized (canonize-query query-string)
        result (reduce set/union (map -join-query canonized))]
    (println (format "query results %s" (str/join " " (map str result))))
    result))

;; Reading input and executing engine commands

(def commands
  {"index" (fn [line]
             (let [[doc-id rst] (str/split line #" " 2)]
               (apply index doc-id (str/split rst #" "))))
   "query" search})

(defn process-command
  [line]
  (let [[cmd expr] (str/split line #" " 2)
        check-fn (case cmd
                   "index" validate-index
                   "query" validate-expr
                   (fn [x] (format "Unknown command: %s" cmd)))]
    (if-let [err (check-fn expr)]
      (println err)
      ((get commands cmd) expr))))

(defn read-with-prompt []
  (print "bs-engine> ")
  (flush)
  (str/trim (read-line)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [v (read-with-prompt)]
    (when (not (contains? #{"quit" "q"} v))
      (do
        (process-command v)
        (recur (read-with-prompt))))))

(comment

  @gin
  (index 1 "a" "b")
  (index 1 "c")
  (-main)

  (process-command "index 1 salt bubad")
  (process-command "query salt")

  )
