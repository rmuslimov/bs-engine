(ns bs-engine.core
  (:gen-class)
  (:require [bs-engine.syntax :refer [re-seq-with-term validate-expr validate-index]]
            [clojure.set :as set]
            [clojure.string :as str]))

(def gin (atom {}))
(def raw-data (atom {}))

;; Operation of & and | for query trees.

(defmulti -perform (fn [op & r] op))
(defmethod -perform "&" [_ treeA treeB]
  (set
   (for [a treeA b treeB]
     (into [] (concat a b)))))

(defmethod -perform "|" [_ treeA treeB]
  (set/union treeA treeB))

;; Operations. Functions used for query parsing and calculation.

(defn operate
  "Takes N+1 operands and N operators and perform calculation from left-to-right."
  [tokens ops]
  (let [pairs (partition 2 (interleave (rest tokens) ops))]
    (reduce
     (fn [rs [t op]] (-perform op rs t))
     (first tokens)
     pairs)))

(defn drop-while-backward
  "Like drop-while but starts from the end."
  [pred s]
  (->> s reverse (take-while pred) reverse (into [])))

(defn take-while-backward
  "Like take-while but starts from the end."
  [pred s]
  (->> s reverse (drop-while pred) reverse drop-last (into [])))

;; Main query parsing function

(defn canonize-query
  "Read string and returns set of queries.

  Each query is list of tokens. Resultset for query is just intersection of resultset for each token.
  Result for all queries is trivial union."
  [query]
  (let [elements (atom (re-seq-with-term (str "(" query ")")))
        oprts (atom [])
        opnds (atom [])
        not-opening? #(not= "(" %)]
    (while (seq @elements)
      (let [{:keys [term value end]} (first @elements)]
        (swap! elements rest)
        (case term
          :operand (swap! opnds conj #{[value]})
          :operator (swap! oprts conj value)
          :open (do (swap! oprts conj value) (swap! opnds conj value))
          :close
          (let [tokens (drop-while-backward not-opening? @opnds)
                ops (drop-while-backward not-opening? @oprts)
                new-v (operate tokens ops)]
            (swap! opnds #(take-while-backward not-opening? %))
            (swap! oprts #(take-while-backward not-opening? %))
            (swap! opnds conj new-v)))))
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

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [v (read-line)]
    (when (not (contains? #{"quit" "q" nil} v))
      (do
        (process-command (str/trim v))
        (recur (read-line))))))


(comment

  (process-command "query")
  (validate-index nil)
  @gin
  (index 1 "a" "b")
  (index 1 "c")

  (canonize-query "soup & tree | bob & (alice & eva)")
  (process-command "index 1 salt bubad")
  (process-command "query soup")

  (-main)

  )
