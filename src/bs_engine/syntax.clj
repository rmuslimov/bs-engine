(ns bs-engine.syntax
  (:require [clojure.string :as str]))

(def query-terms [:operand :operator :open :close])
(def query-syntax #"(\w+)|(\||\&)|(\()|(\))")

(def rules
  {:open #{:open :operand}
   :operand #{:operator :close}
   :operator #{:operand :open}
   :close #{:operator}})

(defn with-term
  "Add more information to regex group returned by re-seq-with-term"
  [groups]
  (loop [[term & terms] query-terms
         [v & rest] groups]
    (if (or v (empty? rest))
      {:term term :value v}
      (recur terms rest))))

(defn re-seq-with-term
  "Like re-seq but adds term and position"
  [query]
  (let [m (re-matcher query-syntax query)
        rcs (fn rcs []
              (when-let [v (re-find m)]
                (cons (-> v rest with-term (assoc :end (.end m))) (lazy-seq (rcs)))))]
    (rcs)))

(defn err-description
  [actual-elem expected]
  (format "%s gotten, but %s expected."
          (name actual-elem) (str/join " or " (map name expected))))

(defn err-marker
  "Additional two lines for error, with marker where exception occured."
  [query position]
  (format "%s\n%s%s" query (str/join (repeat position "_")) "^;"))

(defn validate-expr
  "Scan and check query for mistakes."
  [query]
  (loop [[cur & rst] (re-seq-with-term query)
         remaining-closes 0]
    (let [next-element (-> rst first :term)
          expecting (->> cur :term (get rules))
          wrong? (not (contains? expecting next-element))
          remaining-closes
          (case (:term cur)
            :open (inc remaining-closes)
            :close (dec remaining-closes)
            remaining-closes)]

      (cond
        ;; Unexpected end.
        (and (empty? rst) (not= remaining-closes 0))
        (format "query error\n%s\n%s"
                "Unclosed brackets"
                (err-marker query (:end cur)))

        ;; Operator on last position
        (and (empty? rst) (= (:term cur) :operator))
        (format "query error\n%s\n%s"
                "Operand or bracket opening expected."
                (err-marker query (:end cur)))

        ;; Done. All looks good.
        (empty? rst) nil

        ;; Something forgotten
        wrong? (format "query error\n%s\n%s"
                       (err-description next-element expecting)
                       (err-marker query (:end cur)))
        :default (recur rst remaining-closes)))))

(defn validate-index
  "Scan index expression for mistakes."
  [line]
  (let [[doc-id & tokens] (str/split line #" ")
        err (cond
              (not (re-matches #"\d+" doc-id))
              (format "doc-id %s is not int." doc-id)

              (some nil? (map #(re-matches #"\w+" %) tokens))
              "some tokens are not AN+ value.")]
    (when err
      (format "index error \"%s\"" err))))

(comment

  (some nil? '(nil))
  (check "a | a & a" )
  (check "salt & (butter | potato)")
  (validate-index "1 !asd")
  )
