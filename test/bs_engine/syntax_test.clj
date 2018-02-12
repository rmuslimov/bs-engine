(ns bs-engine.syntax-test
  (:require [bs-engine.syntax :refer :all]
            [clojure.test :refer :all]))

(deftest test-parsing-query
  (testing "Check if parsing of query works as expected."
    (is
     (= (re-seq-with-term "a | (b & c)")
        '({:term :operand, :value "a" :end 1}
          {:term :operator, :value "|" :end 3}
          {:term :open, :value "(" :end 5}
          {:term :operand, :value "b" :end 6}
          {:term :operator, :value "&" :end 8}
          {:term :operand, :value "c" :end 10}
          {:term :close, :value ")" :end 11})))))

(deftest test-validate-query-expr
  (testing "Nested parenthesis."
    (are [x] (nil? x)
      (validate-expr "a & ((b & c) | (d & e))")
      (validate-expr "a & (b & c) | (d & e)")
    )))
