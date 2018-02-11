(ns bs-engine.core-test
  (:require [clojure.test :refer :all]
            [bs-engine.core :refer :all]))

(defn create-index
  [f]
  (reset! gin {})
  (index 1 "soup" "tomato" "cream" "salt")
  (index 2 "cake" "sugar" "eggs" "flour" "sugar" "cocoa" "cream" "butter")
  (index 1 "bread" "butter" "salt")
  (index 3 "soup" "fish" "potato" "salt" "pepper")
  (f))

(use-fixtures :each create-index)

(deftest test-canonize-query
  (testing "Query interpretation"
    (are [x y] (= x y)
      (canonize-query "a | b") #{["b"] ["a"]}
      (canonize-query "a & b") #{["a" "b"]}
      (canonize-query "a & (( b & c ) | d)") #{["a" "d"] ["a" "b" "c"]}
      (canonize-query "a & ((( b & c ) & d) | e)") #{["a" "b" "c" "d"] ["a" "e"]}
      )))

(deftest test-indexing-and-seaching
  (testing "whole flow..."
    (is (= #{2} (search "eggs & flour")))
    (is (= #{1 3} (search "salt & (butter | potato)")))
    (is (= #{1 3} (search "(butter | potato) & salt")))

    (is (= #{3 2} (search "soup | sugar")))
    (is (= #{3} (search "soup")))
    ))
