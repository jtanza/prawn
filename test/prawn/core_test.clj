(ns prawn.core-test
  (:require [clojure.test :refer :all]
            [prawn.core :refer :all]))

(deftest lerf-bitboard-test
  (testing "LERF board mapping"
    (is (= 1 (:a1 board-map)))
    (is (= 2 (:b1 board-map)))
    (is (= Long/MIN_VALUE (:h8 board-map)))))
