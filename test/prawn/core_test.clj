(ns prawn.core-test
  (:require [clojure.test :refer :all]
            [prawn.core :refer :all]))

(deftest lerf-board-mapping
  (is (= 1 (:a1 board-map)))
  (is (= 2 (:b1 board-map)))
  (is (= Long/MIN_VALUE (:h8 board-map))))

(deftest rose-compass-move-generation
  (is (= (:a2 board-map) (north (:a1 board-map))))
  (is (= (:a3 board-map) (north (:a1 board-map) 2)))
  (is (= (:e3 board-map) (south (:e4 board-map))))
  (is (= (:e1 board-map) (south (:e4 board-map) 3)))
  (is (= (:b2 board-map) (west (:c2 board-map))))
  (is (= (:a2 board-map) (west (:c2 board-map) 2)))
  (is (= (:h4 board-map) (east (:g4 board-map))))
  (is (= (:e4 board-map) (east (:a4 board-map) 4))))
