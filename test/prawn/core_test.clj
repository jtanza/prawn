(ns prawn.core-test
  (:require [clojure.test :refer :all]
            [prawn.core :refer :all]))

(deftest lerf-board-mapping
  (is (= 1 (:a1 bitboard-map)))
  (is (= 2 (:b1 bitboard-map)))
  (is (= Long/MIN_VALUE (:h8 bitboard-map))))

(deftest rose-compass-move-generation
  (is (= (:a2 bitboard-map) (north (:a1 bitboard-map))))
  (is (= (:a3 bitboard-map) (north (:a1 bitboard-map) 2)))
  (is (= (:e3 bitboard-map) (south (:e4 bitboard-map))))
  (is (= (:e1 bitboard-map) (south (:e4 bitboard-map) 3)))
  (is (= (:b2 bitboard-map) (west (:c2 bitboard-map))))
  (is (= (:a2 bitboard-map) (west (:c2 bitboard-map) 2)))
  (is (= (:h4 bitboard-map) (east (:g4 bitboard-map))))
  (is (= (:e4 bitboard-map) (east (:a4 bitboard-map) 4))))

(deftest king-movements
  (let [moves (gen-attack-moves :king)]
    (is (= 771 (:a1 moves)))
    (is (= 241461362688 (:e4 moves)))))

(deftest knight-movements
  (let [moves (gen-attack-moves :knight)]
    (is (= 132096 (:a1 moves)))
    (is (= 44272527353856 (:e4 moves)))))
