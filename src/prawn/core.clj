(ns prawn.core
  (:require [clojure.set :as set]))

(def not-a-file (unchecked-long 0xfefefefefefefefe))
(def not-h-file (unchecked-long 0x7f7f7f7f7f7f7f7f))

(def board-steps
  "Represents the ordered transitions (as bitboards) along the
  files of a chessboard, i.e. from a1->a2->a3 etc"
  (map (fn [a]
         (reverse (for [x (range 0 64 8)
                y [a]]
            (bit-set 0 (- y x))))) (range 56 64)))

(def ranked-squares
  "An ordered list of lists containing all 8 ranks of a chessboard 
  as keywords `(<file><rank>)`, e.g. `((:a1 :a2 ...) ... (:h1 ...)`"
  (map (fn [file]
         (for [rank (range 1 9)]
           (keyword (str file rank)))) (map #(char %) (range 97 105))))

(def squares
  "All 64 squares of a chessboard as keywords e.g. `(:a1 :a2 ... :h8)`"
  (flatten ranked-squares))

(def board-map
  "Mapping from square to bit position on a LERF based bitboard."
  (into {} (map (fn [a b] (zipmap a b)) ranked-squares board-steps)))

(def inverted-board-map
  "Mapping from bit position to square on a LERF based bitboard"
  (set/map-invert board-map))

(defn north
  ([square]
   (north square 1))
  ([square n]
   (bit-shift-left square (* n 8))))

(defn south
  "Shift square x bits right. If no x is
  provided shifts eight bits, "
  ([square]
   (south square 1))
  ([square x]
   (bit-shift-right square (* x 8))))

(defn west
  ([square]
   (west square 1))
  ([square n]
   (bit-and (bit-shift-right square n) not-h-file)))

(defn east
  ([square]
   (east square 1))
  ([square n]
   (bit-and (bit-shift-left square n) not-a-file)))

;; TODO can these just take piece keywords?
(defn king
  [square]
  (let [attacks (bit-or (east square) (west square) square)]
    (bit-or (north attacks) (south attacks) attacks)))

(def attack-map
  {:king king})

(defn gen-attack-moves
  "Given a chess piece, generates all legal attack moves of that piece along every
  square of a chessboard as a map of bitboards."
  [piece]
  (into {} (map (fn [square] [(inverted-board-map square) ((piece attack-map) square)])
                (map #(bit-set 0 %) (range 0 64)))))

(defn dec->bin
  "Returns as a string the binary representation of a decimal
  value x, left pads to a full 64 bit value as necessary"
  [x]
  (let [bin (Long/toBinaryString x)]
    (str (apply str (repeat (- 64 (count bin)) "0")) bin)))

(defn pr-board
  "Pretty prints a bitboard into an 8x8 chessboard"
  [bitboard]
  (clojure.string/join "\n" (map (partial apply str) (reverse (partition-all 8 (reverse (dec->bin bitboard)))))))

