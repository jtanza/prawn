(ns prawn.core
  (:require [clojure.set :as set]
            [clojure.string :as string]))

(def not-a-file (unchecked-long 0xfefefefefefefefe))
(def not-h-file (unchecked-long 0x7f7f7f7f7f7f7f7f))
(def not-ab-file (unchecked-long 0xfcfcfcfcfcfcfcfc))
(def not-gh-file (unchecked-long 0x3f3f3f3f3f3f3f3f))

(def file-steps
  "Represents the ordered transitions (as bitboards) along the
  files of a chessboard, i.e. from a1->a2->a3 etc"
  (map (fn [a]
         (reverse (for [x (range 0 64 8)
                y [a]]
                    (bit-set 0 (- y x))))) (range 56 64)))

(def files
  "An ordered list of lists containing all 8 files of a chessboard
  e.g. `((:a1 :a2 ...) ... (:h1 ...)`"
  (map (fn [file]
         (for [rank (range 1 9)]
           (keyword (str file rank)))) (map char (range 97 105))))

(def squares
  "All 64 squares of a chessboard e.g. `(:a1 :a2 ... :h8)`"
  (flatten files))

(def bitboard-map
  "Mapping from square to bit position on a LERF based bitboard."
  (into {} (map zipmap files file-steps)))

(def inverted-bitboard-map
  "Mapping from bit position to square on a LERF based bitboard"
  (set/map-invert bitboard-map))

(defn north
  ([square]
   (north square 1))
  ([square n]
   (bit-shift-left square (* n 8))))

(defn north-north-east
  [square]
  (bit-and (bit-shift-left square 17) not-a-file))

(defn north-east-east
  [square]
  (bit-and (bit-shift-left square 10) not-ab-file))

(defn north-north-west
  [square]
  (bit-and (bit-shift-left square 15) not-h-file))

(defn north-west-west
  [square]
  (bit-and (bit-shift-left square 6) not-gh-file))

(defn south
  ([square]
   (south square 1))
  ([square x]
   (bit-shift-right square (* x 8))))

(defn south-east-east
  [square]
  (bit-and (bit-shift-right square 6) not-ab-file))

(defn south-south-east
  [square]
  (bit-and (bit-shift-right square 15) not-a-file))

(defn south-west-west
  [square]
  (bit-and (bit-shift-right square 10) not-gh-file))

(defn south-south-west
  [square]
  (bit-and (bit-shift-right square 17) not-h-file))

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

(defn king
  "Generates all legal king moves from the given square"
  [square]
  (let [attacks (bit-or (east square) (west square) square)]
    (bit-or (north attacks) (south attacks) attacks)))

(defn knight
  "Generates all legal knight moves from the given square"
  [square]
  (bit-or (north-north-east square) (north-north-west square) (north-east-east square)
          (north-west-west square) (south-south-east square) (south-south-west square)
          (south-west-west square) (south-east-east square)))

(def attack-map
  {:king king
   :knight knight})

(defn gen-attack-moves
  "Given a chess piece, generates all legal attack moves of that piece along every
  square of a chessboard as a map of bitboards."
  [piece]
  (into {} (map (fn [square] [(inverted-bitboard-map square) ((piece attack-map) square)])
                (map #(bit-set 0 %) (range 0 64)))))

(defn dec->bin
  "Returns as a string the binary representation of a decimal
  value x, left pads to a full 64 bit value as necessary"
  [x]
  (let [bin (Long/toBinaryString x)]
    (str (string/join (repeat (- 64 (count bin)) "0")) bin)))

(defn pr-board
  "Pretty prints a bitboard into an 8x8 chessboard"
  [bitboard]
  (print (string/join "\n" (map (partial apply str) (reverse (partition-all 8 (reverse (dec->bin bitboard))))))))

