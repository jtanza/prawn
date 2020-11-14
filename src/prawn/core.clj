(ns prawn.core)

(def not-a-file (unchecked-long 0xfefefefefefefefe))
(def not-h-file (unchecked-long 0x7f7f7f7f7f7f7f7f))

(def board-steps
  "Represents "
  (map (fn [a]
         (for [x (range 0 64 8)
               y [a]]
           (- y x))) (range 56 64)))

(def pieces
  "A list of lists containing all 64 pieces of a chessboard (<file><rank>)
  as keywords, e.g. '((:a1 :a2 ...) ... (:h1 ...)"
  (map (fn [file]
         (for [rank (range 1 9)]
           (keyword (str file rank)))) (map #(char %) (range 97 105))))

(def board-map
  "Mapping from piece to bit position on a LERF based bitboard"
  (into {} (map (fn [a b] (zipmap a b)) pieces board-steps)))

(defn south
  "Shift square x bits right. If no x is
  provided shifts eight bits, "
  ([square]
   (south square 1))
  ([square x]
   (bit-shift-right square (* x 8))))

(defn north
  ([square]
   (north square 1))
  ([square n]
   (bit-shift-left square (* n 8))))

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

;; {0 bitboard 1 bitboard}
(defn gen-attack-moves
  "Given a chess piece, generates all legal attack moves of that piece as a bitboard"
  [piece]
  (into {} (map (fn [square] [square ((piece attack-map) square)])
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
  (clojure.string/join "\n" (map (partial apply str) (partition-all 8 (reverse (dec->bin bitboard))))))


;; this should work
;; (print (pr-board (get (gen-attack-moves :king) (:e4 board-map))))

