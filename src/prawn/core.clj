(ns prawn.core)

(def not-a-file (unchecked-long 0xfefefefefefefefe))
(def not-h-file (unchecked-long 0x7f7f7f7f7f7f7f7f))

(def square-map
  {:a1 (bit-set 0 7)
   :b1 (bit-set 0 6)
   :c1 (bit-set 0 5)
   :d1 (bit-set 0 4)
   :e1 (bit-set 0 3)
   :f1 (bit-set 0 2)
   :g1 (bit-set 0 1)
   :h1 0})

(defn south
  "Shift square x bits right. If no x is
  provided shifts eight bits, "
  ([square]
   (south square 1))
  ([square n]
   (bit-shift-right square (* n 8))))

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

(defn king
  [square]
  (let [attacks (bit-or (east square) (west square) square)]
    (bit-or (north attacks) (south attacks) attacks)))


(def attack-map
  {:king king})

(def direction-operations
  {:east bit-shift-left})

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

(defn print-board
  "Pretty prints a bitboard into an 8x8 chessboard"
  [bitboard]
  (clojure.string/join "\n" (map (partial apply str) (partition-all 8 (reverse (dec->bin bitboard))))))



