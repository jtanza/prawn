(ns prawn.core)

(def a-file-mask (unchecked-long 0xfefefefefefefefe))
(def h-file-mask (unchecked-long 0x7f7f7f7f7f7f7f7f))

(def attack-map
  {:king king})

(def piece-movement
  {:king {:north 8
          :south -8
          :east 1
          :west -1
          :no-west 7
          :no-east 9
          :so-west -9
          :so-east -7}})

(def direction-operations
  {:east bit-shift-left})

;; {0 bitboard 1 bitboard}
(defn gen-attack-moves
  "Given a chess piece, generates all legal attack moves of that piece as a bitboard"
  [piece]
  (into {} (map (fn [square] [square ((piece attack-map) square)])
                (map #(bit-set 0 %) (range 0 64)))))

(defn king
  [square]
  (bit-or (bit-or
           (bit-and (bit-shift-left square 1) a-file-mask)
           (bit-and (bit-shift-right square 1) a-file-mask))
          (bit-or
           (bit-and (bit-shift-left square 8) a-file-mask)
           (bit-and (bit-shift-right square 8) a-file-mask))))

(defn print-board
  "Pretty prints a bitboard into an 8x8 chessboard"
  [bitboard]
  (clojure.string/join "\n" (map (partial apply str) (partition-all 8 (dec->bin bitboard)))))

(defn dec->bin
  "Returns as a string the binary representation of a decimal
  value x, left pads to a full 64 bit value as necessary"
  [x]
  (let [bin (Long/toBinaryString x)]
    (str (apply str (repeat (- 64 (count bin)) "0")) bin)))


