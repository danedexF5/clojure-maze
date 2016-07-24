(ns maze-clojure.core
  (:gen-class))

(def size 10)

(defn create-rooms []
  (vec
    (for [row (range size)]
      (vec
        (for [col (range size)]
          {:row row, :col col, :visited? false, :bottom? true, :right? true :start? false :end? false})))))

(defn possible-neighbors [rooms row col]
  (vec
    (filter
      (fn [room]
        (and room (= false (:visited? room))))
      [(get-in rooms [(dec row) col])
       (get-in rooms [(inc row) col])
       (get-in rooms [row (dec col)])
       (get-in rooms [row (inc col)])])))

(defn random-neighbor [rooms row col]
  (let [neighbors (possible-neighbors rooms row col)]
    (if (pos? (count neighbors))
      (rand-nth neighbors)
      nil)))

(defn tear-down-wall [rooms old-row old-col new-row new-col]
  (cond
    ; going up
    (< new-row old-row)
    (assoc-in rooms [new-row new-col :bottom?] false)
    ; going down
    (> new-row old-row)
    (assoc-in rooms [old-row old-col :bottom?] false)
    ; going left
    (< new-col old-col)
    (assoc-in rooms [new-row new-col :right?] false)
    ; going right
    (> new-col old-col)
    (assoc-in rooms [old-row old-col :right?] false)))

(declare create-maze)

(defn create-maze-loop [rooms old-row old-col new-row new-col]
  (let [new-rooms (tear-down-wall rooms old-row old-col new-row new-col)
        new-rooms (create-maze new-rooms new-row new-col)]
    (if (= rooms new-rooms)
      rooms
      (create-maze-loop new-rooms old-row old-col new-row new-col))))

(defn create-maze [rooms row col first?]
  (let [rooms (assoc-in rooms [row col :visited?] true)
        next-room (random-neighbor rooms row col)
        rooms (if first?
                (assoc-in rooms [row col :start?] true) rooms)]

    (if next-room
      (loop [old-rooms (tear-down-wall rooms row col (:row next-room) (:col next-room))]
        (let [new-rooms (create-maze old-rooms (:row next-room) (:col next-room) false)]
          (if (= old-rooms new-rooms)
            old-rooms
            (recur new-rooms))))
      (if (< (count (filter :end? (flatten rooms))) 1)
        (assoc-in rooms [row col :end?] true)
        rooms))))


(defn print-room [room]
  ; changes based on start / bottom
  (cond
    (and (:bottom? room) (:start? room)) (print ">")
    (and (:bottom? room) (not (:start? room))) (print"_")
    (and (not (:bottom? room)) (:start? room)) (print"v")
    :else (print" "))
  ;(if (:bottom? room)
  ;  (print "_")
  ;  (print " "))
  ; gotta always do
  (if (:right? room)
    (print "|")
    (print " ")))

; if we have bottom and it is a start then ⍜
; if we have NO bottom and it is a start then o

(defn -main [& args]
  (let [rooms (create-rooms)
        rooms (create-maze rooms 0 0 true)]
    ; print top walls
    (doseq [row rooms]
      (print " _"))
    (println)
    ; print grid
    (doseq [row rooms]
      ;print walls
      (print "|")
      (doseq [room row]
        ;eliminated previous conditional, added one here
        (print (str (cond
                      (:start? room) "o" (:end? room) "x"
                      (:bottom? room) "_" :else " ")
                    (if (:right? room) "|" " "))))
      (println))))

