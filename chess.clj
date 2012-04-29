(ns chess.core
  (:require [clojure.string :as str]
            [clojure.set :as set]
	    [clojure.contrib.combinatorics :as combi]))

;;; for debugging / status reportage
(def call-count
  (atom 0))
(def solution-count
  (atom 0))
(def start-time
  (atom 0))

;;; human-readability / pretty-printing functions
(defn get-board [[bx by] loc-pieces]
  (partition bx
	     (for [j (range by)
		   i (range bx)] ;; x first
	       (when-let [a (get loc-pieces [i j])]
		 (name a))))) ;; when :X => "X", otherwise nil

(defn print-board [board]
  (str/join "\n" (for [row board]
                   (str/join "" (for [cell row]
                                  (if cell cell "."))))))

(defn pb [bs locs]
  (-> (get-board bs locs) (print-board) (println)))

;;; core functions

(defn remove-nonexistent-squares [squares [bx by]]
  (for [[x y] squares :when (and (>= x 0) (>= y 0) (< x bx) (< y by))]
    [x y]))

(defn squares-threatened-by [[location piece] board-size]
  (let [[x y] location
        [bx by] board-size]
    (condp = piece
      :K (let [neighbours (remove (partial = [0 0]) (for [i [-1 0 1] j [-1 0 1]] [i j]))
               king-squares (for [[nx ny] neighbours]
			      [(+ x nx) (+ y ny)])]
           (remove-nonexistent-squares king-squares board-size))
      :R (let [lines (into (for [i (range bx)] [i y]) (for [j (range by)] [x j]))
               rook-squares (remove (partial = location) lines)]
           rook-squares) ; construction doesn't generate nonexistents
      :N (let [xs [1 1 2 2 -1 -1 -2 -2]
	       ys [2 -2 1 -1 2 -2 1 -1]
	       coord-adds (map list xs ys)
	       knight-squares (for [[xa ya] coord-adds]
				[(+ x xa) (+ y ya)])]
	   (remove-nonexistent-squares knight-squares board-size))
      :B (let [coord-adds (for [i (range 1 (max bx by))] [i i])
	       bishop-squares (apply concat
				     (for [[xa ya] coord-adds]
				       (map #(list (%1 x xa) (%2 y ya)) [+ + - -] [+ - + -])))]
	   (remove-nonexistent-squares bishop-squares board-size))
      :Q (let [rooks (squares-threatened-by [location :R] board-size)
	       bishs (squares-threatened-by [location :B] board-size)]
	   (into rooks bishs)) ;; rooks, bishs are distinct by construction
      ;; else:
      (str "Error: unknown piece " piece))))

(defn remove-threatening-squares [board-size loc-pieces free-square-set piece]
  (set
   (let [locs (set (keys loc-pieces))]
     (for [square free-square-set
	   :let [threateneds (set (squares-threatened-by [square piece] board-size))]
	   :when (empty? (set/intersection locs threateneds))]
       square))))

(defn update-free-squares [board-size loc-pieces free-squares non-frees piece-types]
  (into {}
        (for [[piece frees] free-squares
              :let [rem-frees (set/difference frees non-frees)]]
          {piece (remove-threatening-squares board-size loc-pieces rem-frees piece)})))

(defn get-piece-square-combinations [piece squares piece-count]
  (let [pieces (repeat piece-count piece)
	square-combs (combi/combinations squares piece-count)]
    (for [comb square-combs]
      (zipmap comb pieces))))

(defn get-moves-for-piece [board-size loc-pieces free-squares piece-counts piece]
  (let [frees (get free-squares piece)
	piece-count (get piece-counts piece)
	piece-square-combos (get-piece-square-combinations piece frees piece-count)
	rem-piece-counts (dissoc piece-counts piece)
	rem-free-squares (dissoc free-squares piece)]
    (for [attempt piece-square-combos
	  :let [squares (keys attempt)
		locs-now (into loc-pieces attempt)
		loc-set (set (keys locs-now))
		threatened-now (set (apply concat (for [loc-piece attempt]
                                                    (squares-threatened-by
                                                     loc-piece board-size))))]
          ;; cannot place pieces so they threaten each other/existing pieces
	  :when (empty? (set/intersection loc-set threatened-now))
	  :let [non-free-now (set/union loc-set threatened-now)
		free-now (update-free-squares board-size locs-now rem-free-squares
                                              non-free-now (keys rem-piece-counts))]
          ;; require valid squares for all remaining pieces
	  :when (every? pos? (map count (vals free-now)))]
      [piece locs-now free-now])))

(def calc-method :min-piece-count) ;; choose piece of which we have the fewest; e.g. {:Q 1, :K 2} => select queen
;;(def calc-method :min-free-squares) ;; choose piece with fewest free valid squares
;;(def calc-method :min-overall) ;; choose piece which results in fewest moves on the next round. this will calculate placings for every piece, which prunes the best, but is very slow (too slow)

(defn get-moves [board-size loc-pieces free-squares piece-counts]
  (if (some empty? (vals free-squares))
    ()
    (condp = calc-method
      :min-piece-count
      (let [piece (first (apply min-key second piece-counts))]
        (get-moves-for-piece board-size loc-pieces free-squares piece-counts piece))
      :min-free-squares
      (let [piece (first (apply min-key (comp count second) free-squares))]
        (get-moves-for-piece board-size loc-pieces free-squares piece-counts piece))
      :min-overall
      (let [all-options
            (for [[piece _] free-squares]
              (get-moves-for-piece board-size loc-pieces free-squares piece-counts piece))]
        (apply min-key count all-options)))))

(defn handle-solution [board-size loc-pieces]
  (do
    (swap! solution-count inc)
    (let [total-time (- (System/currentTimeMillis) @start-time)
          sps (* 1000 (/ @solution-count total-time))
          sols-per-s (double (/ (int (* 10000 sps)) 10000))
          board (print-board (get-board board-size loc-pieces))
          message (str "\n" "call-count " @call-count ", solutions/s " sols-per-s ", solution #" @solution-count ":\n" board)]
      (println message)
      [1]))) ;; could also return the solution, whichever

(defn solve-rec [board-size loc-pieces free-squares piece-counts]
  (swap! call-count inc) ;; for debug only
  (if (empty? piece-counts)
    (handle-solution board-size loc-pieces)
    (let [moves (get-moves board-size loc-pieces free-squares piece-counts)]
      (if (empty? moves)
	()
        (apply concat
               (for [[piece locs-now frees-now] moves
                     :let [rem-counts (dissoc piece-counts piece)
                           rec-result (solve-rec board-size locs-now frees-now rem-counts)]
                     :when (not (empty? rec-result))]
                 rec-result))))))

(defn get-initial-free-squares [[bx by] piece-types]
  (let [all-squares (set (for [i (range bx), j (range by)] [i j]))]
    (into {} (for [piece piece-types] {piece all-squares}))))

(defn solve [board-size piece-counts]
  ;; reset bookkeeping
  (swap! call-count (constantly 0))
  (swap! solution-count (constantly 0))
  (swap! start-time (fn [& ö] (System/currentTimeMillis)))
  ;; then solve
  (let [initial-frees (get-initial-free-squares board-size (keys piece-counts))
	result (solve-rec board-size {} initial-frees piece-counts)
	solution-count (count result)]
    (println (str "\n" "total number of solutions: " solution-count "\n"))
    solution-count))

;;; eof
