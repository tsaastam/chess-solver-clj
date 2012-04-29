(ns chess.test.core
  (:use [chess.core])
  (:use [clojure.test]))

(deftest test-get-board
  (let [locs {[0 0] :R, [2 2] :R, [1 1] :N, [1 3] :N, [3 1] :N, [3 3] :N}
	bs [4 4]]
    (is (= (get-board bs locs)
	   [["R" nil nil nil]
	    [nil "N" nil "N"]
	    [nil nil "R" nil]
	    [nil "N" nil "N"]])))
  (let [locs {[0 0] :R, [2 2] :R, [1 1] :N, [1 3] :N, [1 4] :B}
	bs [3 5]]
    (is (= (get-board bs locs)
	   [["R" nil nil]
	    [nil "N" nil]
	    [nil nil "R"]
	    [nil "N" nil]
            [nil "B" nil]]))))

(deftest test-print-board
  (let [locs {[0 0] :R, [2 2] :R, [1 1] :N, [1 3] :N, [3 1] :N, [3 3] :N}
	bs [4 4]]
    (is (= (print-board (get-board bs locs))
	   "R...\n.N.N\n..R.\n.N.N"))))

(deftest test-remove-nonexistent-squares
  ;; 1x1: .
  (is (= [[0 0]]
         (remove-nonexistent-squares
          [[-1 0] [0 -1] [-1 -1] [0 0]]
          [1 1])))
  ;; 3x1: ...
  (is (= [[0 0] [1 0] [2 0]]
         (remove-nonexistent-squares
          [[-1 0] [0 -1] [-1 -1] [0 0] [1 0] [2 0] [3 0] [0 1] [0 2]]
          [3 1])))
  ;; 1x3: .
  ;;      .
  ;;      .
  (is (= [[0 0] [0 1] [0 2]]
         (remove-nonexistent-squares
          [[-1 0] [0 -1] [-1 -1] [0 0] [1 0] [2 0] [3 0] [0 1] [0 2]]
          [1 3])))
  ;; 2x2: ..
  ;;      ..
  (is (= [[0 0] [1 0] [0 1] [1 1]]
         (remove-nonexistent-squares
          [[-1 0] [0 -1] [-1 -1] [-123 0] [0 -546] [-595 -354]
           [0 0] [1 0] [2 0] [0 1] [1 1] [2 1] [0 2] [1 2] [2 2] ;; all 3x3 squares
           [123 456]]
          [2 2]))))

(deftest test-squares-threatened-by
  (is (= (set (squares-threatened-by [[2 2] :Q] [5 5]))
	 (set [[0 0] [0 2] [0 4] [1 1] [1 2] [1 3] [2 0] [2 1] [2 3] [2 4] [3 1] [3 2] [3 3] [4 0] [4 2] [4 4]])))
  (is (= (set (squares-threatened-by [[0 0] :K] [3 3]))
	 (set [[0 1] [1 0] [1 1]])))
  (is (= (set (squares-threatened-by [[2 1] :R] [3 3]))
	 (set [[2 2] [2 0] [0 1] [1 1]])))
  (is (= (set (squares-threatened-by [[1 2] :N] [4 4]))
	 (set [[2 0] [3 3] [3 1] [0 0]])))
  (is (= (set (squares-threatened-by [[1 2] :B] [4 5]))
	 (set [[0 1] [2 3] [3 4] [2 1] [0 3] [3 0]]))))

(deftest test-get-initial-free-squares
  (let [frees (get-initial-free-squares [2 2] [:K :R :B])]
    (is (= (set [:K :R :B]) (set (keys frees))))
    (is (every? true?
		(for [[_ squares] frees]
		  (= (set [[0 0] [0 1] [1 0] [1 1]]) squares)))))
  (let [frees (get-initial-free-squares [4 4] [:Q :K :R :N :B])]
    (is (= (set [:Q :K :R :N :B]) (set (keys frees))))
    (is (every? true?
		(for [[_ squares] frees]
		  (= (set [[0 0] [1 0] [2 0] [3 0]
			   [0 1] [1 1] [2 1] [3 1]
			   [0 2] [1 2] [2 2] [3 2]
			   [0 3] [1 3] [2 3] [3 3]])
		     squares))))))

(deftest test-get-piece-square-combinations
  (is (= (set (get-piece-square-combinations :Q [[0 0] [0 1] [1 0]] 2))
	 (set [{[0 1] :Q, [0 0] :Q} {[1 0] :Q, [0 0] :Q} {[1 0] :Q, [0 1] :Q}]))))

(deftest test-remove-threatening-squares
  (is (= (remove-threatening-squares [3 3] {[0 0] :Q} #{[2 1] [1 2]} :N) ;; #{} since knight on [2 1] or [1 2] threatens queen at [0 0]
	 #{}))
  (is (= (remove-threatening-squares [3 3] {[0 0] :Q} #{[2 1] [1 2]} :B) ;; #{[2 1] [1 2]} - bishop doesn't threaten queen at [0 0]
	 #{[1 2] [2 1]})))

(defn update-frees [bs loc-piece pieces]
  (let [locvec (first (vec loc-piece))
	locset (set (list (first locvec)))
	frees (get-initial-free-squares bs pieces)
	threateneds (into locset (squares-threatened-by locvec bs))]
    (update-free-squares bs loc-piece frees threateneds pieces)))

(deftest test-update-free-squares
  (is (= (set (update-frees [4 4] {[2 1] :N} [:Q :B :R :K]))
	 (set {:Q #{}, :B #{[2 2] [1 1] [2 3] [0 1] [3 1] [2 0]}, :R #{[3 2] [1 0] [1 2] [0 3] [3 0]}, :K #{[2 3] [0 1] [0 3]}})))
  (is (= (set (update-frees [3 3] {[2 1] :N} [:R :B :K]))
	 (set {:R #{[1 0] [1 2]}, :B #{[2 2] [1 1] [0 1] [2 0]}, :K #{[0 1]}})))
  (is (= (set (update-frees [3 3] {[2 1] :N} [:R :B :K :Q]))
	 (set {:R #{[1 0] [1 2]}, :B #{[2 2] [1 1] [0 1] [2 0]}, :K #{[0 1]}, :Q #{}})))
  (is (= (set (update-frees [3 3] {[0 2] :N} [:R :B :K]))
	 (set {:R #{[1 1] [2 0]}, :B #{[2 2] [0 0] [0 1] [1 2]}, :K #{[2 2] [0 0] [2 0]}})))
  (is (= (set (update-frees [3 3] {[0 2] :N} [:R :B :K :Q]))
	 (set {:R #{[1 1] [2 0]}, :B #{[2 2] [0 0] [0 1] [1 2]}, :K #{[2 2] [0 0] [2 0]}, :Q #{}}))))

(deftest test-get-moves
  (let [bs [3 3], loc {[2 1] :N}, pieces [:R :B :K :Q], frees-now (update-frees bs loc pieces)]
    (is (= (get-moves bs loc frees-now {:R 1, :B 1, :K 1, :Q 1}) ;; empty since queen cannot be placed
	   [])))
  (let [bs [4 4], loc {[2 1] :N}, pieces [:R :B :K :Q], frees-now (update-frees bs loc pieces)]
    (is (= (get-moves bs loc frees-now {:R 1, :B 1, :K 1, :Q 1}) ;; empty since queen cannot be placed
	   [])))
  (let [bs [3 3], loc {[2 1] :N}, pieces [:B], frees-now (update-frees bs loc pieces)
	con (get-moves bs loc frees-now {:B 2})]
    (is (= (map #(nth % 0) con)
	   (repeat (count con) :B)))
    (is (= (set (map #(nth % 1) con))
	   (set [{[2 1] :N, [0 1] :B, [2 2] :B}
		 {[2 1] :N, [2 0] :B, [2 2] :B}
		 {[2 1] :N, [0 1] :B, [1 1] :B}
		 {[2 1] :N, [2 0] :B, [0 1] :B}])))
    (is (every? empty? (map #(nth % 2) con)))) ;; all pieces done
  (let [bs [3 3], loc {[2 1] :N}, pieces [:K], frees-now (update-frees bs loc pieces)
	con (get-moves bs loc frees-now {:K 1})]
    (is (= (map #(nth % 0) con)
	   (repeat (count con) :K)))
    (is (= (set (map #(nth % 1) con))
	   (set [{[2 1] :N, [0 1] :K}]))) ;; only one possible spot for king
    (is (every? empty? (map #(nth % 2) con))))
  (let [bs [3 3], loc {[2 1] :N}, pieces [:K], frees-now (update-frees bs loc pieces)
	con (get-moves bs loc frees-now {:K 2})]
    (is (= con []))) ;; empty since only room for one king
  (let [bs [3 3], loc {[2 1] :N}, pieces [:R], frees-now (update-frees bs loc pieces)
	con (get-moves bs loc frees-now {:R 1})]
    (is (= (map #(nth % 0) con)
	   (repeat (count con) :R)))
    (is (= (set (map #(nth % 1) con))
	   (set [{[2 1] :N, [1 2] :R}
		 {[2 1] :N, [1 0] :R}])))
    (is (every? empty? (map #(nth % 2) con))))
  (let [bs [3 3], loc {[2 1] :N}, pieces [:R], frees-now (update-frees bs loc pieces)
	con (get-moves bs loc frees-now {:R 2})]
    (is (= con []))) ;; empty since although room for both rooks, they would threaten each other
  (let [bs [3 3], loc {[2 1] :N}, pieces [:Q], frees-now (update-frees bs loc pieces)
	con (get-moves bs loc frees-now {:Q 1})]
    (is (= con []))) ;; empty, no room for queen
  (let [bs [3 3], loc {[2 1] :N}, pieces [:R :K], frees-now (update-frees bs loc pieces)
	con (get-moves bs loc frees-now {:R 1, :K 1})]
    (is (= con []))) ;; empty, room for both but they would threaten each other
  )

(deftest test-solve-rec
  (is (= (solve-rec [3 3] {[2 1] :K} {:R #{[0 0] [0 2]}} {:R 2})
	 [])) ;; rooks will threaten each other
  (is (= (solve-rec [3 3] {[2 1] :K} {:R #{[0 0] [0 2]}} {:R 1})
	 [1 1])) ;; one rook is fine though. it can be on either square
  (is (= (solve-rec [3 3] {} (get-initial-free-squares [3 3] [:K :R]) {:K 2, :R 1})
	 [1 1 1 1])) ;; example 1 from task spec
  (is (= (solve-rec [3 3] {} (get-initial-free-squares [3 3] [:Q :N]) {:N 1, :Q 1})
	 [])) ;; after first piece, no room for other
  (is (= (solve-rec [3 3] {} (get-initial-free-squares [3 3] [:Q :N]) {:Q 1, :N 1})
	 [])) ;; same
  (is (= (solve-rec [3 3] {} (get-initial-free-squares [3 3] [:Q :N :R]) {:Q 1, :N 1, :R 1})
	 [])) ;; adding more pieces => even more false ;)
  (is (= (solve-rec [3 3] {} (get-initial-free-squares [3 3] [:Q :R]) {:Q 1, :R 2})
	 [1 1 1 1])) ;; this is fine though
  (is (= (solve-rec [3 3] {} (get-initial-free-squares [3 3] [:N :R]) {:N 3, :R 1})
	 [1 1 1 1])) ;; also fine
  (is (= (solve-rec [4 4] {} (get-initial-free-squares [4 4] [:R :N]) {:R 2, :N 4})
	 [1 1 1 1 1 1 1 1])) ;; example 2 from task spec
  (is (= (solve-rec [3 3] {[1 1] :K} {:R #{}} {:R 1})
	 [])) ;; no free squares
  (is (= (solve-rec [3 3] {[0 0] :K} {:R #{[2 1] [1 2] [2 2]}} {:R 4})
	 [])) ;; not enough free squares
  )

(deftest test-solve
  (is (= (solve [3 3] {:K 1, :R 2}) 4))
  (is (= (solve [4 4] {:R 2, :N 4}) 8))
  (is (= (solve [4 5] {:Q 1, :R 1, :N 3}) 16)))

(run-tests 'chess.test.core)
