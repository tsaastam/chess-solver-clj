(ns chess.main
  (:gen-class)
  (:use [chess.core])
  (:use [clojure.contrib.command-line]))

(defn -main [& args]
  (with-command-line args
    "this is a text"
    [[size "board size, e.g. '[3 3]'" nil]
     [pieces "piece counts, e.g. '{:K 2, :Q 1}'. Pieces are:\n:K king, :Q queen, :R rook, :N knight, :B bishop" nil]
     remaining]
    (if (or (= size nil)
	    (= pieces nil))
	(println "Usage: <this> -size [size] -pieces [pieces]\n  e.g. <this> -size '[4 4]' -pieces '{:R 2, :N 4}'")
	(let [bs (eval (read-string size))
	      pc (eval (read-string pieces))]
	  (solve bs pc)))))
