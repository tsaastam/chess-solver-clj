(defproject chess "0.3.9.2-BETA"
  :description "Chess problem solver, by Taneli Saastamoinen"
  :main chess.main
  :dependencies [[org.clojure/clojure "1.2.0"]
		 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies [[swank-clojure "1.3.2"]]
  :jvm-opts ["-Dfile.encoding=UTF8"])
