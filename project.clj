(defproject bf-interpreter "0.1.0-SNAPSHOT"
  :description "Brainfuck interpreter."
  :url "https://github.com/mvukic/bf-interpreter-clojure"
  :license {:name "MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot bf-interpreter.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
