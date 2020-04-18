(defproject baseline "0.1.0-SNAPSHOT"
  :description "A program to measure the Clojure runtime startup cost"
  :url "https://github.com/kkredit/scramble-squares-solver"
  :license {:name "MIT License"
            :url "https://mit-license.org/"}
  :dependencies [[org.clojure/clojure "1.8.0"] [org.clojure/tools.trace "0.7.10"]]
  :main baseline.core
  :aot [baseline.core])
