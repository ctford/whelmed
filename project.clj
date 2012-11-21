(defproject whelmed "0.1.0-SNAPSHOT"
  :description "Whelmed. Not overwhelmed. Not underwhelmed. Just whelmed."
  :url "http://github.com/ctford/whelmed"
  :main ^{:skip-aot true} whelmed.core
  :dependencies	[
    [org.clojure/clojure "1.4.0"]
    [overtone "0.7.1" :exclusions [seesaw]]
    [leipzig "0.1.0" :exclusions [seesaw]]
    [org.clojure/math.numeric-tower "0.0.1"]])
