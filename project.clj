(defproject whelmed "0.1.0-SNAPSHOT"
  :description "Whelmed. Not overwhelmed. Not underwhelmed. Just whelmed."
  :url "http://github.com/ctford/whelmed"
  :main ^{:skip-aot true} whelmed.play
  :jvm-opts ^:replace  []
  :dependencies	[
    [org.clojure/clojure "1.8.0"]
    [overtone "0.10.3"]
    [leipzig "0.11.0-SNAPSHOT"]])
