(defproject fn.trace "1.4.0-SNAPSHOT"
  :description "A clojure trace library similar to clojure.contrib.trace but adds some flexibility."
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [weissjeffm/clojure.prxml "1.3.0-SNAPSHOT"]]
  :dev-dependencies [[clj-stacktrace "0.2.4"]]
  :jvm-opts ["-Xmx128m"])
