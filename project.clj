(defproject harald "0.1.0-SNAPSHOT"
  :description "Game engine for Harald."
  :url "https://alphajuliet.com/ns/harald/"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [spec-dict "0.2.1"]
                 [random-seed "1.0.0"]]
  :repl-options {:init-ns harald.core})
