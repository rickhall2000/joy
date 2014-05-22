(defproject joy "0.1.0-SNAPSHOT"
  :description "scratch pad as I work through the joy of clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-1835"]]
  :plugins [[lein-cljsbuild "0.3.2"]]
  :cljsbuild
  {:builds
   [{:source-paths ["src/cljs"]
     :compiler
     {:output-to "dev-target/all.js"
      :optimizations :whitespace
      :pretty-print true}}]})
