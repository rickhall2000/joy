(ns joy.externs-for-cljs
  (:require [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [clojure.walk :refer [prewalk]]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io])
  (:import (clojure.lang LineNumberingPushbackReader)))

(defn read-file
  [filename]
  (let [eof (Object.)]
    (with-open [reader (LineNumberingPushbackReader.
                        (io/reader filename))]
      (doall
       (take-while #(not= % eof)
                   (repeatedly #(read reader false eof)))))))

(defn file-ast
  [filename]
  (binding [ana/*cljs-ns* 'cljs.user
            ana/*cljs-file* filename]
    (mapv #(ana/analyze (ana/empty-env) %)
          (read-file filename))))

(count (file-ast "src/cljs/joy/music.cljs"))

(defn flatten-ast [ast]
  (mapcat #(tree-seq :children :children %) ast))

(def flat-ast (flatten-ast (file-ast "src/cljs/joy/music.cljs")))

(count flat-ast)

(defn get-interop-used
  [flat-ast]
  (set (keep #(some % [:method :field]) flat-ast)))

(get-interop-used flat-ast)

(defn externs-for-interop [syms]
  (apply str
         "var DummyClass={};\n"
         (map #(str "DummyClass." % "=function(){};\n")
              syms)))

(spit "externs.js"
      (externs-for-interop
       (get-interop-used flat-ast)))
