(ns joy.ch13
  (:require [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [clojure.walk :refer [prewalk]]
            [clojure.pprint :refer [pprint]]))

(def code-string "(defn hello[a] (js/alert (pr-str 'greetings x)))")

(def code-data (read-string code-string))
code-data

(first code-string)
(first code-data)

(def ast (ana/analyze (ana/empty-env) code-data))
ast

(keys ast)

(defn print-ast [ast]
  (pprint
   (prewalk
    (fn [x]
      (if (map? x)
        (select-keys x [:children :name :form :op])
        x))
    ast)))


(print-ast ast)

(comp/emit ast)
