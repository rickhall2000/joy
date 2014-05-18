(ns joy.ch12
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [com.sun.net.httpserver HttpHandler HttpExchange
            HttpServer]
           [java.net InetSocketAddress URLDecoder URI]
           [java.io File FilterOutputStream]
           [java.util Comparator Collections ArrayList]))

(def OK java.net.HttpURLConnection/HTTP_OK)

(defn respond
  ([exchange body]
     (respond identity exchange body))
  ([around exchange body]
     (.sendResponseHeaders exchange OK 0)
     (with-open [resp (around (.getResponseBody exchange))]
       (.write resp (.getBytes body)))))

(defn new-server [port path handler]
  (doto
      (HttpServer/create (InetSocketAddress. port) 0)
    (.createContext path handler)
    (.setExecutor nil)
    (.start)))

(defn default-handler [txt]
  (proxy (HttpHandler)
      []
    (handle [exchange]
      (respond exchange txt))))

#_(def server
  (new-server
   8123
   "/joy/hello"
   (default-handler "Hello Atlanta")))

(comment

 (.stop server 0)

 )

(def p (default-handler
         "There's no problem that can't be solved with another level of indirection"))


(def server (new-server 8123 "/" p))

(update-proxy p
              {"handle" (fn [this exchange]
                          (respond exchange (str "this is" this)))})

(def echo-handler
  (fn [_ exchange]
    (let [headers (.getRequestHeaders exchange)]
      (respond exchange (prn-str headers)))))

(update-proxy p {"handle" echo-handler})

(defn html-around [o]
  (proxy [FilterOutputStream]
      [o]
    (write [raw-bytes]
      (proxy-super write
                   (.getBytes (str "<html><body"
                                   (String. raw-bytes)
                                   "</body></html>"))))))

(defn listing [file]
  (-> file .list sort))

(listing (io/file "."))

(listing (io/file "./README.md"))

(defn html-links [root filenames]
      (string/join
       (for [file filenames]
         (str "<a href='"
              (str root
                   (if (= "/" root)
                     ""
                     File/separator)
                   file)
              "'>"
              file "</a><br>"))))

(html-links "." (listing (io/file ".")))

(defn details [file]
  (str (.getName file) " is "
       (.length file) " bytes"))

(details (io/file "./README.md"))

(defn uri->file [root uri]
  (->> uri
       str
       URLDecoder/decode
       (str root)
       io/file))

(uri->file "." (URI. "/project.clj"))
(details (uri->file "." (URI. "/project.clj")))

(def fs-handler
  (fn [_ exchange]
    (let [uri (.getRequestURI exchange)
          file (uri->file "." uri)]
      (if (.isDirectory file)
        (do (.add (.getResponseHeaders exchange)
                  "Content-Type" "text/html")
            (respond html-around
                     exchange
                     (html-links (str uri) (listing file))))
        (respond exchange (details file))))))

(update-proxy p {"handle" fs-handler})

;; 12.3 Arrays
(doto (StringBuilder. "abc")
  (.append (into-array [\x \y \z])))

(doto (StringBuilder. "abc")
  (.append (char-array [\x \y \z])))

(let [ary (make-array Long/TYPE 3 3)]
  (dotimes [i 3]
    (dotimes [j 3]
      (aset ary i j (+ i j))))
  (map seq ary))

(into-array ["a" "b" "c"])

(into-array [(java.util.Date.) (java.sql.Time. 0)])

#_(into-array ["a" "b" 1M])

(into-array Number [1 2.0 3M 4/5])

(to-array-2d [[1 2 3]
              [4 5 6]])

(to-array ["a" 1M #(proxy [Object] [])])

(to-array [1 (int 2)])

(def ary (into-array [1 2 3]))
(def sary (seq ary))
sary

(aset ary 0 42)
sary

(defn asum-sq [xs]
  (let [dbl (amap xs i ret
                  (* (aget xs i)
                     (aget xs i)))]
    (areduce dbl i ret 0
             (+ ret (aget dbl i)))))

(asum-sq (double-array [1 2 3 4 5]))

(defmulti what-is class)
(defmethod what-is
  (Class/forName "[Ljava.lang.String;")
  [_]
  "1d String")

(defmethod what-is
  (Class/forName "[[Ljava.lang.Object;")
  [_]
  "2d Object")

(defmethod what-is
  (Class/forName "[[[[I")
  [_]
  "Primitive 4d int")

(defmethod what-is (Class/forName "[[D")
  [a]
  "Primative 2d double")

(defmethod what-is (Class/forName "[Lclojure.lang.PersistentVector;")
  [a]
  "1d Persistent Vector")



(what-is (into-array ["a" "b"]))
(what-is (to-array-2d [[1 2] [3 4]]))
(what-is (make-array Integer/TYPE 2 2 2 2))

(what-is (into-array (map double-array [[1.0] [2.0]])))
(what-is (into-array [[1.0] [2.0]]))

(String/format "An int %d and a String %s"
               (to-array [99 "luftballons"]))

;; 12.4
(ancestors (class #()))

(defn gimmie [] (ArrayList. [1 3 4 8 2]))

(doto (gimmie)
  (Collections/sort (Collections/reverseOrder)))

(doto (gimmie)
  (Collections/sort
   (reify Comparator
     (compare [this l r]
       (cond
        (> l r) -1
        (= l r) 0
        :else 1)))))

(doto (gimmie) (Collections/sort #(compare %2 %1)))
(doto (gimmie) (Collections/sort >))
(doto (gimmie) (Collections/sort <))
(doto (gimmie) (Collections/sort (complement <)))
