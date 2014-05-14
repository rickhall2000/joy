(ns joy.ch12
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [com.sun.net.httpserver HttpHandler HttpExchange
            HttpServer]
           [java.net InetSocketAddress URLDecoder URI]
           [java.io File FilterOutputStream]))

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
