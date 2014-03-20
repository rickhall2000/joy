(ns joy.ch9
  (:refer-clojure :exclude [get]))

(defn beget [this proto]
  (assoc this ::prototype proto))

(beget {:sub 0} {:super 1})

(defn get [m k]
  (when m
    (if-let [[_ v] (find m k)]
      v
      (recur (::prototype m) k))))

(get (beget {:sub 0} {:super 1}) :super)

(def put assoc)

(def cat {:likes-dogs true, :ocd-bathing true})
(def morris (beget {:likes-9lives true} cat))
(def post-traumatic-morris (beget {:likes-dogs nil} morris))

(get cat :likes-dogs)
(get morris :likes-dogs)
(get post-traumatic-morris :likes-dogs)
(get post-traumatic-morris :likes-9lives)

(defmulti compiler :os)
(defmethod compiler ::unix [m] (get m :c-compiler))
(defmethod compiler ::osx [m] (get m ::llvm-compiler))

(def clone (partial beget {}))
(def unix {:os ::unix :c-compiler "cc", :home "/home" :dev "/dev"})
(def osx (-> (clone unix)
             (put :os ::osx)
             (put ::llvm-compiler "clang")
             (put :home "/Users")))

(compiler unix)
(compiler osx)

(defmulti home :os)
(defmethod home ::unix [m] (get m :home))
(home unix)
(derive ::osx ::unix)
(home osx)

(parents ::osx)
(isa? ::osx ::unix)
(isa? ::unix ::osx)
(derive ::osx ::bsd)
(defmethod home ::bsd [m] "/home")
(prefer-method home ::unix ::bsd)
(home osx)

(derive (make-hierarchy) ::osx ::unix)

(defmulti compile-cmd (juxt :os compiler))

(defmethod compile-cmd [::osx "clang"] [m]
  (str "/usr/bin" (get m :c-compiler)))

(defmethod compile-cmd :default [m]
  (str "Unsure where to locate " (get m :c-compiler)))

(compile-cmd osx)

((juxt :os compiler) osx)

(defrecord TreeNode [val l r])
(TreeNode. 5 nil nil)

(defn xconj [t v]
  (cond
   (nil? t) (TreeNode. v nil nil)
   (< v (:val t)) (TreeNode. (:val t) (xconj (:l t) v) (:r t))
   :else (TreeNode. (:val t) (:l t) (xconj (:r t) v))))

(defn xseq [t]
  (when t
    (concat (xseq (:l t)) [(:val t)] (xseq (:r t)))))

(def sample-tree (reduce xconj nil [3 5 2 4 6]))
(xseq sample-tree)

(defprotocol FIXO
  (fixo-push [fixo value])
  (fixo-pop [fixo])
  (fixo-peek [fixo]))


(extend-type TreeNode
  FIXO
  (fixo-push [node value]
             (xconj node value)))

(xseq (fixo-push sample-tree 5/2))

(extend-type clojure.lang.IPersistentVector
  FIXO
  (fixo-push [vector value]
             (conj vector value)))

(fixo-push [2 3 4 5 6] 5/2)

(use 'clojure.string)
(defprotocol StringOps (rev [s]) (upp [s]))

(extend-type String
  StringOps
  (rev [s] (clojure.string/reverse s)))

(rev "works")

(extend-type String
  StringOps
  (upp [s] (clojure.string/upper-case s)))

(upp "works")

(def rev-mixin {:rev clojure.string/reverse})
(def upp-mixin {:upp (fn [this] (.toUpperCase this))})
(def fully-mixed (merge upp-mixin rev-mixin))
(extend String StringOps fully-mixed)

(extend-type nil
  FIXO
  (fixo-push [t v]
             (TreeNode. v nil nil)))

(xseq (reduce fixo-push nil [3 5 2 4 6 0]))

(extend-type TreeNode
  FIXO
  (fixo-push [n v]
             (xconj n v))
  (fixo-peek [n]
             (if (:l n)
               (recur (:l n))
               (:val n)))
  (fixo-pop [node]
            (if (:l node)
              (TreeNode. (:val node) (fixo-pop (:l node)) (:r node))
              (:r node))))

(extend-type clojure.lang.IPersistentVector
  FIXO
  (fixo-push [vector value]
             (conj vector value))
  (fixo-peek [vector]
             (peek vector))
  (fixo-pop [vector]
            (pop vector)))

(defn fixo-into [c1 c2]
  (reduce fixo-push c1 c2))

(xseq (fixo-into (TreeNode. 5 nil nil) [2 4 6 7]))
(seq (fixo-into [5] [2 4 6 7]))

(def tree-node-fixo
  {:fixo-push (fn [node value]
                (xconj node value))
   :fixo-peek (fn [node]
                (if (:l node)
                  (recur (:l node))
                  (:val node)))
   :fixo-pop (fn [node]
               (if (:l node)
                 (TreeNode. (:val node)
                            (fixo-pop (:l node)) (:r node))
                 (:r node)))})

(extend TreeNode FIXO tree-node-fixo)

(xseq (fixo-into (TreeNode. 5 nil nil) [2 4 6 7]))

(defn fixed-fixo
  ([limit] (fixed-fixo limit []))
  ([limit vector]
   (reify FIXO
     (fixo-push [this value]
                (if (< (count vector) limit)
                  (fixed-fixo limit (conj vector value))
                  this))
     (fixo-peek [_]
                (peek vector))
     (fixo-pop [_]
               (pop vector)))))

;; 9.4
