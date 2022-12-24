(ns utils.bimap)

(defprotocol ReversibleMap
  (rmap [m]))

(defn- rdissoc [d r v]
  (if-let [[_ k] (find r v)] (dissoc d k) d))

(deftype Bimap [^clojure.lang.IPersistentMap direct reverse]
  Object
  (hashCode [x]
    (.hashCode direct))
  (equals [x y]
    (.equals direct y))
  clojure.lang.IPersistentMap
  (without [this k]
    (Bimap.
     (dissoc direct k)
     (rdissoc reverse direct k)))
  (assocEx [this k v]
    (if (or (contains? direct k) (contains? reverse v))
      (throw (Exception. "Key or value already present"))
      (assoc this k v)))
  java.lang.Iterable
  (iterator [this]
    (.iterator direct))
  clojure.lang.Associative
  (assoc [this k v]
    (Bimap.
     (assoc (rdissoc direct reverse v) k v)
     (assoc (rdissoc reverse direct k) v k)))
  (containsKey [this k]
    (contains? direct k))
  (entryAt [this k]
    (find direct k))
  clojure.lang.IPersistentCollection
  (cons [this x]
    (if-let [[k v] (and (vector? x) x)]
      (assoc this k v)
      (reduce (fn [m [k v]] (assoc m k v)) this x)))
  (empty [this]
    (Bimap. (.empty direct) (.empty reverse)))
  (equiv [this x]
    (.equiv direct x))
  clojure.lang.Seqable
  (seq [this]
    (seq direct))
  clojure.lang.ILookup
  (valAt [this k else]
    (direct k else))
  (valAt [this k]
    (direct k))
  clojure.lang.Counted
  (count [this]
    (count direct))
  ReversibleMap
  (rmap [this]
    (Bimap. reverse direct))
  clojure.lang.IFn
  (invoke [this k]
    (direct k))
  (invoke [this k default]
    (direct k default)))

(defn bimap [& kvs]
  (reduce (fn [m [k v]] (assoc m k v)) (Bimap. (sorted-map) (sorted-map)) (partition 2 kvs)))
