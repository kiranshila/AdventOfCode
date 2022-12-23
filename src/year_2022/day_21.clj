(ns year-2022.day-21
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn tree-eval
  ([tree] (tree-eval tree "root"))
  ([tree node]
   (if (integer? (tree node))
     (tree node)
     (apply (get-in tree [node :op])
            (map (partial tree-eval tree)
                 (get-in tree [node :args]))))))

(defn parse-line [line]
  (let [[monkey rest] (map str/trim (str/split line #":"))]
    {monkey
     (if-let [x (parse-long rest)]
       x
       (let [[lhs op rhs] (str/split rest #" ")]
         {:op (eval (read-string op))
          :args [lhs rhs]}))}))

(defn build-tree [input]
  (apply merge (map parse-line (str/split-lines input))))

(def tree (->> (slurp (io/resource "2022/21/input"))
               build-tree))

(def part-1 (tree-eval tree))

(def foo (-> tree
             (update-in ["root" :op] =)
             (assoc "humn" nil)))

;; In the REPL....it's just a line
; (get-in foo ["root" :args])
(def rhs (tree-eval foo "qqqz"))
(def b (tree-eval (assoc foo "humn" 0) "zhms"))
(def c (tree-eval (assoc foo "humn" 1) "zhms"))
(def m (- c b))
(def part-2 (/ (- rhs b) m))
