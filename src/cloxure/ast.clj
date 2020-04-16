(ns cloxure.ast
  (:require [cloxure.scanner :as scanner])
  (:require [clojure.string :as str]))

(defn- binary [left operator right]
  {:type :binary :left left :right right :operator operator})

(defn- unary [operator right]
  {:type :unary :operator operator :right right})

(defn- literal [value]
  {:type :literal :value value})

(defn- group [expression]
  {:type :group :expression expression})

(defmulti pretty-print :type)

(defn- parenthesize [name & expressions]
  (str "(" name " " (str/join " " (map pretty-print expressions)) ")"))

(defmethod pretty-print :binary [{op :operator l :left r :right}]
  (parenthesize (:text op) l r))

(defmethod pretty-print :unary [{op :operator r :right}]
  (parenthesize (:text op) r))

(defmethod pretty-print :group [{op :operator e :expression}]
  (parenthesize "group" e))

(defmethod pretty-print :literal [{value :value}]
  (if (nil? value) "nil" (str value)))

(def sample (binary (unary (scanner/token :minus "-")
                           (literal 123))
                    (scanner/token :star "*")
                    (group (literal 45.67))))

(comment (pretty-print sample))