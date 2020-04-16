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

(defmulti pretty-print
  "Converts an AST to string using a lisp-like notatation" 
  :type)

(defn- parenthesize [name & expressions]
  (str "(" name " " (str/join " " (map pretty-print expressions)) ")"))

(defmethod pretty-print :binary [{op :operator l :left r :right}]
  (parenthesize (:text op) l r))

(defmethod pretty-print :unary [{op :operator r :right}]
  (parenthesize (:text op) r))

(defmethod pretty-print :group [{e :expression}]
  (parenthesize "group" e))

(defmethod pretty-print :literal [{value :value}]
  (if (nil? value) "nil" (str value)))

(def sample )

(comment (pretty-print (binary (unary (scanner/token :minus "-")
                                      (literal 123))
                               (scanner/token :star "*")
                               (group (literal 45.67)))))

(defmulti rpn-print
  "Converts and AST to string using Reverse Polish Notation"
  :type)

(defmethod rpn-print :binary [{op :operator l :left r :right}]
  (str/join " " [(rpn-print l) (rpn-print r) (:text op)]))

(defmethod rpn-print :unary [{op :operator r :right}]
  (str/join " " [(rpn-print r) (:text op)]))

(defmethod rpn-print :group [{e :expression}]
  (rpn-print e))

(defmethod rpn-print :literal [{value :value}]
  (if (nil? value) "nil" (str value)))

(comment (rpn-print (binary (binary (literal 1)
                                    (scanner/token :plus "+")
                                    (literal 2))
                            (scanner/token :star "*")
                            (binary (literal 4)
                                    (scanner/token :minus "-")
                                    (literal 3)))))