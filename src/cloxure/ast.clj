(ns cloxure.ast
  (:require [cloxure.scanner :as scanner])
  (:require [clojure.string :as str]))

(defn binary [left operator right]
  {:type :binary :left left :right right :operator operator})

(defn unary [operator right]
  {:type :unary :operator operator :right right})

(defn literal [value]
  {:type :literal :value value})

(defn variable [name-token]
  {:type :variable :name-token name-token})

(defn group [expression]
  {:type :group :expression expression})

(defn print-stmt [expression]
  {:type :print-stmt :expression expression})

(defn var-stmt [name-token initializer]
  {:type :var-stmt :name-token name-token :initializer initializer})

(defn assign [name-token value-expr]
  {:type :assign :name-token name-token :value-expr value-expr})

(defn block [statements]
  {:type :block :statements statements})

(defn if-stmt [condition, then-branch, else-branch]
  {:type :if-stmt :condition condition :then-branch then-branch :else-branch else-branch})



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
  (if (nil? value) "nil" (str/trim (prn-str value))))

(defmethod pretty-print :variable [{name-token :name-token}]
  (:text name-token))

(defmethod pretty-print :print-stmt [{e :expression}]
  (parenthesize "print" e))

(defmethod pretty-print :var-stmt [{n :name-token i :initializer}]
  (if (nil? i) 
    (format "(var %s)" (:text n))
    (format "(var %s = %s)" (:text n) (pretty-print i))))

(defmethod pretty-print :assign [{n :name-token v :value-expr}]
  (format "(%s = %s)" (:text n) (pretty-print v)))

(defmethod pretty-print :block [{s :statements}]
  (str "(block " (str/join " " (map pretty-print s))  ")"))

(defmethod pretty-print :if-stmt [{c :condition t :then-branch e :else-branch}]
  (format "(if (%s) %s %s)"
          (pretty-print c)
          (pretty-print t)
          (if (nil? e) "nil" (pretty-print e))))


(comment (pretty-print (binary (unary (scanner/token :minus "-")
                                      (literal 123))
                               (scanner/token :star "*")
                               (group (literal 45.67)))))