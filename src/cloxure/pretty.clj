(ns cloxure.pretty
  (:require [clojure.string :as str]))

(defmulti pretty-print
  "Converts an AST to string using a lisp-like notatation"
  :type)

(defn- parenthesize [name & expressions]
  (str "(" name " " (str/join " " (map pretty-print expressions)) ")"))

(defmethod pretty-print :binary [{op :operator l :left r :right}]
  (parenthesize (:lexeme op) l r))

(defmethod pretty-print :logical [{op :operator l :left r :right}]
  (parenthesize (:lexeme op) l r))

(defmethod pretty-print :unary [{op :operator r :right}]
  (parenthesize (:lexeme op) r))

(defmethod pretty-print :group [{e :expression}]
  (parenthesize "group" e))

(defmethod pretty-print :literal [{value :value}]
  (if (nil? value) "nil" (str/trim (prn-str value))))

(defmethod pretty-print :variable [{name-token :name-token}]
  (:lexeme name-token))

(defmethod pretty-print :print-stmt [{e :expression}]
  (parenthesize "print" e))

(defmethod pretty-print :var-stmt [{n :name-token i :initializer}]
  (if (nil? i)
    (format "(var %s)" (:lexeme n))
    (format "(var %s = %s)" (:lexeme n) (pretty-print i))))

(defmethod pretty-print :assign [{n :name-token v :value-expr}]
  (format "(%s = %s)" (:lexeme n) (pretty-print v)))

(defmethod pretty-print :block [{s :statements}]
  (apply parenthesize "block" s))

(defmethod pretty-print :return-stmt [{v :value}]
  (format "(return %s)" (pretty-print v)))

(defmethod pretty-print :if-stmt [{c :condition t :then-branch e :else-branch}]
  (if (nil? e)
    (parenthesize "if" c t)
    (parenthesize "if" c t e)))

(defmethod pretty-print :while-stmt [{c :condition b :body}]
  (parenthesize "while" c b))

(defmethod pretty-print :call [{c :callee args :arguments}]
  (format "(call %s [%s])"
          (pretty-print c)
          (str/join ", " (map pretty-print args))))

(defmethod pretty-print :get-expr [{obj :object n :name-token}]
  (format "(get %s %s)" (pretty-print obj) (:lexeme n)))

(defmethod pretty-print :set-expr [{obj :object n :name-token v :value}]
  (format "(set %s %s %s)" (pretty-print obj) (:lexeme n) (pretty-print v)))

(defmethod pretty-print :fun-stmt [{name-token :name-token params :params body :body}]
  (format "(fun %s [%s] %s)"
          (:lexeme name-token)
          (str/join ", " (map :lexeme params))
          (str/join " " (map pretty-print body))))

(defmethod pretty-print :class-stmt [{name :name-token methods :methods}]
  (format "(class %s \n%s)"
          (:lexeme name)
          (str/join "\n" (map pretty-print methods))))

(defmethod pretty-print :this-expr [_]
  "*this*")

(defmethod pretty-print :super [{method :method}]
  (format "(super %s)" (:lexeme method)))
