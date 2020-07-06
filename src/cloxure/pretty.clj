(ns cloxure.pretty
  (:require [clojure.string :as str]
            [cloxure.token :as token]
            [cloxure.ast :as ast]))

(defmulti pretty-print
  "Converts an AST to string using a lisp-like notatation"
  :type)

(defn- parenthesize [name & expressions]
  (str "(" name " " (str/join " " (map pretty-print expressions)) ")"))

(defmethod pretty-print ::ast/binary [{::ast/keys [operator left right]}]
  (parenthesize (::token/lexeme operator) left right))

(defmethod pretty-print ::ast/logical [{::ast/keys [operator left right]}]
  (parenthesize (::token/lexeme operator) left right))

(defmethod pretty-print ::ast/unary [{::ast/keys [operator right]}]
  (parenthesize (::token/lexeme operator) right))

(defmethod pretty-print ::ast/group [{::ast/keys [expression]}]
  (parenthesize "group" expression))

(defmethod pretty-print ::ast/literal [{::ast/keys [value]}]
  (if (nil? value) "nil" (str/trim (prn-str value))))

(defmethod pretty-print ::ast/variable [{::ast/keys [name-token]}]
  (::token/lexeme name-token))

(defmethod pretty-print ::ast/print-stmt [{::ast/keys [expression]}]
  (parenthesize "print" expression))

(defmethod pretty-print ::ast/var-stmt [{::ast/keys [name-token initializer]}]
  (if (nil? initializer)
    (format "(var %s)" (::token/lexeme name-token))
    (format "(var %s = %s)" (::token/lexeme name-token) (pretty-print initializer))))

(defmethod pretty-print ::ast/assign [{::ast/keys [name-token value]}]
  (format "(%s = %s)" (::token/lexeme name-token) (pretty-print value)))

(defmethod pretty-print ::ast/block [{::ast/keys [statements]}]
  (apply parenthesize "block" statements))

(defmethod pretty-print ::ast/return-stmt [{::ast/keys [value]}]
  (format "(return %s)" (pretty-print value)))

(defmethod pretty-print ::ast/if-stmt [{::ast/keys [condition then-branch else-branch]}]
  (if (nil? else-branch)
    (parenthesize "if" condition then-branch)
    (parenthesize "if" condition then-branch else-branch)))

(defmethod pretty-print ::ast/while-stmt [{::ast/keys [condition body]}]
  (parenthesize "while" condition body))

(defmethod pretty-print ::ast/call [{::ast/keys [callee arguments]}]
  (format "(call %s [%s])"
          (pretty-print callee)
          (str/join ", " (map pretty-print arguments))))

(defmethod pretty-print ::ast/get-expr [{::ast/keys [object name-token]}]
  (format "(get %s %s)" (pretty-print object) (::token/lexeme name-token)))

(defmethod pretty-print ::ast/set-expr [{::ast/keys [object name-token value]}]
  (format "(set %s %s %s)" 
          (pretty-print object) (::token/lexeme name-token) (pretty-print value)))

(defmethod pretty-print ::ast/fun-stmt [{::ast/keys [name-token params body]}]
  (format "(fun %s [%s] %s)"
          (::token/lexeme name-token)
          (str/join ", " (map ::token/lexeme params))
          (str/join " " (map pretty-print body))))

(defmethod pretty-print ::ast/class-stmt [{::ast/keys [name-token methods]}]
  (format "(class %s \n%s)"
          (::token/lexeme name-token)
          (str/join "\n" (map pretty-print methods))))

(defmethod pretty-print ::ast/this-expr [_]
  "*this*")

(defmethod pretty-print ::ast/super [{::ast/keys [method]}]
  (format "(super %s)" (::token/lexeme method)))
