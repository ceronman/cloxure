(ns cloxure.ast
  (:require [cloxure.scanner :as scanner])
  (:require [clojure.string :as str]))

(defn binary [left operator right]
  {:type :binary :left left :right right :operator operator})

(defn call [callee paren arguments]
  {:type :call :callee callee :paren paren :arguments arguments})

(defn get-expr [object name-token]
  {:type :get-expr :object object :name-token name-token})

(defn set-expr [object name-token value]
  {:type :set-expr :object object :name-token name-token :value value})

(defn unary [operator right]
  {:type :unary :operator operator :right right})

(defn literal [value]
  {:type :literal :value value})

(defn logical [left operator right]
  {:type :logical :left left :right right :operator operator})

(defn variable [name-token]
  {:type :variable :name-token name-token})

(defn group [expression]
  {:type :group :expression expression})

(defn print-stmt [expression]
  {:type :print-stmt :expression expression})

(defn return-stmt [keyword value]
  {:type :return-stmt :keyword keyword :value value})

(defn var-stmt [name-token initializer]
  {:type :var-stmt :name-token name-token :initializer initializer})

(defn assign [name-token value-expr]
  {:type :assign :name-token name-token :value-expr value-expr})

(defn block [statements]
  {:type :block :statements statements})

(defn if-stmt [condition, then-branch, else-branch]
  {:type :if-stmt :condition condition :then-branch then-branch :else-branch else-branch})

(defn while-stmt [condition body]
  {:type :while-stmt :condition condition :body body})

(defn fun-stmt [name params body]
  {:type :fun-stmt :name name :params params :body body})

(defn class-stmt [name-token methods]
  {:type :class-stmt :name-token name-token :methods methods})



(defmulti pretty-print
  "Converts an AST to string using a lisp-like notatation" 
  :type)

(defn- parenthesize [name & expressions]
  (str "(" name " " (str/join " " (map pretty-print expressions)) ")"))

(defmethod pretty-print :binary [{op :operator l :left r :right}]
  (parenthesize (:text op) l r))

(defmethod pretty-print :logical [{op :operator l :left r :right}]
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
  (format "(get %s %s)" (pretty-print obj) (:text n)))

(defmethod pretty-print :set-expr [{obj :object n :name-token v :value}]
  (format "(set %s %s %s)" (pretty-print obj) (:text n) (pretty-print v)))

(defmethod pretty-print :fun-stmt [{name :name params :params body :body}]
  (format "(fun %s [%s] %s)"
          (:text name)
          (str/join ", " (map :text params))
          (str/join " " (map pretty-print body))))

(defmethod pretty-print :class-stmt[{name :name-token methods :methods}]
  (format "(class %s \n%s)"
          (:text name)
          (str/join "\n" (map pretty-print methods))))


(comment (pretty-print (binary (unary (scanner/token :minus "-")
                                      (literal 123))
                               (scanner/token :star "*")
                               (group (literal 45.67)))))