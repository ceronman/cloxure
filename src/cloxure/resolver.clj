(ns cloxure.resolver
  "Statically resolves local variable scoping for the Lox language."
  (:require
   [cloxure.token :as token]
   [cloxure.ast :as ast]
   [cloxure.error :refer [token-error]]))

(defn- new-resolver []
  {::scopes '()
   ::locals {}
   ::errors []
   ::current-fn nil
   ::current-class nil})

(defn- add-error [resolver token message]
  (update resolver ::errors conj (token-error token message)))

(defn- add-to-scope [resolver name ready?]
  (let [[head & tail] (::scopes resolver)]
    (assoc resolver ::scopes (conj tail (assoc head name ready?)))))

(defn- declare-name [resolver name-token ready?]
  (let [scope (peek (::scopes resolver))
        name (::token/lexeme name-token)]
    (if scope
      (if (and (not ready?) (contains? scope name))
        (add-error resolver name-token 
               "Variable with this name already declared in this scope.")
        (add-to-scope resolver name ready?))
      resolver)))

(defn- declare-names [resolver name-tokens ready?]
  (reduce #(declare-name %1 %2 ready?) resolver name-tokens))

(defn- resolve-local [resolver node name-token]
  (loop [pos 0
         scopes (::scopes resolver)]
    (cond
      (empty? scopes) 
      resolver
      
      (contains? (peek scopes) (::token/lexeme name-token)) 
      (update resolver ::locals assoc node pos)
      
      :else 
      (recur (inc pos) (rest scopes)))))

(defn- begin-scope [resolver]
  (update resolver ::scopes conj {}))

(defn- end-scope [resolver]
  (update resolver ::scopes pop))

(defmulti resolve-locals
  "Statically resolves scoping of variables.
   
   This multi-method accepts a resolver state and an AST node.
   It dispatches on the node type (see cloxure.ast).
   It returns the new resolver state after resolving the node."
  (fn [_ node] (::ast/type node)))

(defn- resolve-statements [resolver statements]
  (reduce resolve-locals resolver statements))

(defmethod resolve-locals ::ast/block [resolver node]
  (-> resolver
      (begin-scope)
      (resolve-statements (::ast/statements node))
      (end-scope)))

(defmethod resolve-locals ::ast/var-stmt [resolver node]
  (let [name-token (::ast/name-token node)
        resolver (declare-name resolver name-token false)
        initializer (::ast/initializer node)
        resolver (if initializer (resolve-locals resolver initializer) resolver)]
    (declare-name resolver name-token true)))

(defmethod resolve-locals ::ast/variable [resolver node]
  (let [name-token (::ast/name-token node)
        [scope & _] (::scopes resolver)
        resolver (if (and scope (false? (get scope (::token/lexeme name-token))))
                   (add-error resolver
                          name-token
                          "Cannot read local variable in its own initializer.")
                   resolver)]
    (resolve-local resolver node name-token)))

(defmethod resolve-locals ::ast/assign [resolver node]
  (-> resolver
      (resolve-locals (::ast/value node))
      (resolve-local node (::ast/name-token node))))

(defn- resolve-function [resolver fun-stmt fn-type]
  (let [prev-fn (::current-fn resolver)]
    (-> resolver
        (begin-scope)
        (assoc ::current-fn fn-type)
        (declare-names (::ast/params fun-stmt) false)
        (declare-names (::ast/params fun-stmt) true)
        (resolve-statements (::ast/body fun-stmt))
        (assoc ::current-fn prev-fn)
        (end-scope))))

(defmethod resolve-locals ::ast/fun-stmt [resolver node]
  (-> resolver
      (declare-name (::ast/name-token node) true)
      (resolve-function node ::function)))

(defn- resolve-methods [resolver methods]
  (reduce
   (fn [resolver method]
     (if (= (-> method ::ast/name-token ::token/lexeme) "init")
       (resolve-function resolver method ::ast/initializer)
       (resolve-function resolver method ::ast/method)))
   resolver
   methods))

(defn- resolve-superclass [resolver superclass]
  (-> resolver
      (resolve-locals superclass)
      (begin-scope)
      (add-to-scope "super" true)))

(defmethod resolve-locals ::ast/class-stmt [resolver node]
  (let [prev-class (::current-class resolver)
        name (::token/lexeme (::ast/name-token node))
        superclass (::ast/superclass node)
        superclass-name (::token/lexeme (::ast/name-token superclass))]
    (if (and superclass (= name superclass-name))
      (add-error resolver
             (::ast/name-token superclass)
             "A class cannot inherit from itself.")
      (-> resolver
          (declare-name (::ast/name-token node) true)
          (cond-> superclass (resolve-superclass superclass))
          (begin-scope)
          (add-to-scope "this" true)
          (assoc ::current-class (if (::ast/superclass node) ::subclass ::class))
          (resolve-methods (::ast/methods node))
          (assoc ::current-class prev-class)
          (end-scope)
          (cond-> superclass (end-scope))))))

(defmethod resolve-locals ::ast/if-stmt [resolver node]
  (-> resolver
      (resolve-locals (::ast/condition node))
      (resolve-locals (::ast/then-branch node))
      (cond-> (::ast/else-branch node) (resolve-locals (::ast/else-branch node)))))

(defmethod resolve-locals ::ast/print-stmt [resolver node]
  (resolve-locals resolver (::ast/expression node)))

(defmethod resolve-locals ::ast/while-stmt [resolver node]
  (-> resolver
      (resolve-locals (::ast/condition node))
      (resolve-locals (::ast/body node))))

(defmethod resolve-locals ::ast/binary [resolver node]
  (-> resolver
      (resolve-locals (::ast/left node))
      (resolve-locals (::ast/right node))))

(defmethod resolve-locals ::ast/logical [resolver node]
  (-> resolver
      (resolve-locals (::ast/left node))
      (resolve-locals (::ast/right node))))

(defmethod resolve-locals ::ast/unary [resolver node]
  (resolve-locals resolver (::ast/right node)))

(defmethod resolve-locals ::ast/group [resolver node]
  (resolve-locals resolver (::ast/expression node)))

(defmethod resolve-locals ::ast/literal [resolver _]
  resolver)

(defmethod resolve-locals ::ast/call [resolver node]
  (let [resolver (resolve-locals resolver (::ast/callee node))]
    (reduce resolve-locals resolver (::ast/arguments node))))

(defmethod resolve-locals ::ast/get-expr [resolver node]
  (resolve-locals resolver (::ast/object node)))

(defmethod resolve-locals ::ast/set-expr [resolver node]
  (-> resolver
      (resolve-locals (::ast/object node))
      (resolve-locals (::ast/value node))))

(defmethod resolve-locals ::ast/this-expr [resolver node]
  (if (nil? (::current-class resolver))
    (add-error resolver
           (::ast/keyword node)
           "Cannot use 'this' outside of a class.")
    (resolve-local resolver node (::ast/keyword node))))

(defmethod resolve-locals ::ast/super [resolver node]
  (case (::current-class resolver)
    ::subclass (resolve-local resolver node (::ast/keyword node))
    nil (add-error resolver (::ast/keyword node) 
               "Cannot use 'super' outside of a class.")
    (add-error resolver (::ast/keyword node) 
           "Cannot use 'super' in a class with no superclass.")))

(defmethod resolve-locals ::ast/return-stmt [resolver node]
  (if (nil? (::current-fn resolver))
    (add-error resolver
           (::ast/keyword node)
           "Cannot return from top-level code.")
    (if (::ast/value node)
      (if (= (::current-fn resolver) ::ast/initializer)
        (add-error resolver
               (::ast/keyword node)
               "Cannot return a value from an initializer.")
        (resolve-locals resolver (::ast/value node)))
      resolver)))

(defn locals
  "Statically resolve the lexical scoping of variables and references for Lox.
   
   It accepts a list of AST nodes (see cloxure.ast) representing the 
   statements/declarations of a Lox program. This are generated by the Lox 
   parser (See cloxure.parser).
   Returns a vector with two elements: A map of tokens to distance from the current
   scope and a list of errors found during the resolving phase."
  [statements]
  (let [resolver (new-resolver)
        resolver (resolve-statements resolver statements)]
    [(::locals resolver) (::errors resolver)]))