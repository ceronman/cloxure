(ns cloxure.interpreter
  "A tree-walk interpreter for the Lox programming language."
  (:require
   [cloxure.token :as token]
   [cloxure.ast :as ast]
   [cloxure.callable :as callable :refer [LoxCallable]]
   [cloxure.environment :as environment]
   [cloxure.error :as error :refer [runtime-error]]))

(def time-builtin
  (reify LoxCallable
    (call [this state _]
      (assoc state ::result (/ (System/currentTimeMillis) 1000.0)))
    (arity [this] 0)
    (to-string [this] "<native fn>")))

(def builtins
  {"clock" time-builtin})

(defn new-interpreter-state 
  "Creates a new interpreter state to be passed to the `interpret` function.
   This state can then be pased from call to call, enabling REPL support."
  []
  (let [globals (environment/new-environment builtins)]
    {::result nil
     ::errors []
     ::globals globals
     ::environment globals
     ::locals {}}))

(defn- declare-variable [state name-token value]
  (let [env (::environment state)]
    (environment/declare-name! env (::token/lexeme name-token) value)
    state))

(defn- lookup-variable [state name-token expr]
  (let [{env ::environment globals ::globals locals ::locals} state]
    (assoc state ::result
     (if-let [distance (get locals expr)]
       (environment/get-name-at env distance (::token/lexeme name-token))
       (environment/get-name globals name-token)))))

(defn- assign-variable [state name-token expr value]
  (if-let [distance (get-in state [::locals expr])]
    (environment/assign-name-at! (::environment state) name-token distance value)
    (environment/assign-name! (::globals state) name-token value))
  state)

(defn- truthy? [value]
  (cond
    (nil? value) false
    (instance? Boolean value) value
    :else true))

(defn- numeric-operation 
  ([op-token operator left right]
   (if (and (instance? Double left) (instance? Double right))
     (operator left right)
     (runtime-error op-token "Operands must be numbers.")))
  ([op-token operator right]
   (if (instance? Double right)
     (operator right)
     (runtime-error op-token "Operand must be a number."))))

(defmulti evaluate
  "Interprets a Lox AST.
   
   This multimethod takes an interpreter state and an AST node.
   It dispatches on the node type and returns a new state after
   evaluating the node. The result value of the evaluation is returned
   in the :::result field of the state."
  (fn [_ node] (::ast/type node)))

(defmethod evaluate ::ast/literal [state literal-expr]
  (assoc state ::result (::ast/value literal-expr)))

(defmethod evaluate ::ast/group [state group-expr]
  (evaluate state (::ast/expression group-expr)))

(defmethod evaluate ::ast/unary [state unary-expr]
  (let [state (evaluate state (::ast/right unary-expr))
        right (::result state)
        operator (::token/type (::ast/operator unary-expr))]
    (assoc state ::result
           (case operator
             ::token/minus (numeric-operation (::ast/operator unary-expr) - right)
             ::token/bang (not (truthy? right))))))

(defmethod evaluate ::ast/binary [state binary-expr]
  (let [state (evaluate state (::ast/left binary-expr))
        left (::result state)
        state (evaluate state (::ast/right binary-expr))
        right (::result state)
        op-token (::ast/operator binary-expr)
        operator (::token/type (::ast/operator binary-expr))]
    (assoc state ::result
           (case operator
             ::token/bang_equal (not= left right)
             ::token/equal_equal (= left right)
             ::token/greater (numeric-operation op-token > left right)
             ::token/greater_equal (numeric-operation op-token >= left right)
             ::token/less (numeric-operation op-token < left right)
             ::token/less_equal (numeric-operation op-token <= left right)
             ::token/minus (numeric-operation op-token - left right)
             ::token/plus (cond
                            (and (instance? Double left) (instance? Double right))
                            (+ left right)
                            
                            (and (instance? String left) (instance? String right))
                            (str left right)
                            
                            :else
                            (runtime-error
                             op-token
                             "Operands must be two numbers or two strings."))
             ::token/slash (numeric-operation op-token / left right)
             ::token/star (numeric-operation op-token * left right)))))

(defmethod evaluate ::ast/logical [state logical-expr]
  (let [state (evaluate state (::ast/left logical-expr))
        left (::result state)
        operator (::token/type (::ast/operator logical-expr))
        left-truthy? (truthy? left)]
    (cond
      (and (= operator ::token/or) left-truthy?) state
      (and (= operator ::token/and) (not left-truthy?)) state
      :else (evaluate state (::ast/right logical-expr)))))

(defmethod evaluate ::ast/var-stmt [state var-stmt]
  (let [initializer (::ast/initializer var-stmt)
        state (if initializer
                (evaluate state initializer)
                (assoc state ::result nil))]
    (-> state
        (declare-variable (::ast/name-token var-stmt) (::result state))
        (assoc ::result nil))))

(defmethod evaluate ::ast/variable [state var-expr]
  (lookup-variable state (::ast/name-token var-expr) var-expr))

(defmethod evaluate ::ast/assign [state assign-expr]
  (let [state (evaluate state (::ast/value assign-expr))
        value (::result state)]
    (assign-variable state (::ast/name-token assign-expr) assign-expr value)))

(defn- evaluate-statements [state statements]
  (reduce evaluate state statements))

(defmethod evaluate ::ast/block [state block-stmt]
  (-> state
      (update ::environment environment/push-scope)
      (evaluate-statements (::ast/statements block-stmt))
      (update ::environment environment/pop-scope)
      (assoc ::result nil)))

(defn- evaluate-args [state args]
  (reduce
   (fn [state arg]
     (let [all-results (::result state)
           state (evaluate state arg)]
       (assoc state ::result (conj all-results (::result state)))))
   (assoc state ::result [])
   args))

(defmethod evaluate ::ast/call [state call-expr]
  (let [state (evaluate state (::ast/callee call-expr))
        callee (::result state)
        state (evaluate-args state (::ast/arguments call-expr))
        arguments (::result state)]
    (cond
      (not (satisfies? LoxCallable callee))
      (runtime-error (::ast/paren call-expr) "Can only call functions and classes.")

      (not= (callable/arity callee) (count arguments))
      (runtime-error (::ast/paren call-expr)
                     (format "Expected %d arguments but got %d."
                             (callable/arity callee)
                             (count arguments)))

      :else 
      (callable/call callee state arguments))))

(defn- declare-fn-args [state params args]
  (reduce (fn [state i]
            (declare-variable state (nth params i) (nth args i)))
          state
          (range (count params))))

(defn- execute-fn-body [state statements closure initializer?]
  (try
    (-> state
        (evaluate-statements statements)
        (assoc ::result (when initializer? 
                          (environment/get-name-at closure 0 "this"))))
    (catch clojure.lang.ExceptionInfo e
      (let [{return? ::return? state ::state} (ex-data e)]
        (if return?
          (if initializer? 
            (assoc state ::result (environment/get-name-at closure 0 "this")) 
            state)
          (throw e))))))

(defrecord LoxFunction [declaration closure initializer?]
  LoxCallable
  (arity [this]
         (-> this :declaration ::ast/params count))
  (to-string [this] 
             (format "<fn %s>" 
                     (get-in this [:declaration ::ast/name-token ::token/lexeme])))
  (call [this state args]
        (let [declaration (:declaration this)
              env (::environment state)
              closure (:closure this)]
          (-> state
              (assoc ::environment (environment/push-scope closure))
              (declare-fn-args (::ast/params declaration) args)
              (execute-fn-body (::ast/body declaration) closure (:initializer? this))
              (assoc ::environment env)))))

(defn function-bind [lox-fn instance]
  (let [env (environment/push-scope (:closure lox-fn))]
    (environment/declare-name! env "this" instance)
    (->LoxFunction (:declaration lox-fn) env (:initializer? lox-fn))))


(defn find-method [lox-class name]
  (let [{:keys [methods superclass]} lox-class]
    (if-let [method (get methods name)]
      method
      (if superclass
        (recur superclass name)
        nil))))

(defrecord LoxInstance [lox-class fields])

(defn- new-instance [lox-class]
  (->LoxInstance lox-class (atom {})))

(defn- instance-get [object name-token]
  (let [fields (:fields object)
        name (::token/lexeme name-token)]
    (if (contains? @fields name)
      (get @fields name)
      (if-let [method (find-method (:lox-class object) name)]
        (function-bind method object)
        (runtime-error name-token (str "Undefined property '" name "'."))))))

(defn- instance-set! [object name-token value]
  (swap! (:fields object) assoc (::token/lexeme name-token) value))

(defrecord LoxClass [name superclass methods]
  LoxCallable
  (arity [this]
    (if-let [initializer (find-method this "init")]
      (callable/arity initializer)
      0))
  (to-string [this] (:name this))
  (call [this state args]
    (let [instance (new-instance this)
          initializer (find-method this "init")
          state (if initializer
                  (callable/call (function-bind initializer instance) state args)
                  state)]
      (assoc state ::result instance))))

(defmethod evaluate ::ast/this-expr [state this-expr]
  (lookup-variable state (::ast/keyword this-expr) this-expr))

(defmethod evaluate ::ast/get-expr [state get-expr]
  (let [name-token (::ast/name-token get-expr)
        state (evaluate state (::ast/object get-expr))
        object (::result state)]
    (if (instance? LoxInstance object)
      (assoc state ::result (instance-get object name-token))
      (runtime-error name-token "Only instances have properties."))))

(defmethod evaluate ::ast/set-expr [state set-expr]
  (let [name-token (::ast/name-token set-expr)
        state (evaluate state (::ast/object set-expr))
        object (::result state)]
    (if (instance? LoxInstance object)
      (let [state (evaluate state (::ast/value set-expr))
            value (::result state)]
        (instance-set! object name-token value)
        (assoc state ::result value))
      (runtime-error name-token "Only instances have fields."))))

(defmethod evaluate ::ast/super [state super-expr]
  (let [distance (get-in state [::locals super-expr])
        env (::environment state)
        lox-class (environment/get-name-at env distance "super")
        object (environment/get-name-at env (dec distance) "this")
        method-name (::token/lexeme (::ast/method super-expr))
        method (find-method lox-class method-name)]
    (if method
      (assoc state ::result (function-bind method object))
      (runtime-error (::ast/method super-expr) 
                     (str "Undefined property '" method-name "'.")))))

(defn- stringify [value]
  (cond
    (nil? value) "nil"
    (instance? Double value) (let [s (str value)]
                               (if (.endsWith s ".0")
                                 (subs s 0 (- (count s) 2))
                                 s))
    (instance? String value) value
    (instance? Boolean value) (if value "true" "false")
    (instance? LoxInstance value) (format "%s instance" (-> value :lox-class :name))
    (satisfies? LoxCallable value) (callable/to-string value)
    :else "<???>"))

(defmethod evaluate ::ast/print-stmt [state print-stmt]
  (let [state (evaluate state (::ast/expression print-stmt))]
    (assoc state ::result (println (stringify (::result state))))))

(defmethod evaluate ::ast/if-stmt [state if-stmt]
  (let [state (evaluate state (::ast/condition if-stmt))]
    (if (truthy? (::result state))
      (evaluate state (::ast/then-branch if-stmt))
      (if-let [else-branch (::ast/else-branch if-stmt)]
        (evaluate state else-branch)
        (assoc state ::result nil)))))

(defmethod evaluate ::ast/while-stmt [state while-stmt]
  (let [state (evaluate state (::ast/condition while-stmt))]
    (if (truthy? (::result state))
      (recur (evaluate state (::ast/body while-stmt)) while-stmt)
      (assoc state ::result nil))))

(defmethod evaluate ::ast/fun-stmt [state fun-stmt]
  (let [f (->LoxFunction fun-stmt (::environment state) false)]
    (declare-variable state (::ast/name-token fun-stmt) f)))

(defmethod evaluate ::ast/return-stmt [state return-stmt]
  (let [value (::ast/value return-stmt)
        state (if value (evaluate state value) (assoc state ::result nil))]
    (throw (ex-info "return" {::return? true
                              ::state state}))))

(defn- evaluate-superclass [state class-stmt]
  (if-let [superclass (::ast/superclass class-stmt)]
    (let [state (evaluate state superclass)
          value (::result state)]
      (if (instance? LoxClass value)
        state
        (runtime-error (::ast/name-token superclass) "Superclass must be a class.")))
    (assoc state ::result nil)))

(defmethod evaluate ::ast/class-stmt [state class-stmt]
  (let [name-token (::ast/name-token class-stmt)
        state (evaluate-superclass state class-stmt)
        superclass (::result state)
        env (::environment state)
        env (if superclass 
              (environment/declare-name! (environment/push-scope env) "super" superclass) 
              env)
        methods (->> (::ast/methods class-stmt)
                     (map (fn [fun-stmt]
                            (let [name (::token/lexeme (::ast/name-token fun-stmt))]
                              [name (->LoxFunction fun-stmt env (= name "init"))])))
                     (into {}))
        lox-class (->LoxClass (::token/lexeme name-token) superclass methods)]
    (declare-variable state name-token lox-class)))


(defn interpret 
  "Interprets Lox code.
   It takes an interpreter state, usually created using the new-interpreter-state
   function, and a list of AST statements generated from the parser 
   (see cloxure.parser). Additionally it takes a map of lexical scoping locals
   produced by the resolver (see cloxure.resolver). It returns an updated
   interpreter state after executing the statements."
  [state statements locals]
  (try
    (evaluate-statements (update state ::locals merge locals) statements)
    (catch clojure.lang.ExceptionInfo e
      (let [lox-error (ex-data e)]
        (if (= (::error/type lox-error) ::error/runtime)
          (-> state
              (update ::errors conj lox-error)
              (assoc ::result nil))
          (throw e))))))
