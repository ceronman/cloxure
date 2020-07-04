(ns cloxure.interpreter
  (:require
   [cloxure.token :as token]
   [cloxure.ast :as ast]
   [cloxure.error :refer [runtime-error]]))

; TODO: this is for debugging purposes, remove later.
(set! clojure.core/*print-level* 5)
(set! clojure.core/*warn-on-reflection* true)

(defprotocol LoxCallable
  "Represent a callable object in Lox"
  (arity [this])
  (call [this state arguments])
  (to-string [this]))

(def time-builtin
  (reify LoxCallable
    (call [this state _]
      (assoc state :result (/ (System/currentTimeMillis) 1000.0)))
    (arity [this] 0)
    (to-string [this] "<native fn>")))

(def builtins
  {"clock" time-builtin})

(defn new-interpreter-state []
  (let [globals (atom (assoc builtins :enclosing nil))]
    {:result nil
     :errors []
     :globals globals
     :environment globals
     :locals {}}))

(defn- raise-error [token message]
  (throw (ex-info "Runtime Error" 
                  {::error (runtime-error token message)})))

(defn- env-ancestor [env distance]
  (if (zero? distance)
    env
    (recur (:enclosing @env) (dec distance))))

(defn- env-get-at [env distance name]
  (let [env (env-ancestor env distance)]
    (get @env name)))

(defn- env-get [env name-token]
  (let [name (::token/lexeme name-token)]
   (if (contains? @env name)
     (get @env name)
     (raise-error name-token (format "Undefined variable '%s'." name)))))

(defn- env-declare! [env name value]
  (swap! env assoc name value)
  env)

(defn- env-assign! [env name-token value]
  (let [name (::token/lexeme name-token)]
    (if (contains? @env name)
     (swap! env assoc name value)
     (raise-error name-token (format "Undefined variable '%s'." name)))))

(defn- declare-variable [state name-token value]
  (let [env (:environment state)]
    (env-declare! env (::token/lexeme name-token) value)
    state))

(defn- lookup-variable [state name-token expr]
  (if-let [distance (get-in state [:locals expr])]
    (assoc state :result (env-get-at (:environment state) distance (::token/lexeme name-token)))
    (assoc state :result (env-get (:globals state) name-token))))

(defn- assign-variable [state name-token expr value]
  (let [distance (get-in state [:locals expr])
        env (if (nil? distance)
              (:globals state)
              (env-ancestor (:environment state) distance))]
    (env-assign! env name-token value)
    state))

(defn- push-scope [env]
  (atom {:enclosing env}))

(defn- pop-scope [env]
  (:enclosing (deref env)))

(defn- truthy? [value]
  (cond
    (nil? value) false
    (instance? Boolean value) value
    :else true))

(defn- numeric-operation 
  ([op-token operator left right]
   (if (and (instance? Double left) (instance? Double right))
     (operator left right)
     (raise-error op-token "Operands must be numbers.")))
  ([op-token operator right]
   (if (instance? Double right)
     (operator right)
     (raise-error op-token "Operand must be a number."))))

(defmulti evaluate
  "Interprets a Lox AST"
  (fn [_ node] (::ast/type node)))

(defmethod evaluate ::ast/literal [state literal-expr]
  (assoc state :result (::ast/value literal-expr)))

(defmethod evaluate ::ast/group [state group-expr]
  (evaluate state (::ast/expression group-expr)))

(defmethod evaluate ::ast/unary [state unary-expr]
  (let [state (evaluate state (::ast/right unary-expr))
        right (:result state)
        operator (::token/type (::ast/operator unary-expr))]
    (assoc state :result (case operator
                           ::token/minus (numeric-operation (::ast/operator unary-expr) - right)
                           ::token/bang (not (truthy? right))))))

(defmethod evaluate ::ast/binary [state binary-expr]
  (let [state (evaluate state (::ast/left binary-expr))
        left (:result state)
        state (evaluate state (::ast/right binary-expr))
        right (:result state)
        op-token (::ast/operator binary-expr)
        operator (::token/type (::ast/operator binary-expr))]
    (assoc state :result
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
                            (raise-error op-token
                                           "Operands must be two numbers or two strings."))
             ::token/slash (numeric-operation op-token / left right)
             ::token/star (numeric-operation op-token * left right)))))

(defmethod evaluate ::ast/logical [state logical-expr]
  (let [state (evaluate state (::ast/left logical-expr))
        left (:result state)
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
                (assoc state :result nil))]
    (-> state
        (declare-variable (::ast/name-token var-stmt) (:result state))
        (assoc :result nil))))

(defmethod evaluate ::ast/variable [state var-expr]
  (lookup-variable state (::ast/name-token var-expr) var-expr))

(defmethod evaluate ::ast/assign [state assign-expr]
  (let [state (evaluate state (::ast/value-expr assign-expr))
        value (:result state)]
    (assign-variable state (::ast/name-token assign-expr) assign-expr value)))

(defn- evaluate-statements [state statements]
  (reduce evaluate state statements))

(defmethod evaluate ::ast/block [state block-stmt]
  (-> state
      (update :environment push-scope)
      (evaluate-statements (::ast/statements block-stmt))
      (update :environment pop-scope)
      (assoc :result nil)))

(defn- evaluate-args [state args]
  (reduce
   (fn [state arg]
     (let [all-results (:result state)
           state (evaluate state arg)]
       (assoc state :result (conj all-results (:result state)))))
   (assoc state :result [])
   args))

(defmethod evaluate ::ast/call [state call-expr]
  (let [state (evaluate state (::ast/callee call-expr))
        callee (:result state)
        state (evaluate-args state (::ast/arguments call-expr))
        arguments (:result state)]
    (cond
      (not (satisfies? LoxCallable callee))
      (raise-error (::ast/paren call-expr) "Can only call functions and classes.")

      (not= (arity callee) (count arguments))
      (raise-error (::ast/paren call-expr)
                     (format "Expected %d arguments but got %d."
                             (arity callee)
                             (count arguments)))

      :else (call callee state arguments))))

(defn- declare-fn-args [state params args]
  (reduce (fn [state i]
            (declare-variable state (nth params i) (nth args i)))
          state
          (range (count params))))

(defn- execute-fn-body [state statements closure initializer?]
  (try
    (-> state
        (evaluate-statements statements)
        (assoc :result (if initializer? (env-get-at closure 0 "this") nil)))
    ; TODO: Investigate custom exceptions
    (catch clojure.lang.ExceptionInfo e
      (let [{return? ::return? state ::state} (ex-data e)]
        (if return?
          (if initializer? (assoc state :result (env-get-at closure 0 "this")) state)
          (throw e))))))

(defrecord LoxFunction [declaration closure initializer?]
  LoxCallable
  (arity [this] (-> this :declaration ::ast/params count))
  (to-string [this] (format "<fn %s>" (get-in this [:declaration ::ast/name-token ::token/lexeme])))
  (call [this state args]
        (let [declaration (:declaration this)
              env (:environment state)
              closure (:closure this)]
          (-> state
              (assoc :environment (push-scope closure))
              (declare-fn-args (::ast/params declaration) args)
              (execute-fn-body (::ast/body declaration) closure (:initializer? this))
              (assoc :environment env)))))

(defn lox-fn-bind [lox-fn instance]
  (let [env (push-scope (:closure lox-fn))]
    (env-declare! env "this" instance)
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
        (lox-fn-bind method object)
        (raise-error name-token (str "Undefined property '" name "'."))))))

(defrecord LoxClass [name superclass methods]
  LoxCallable
  (arity [this]
    (if-let [initializer (find-method this "init")]
      (arity initializer)
      0))
  (to-string [this] (:name this))
  (call [this state args]
    (let [instance (new-instance this)
          initializer (find-method this "init")
          state (if initializer
                  (call (lox-fn-bind initializer instance) state args)
                  state)]
      (assoc state :result instance))))

(defn- instance-set! [object name-token value]
  (swap! (:fields object) assoc (::token/lexeme name-token) value))

(defmethod evaluate ::ast/this-expr [state this-expr]
  (lookup-variable state (::ast/keyword this-expr) this-expr))

(defmethod evaluate ::ast/get-expr [state get-expr]
  (let [name-token (::ast/name-token get-expr)
        state (evaluate state (::ast/object get-expr))
        object (:result state)]
    (if (instance? LoxInstance object)
      (assoc state :result (instance-get object name-token))
      (raise-error name-token "Only instances have properties."))))

(defmethod evaluate ::ast/set-expr [state set-expr]
  (let [name-token (::ast/name-token set-expr)
        state (evaluate state (::ast/object set-expr))
        object (:result state)]
    (if (instance? LoxInstance object)
      (let [state (evaluate state (::ast/value set-expr))
            value (:result state)]
        (instance-set! object name-token value)
        (assoc state :result value))
      (raise-error name-token "Only instances have fields."))))

(defmethod evaluate ::ast/super [state super-expr]
  (let [distance (get-in state [:locals super-expr])
        env (:environment state)
        lox-class (env-get-at env distance "super")
        object (env-get-at env (dec distance) "this")
        method-name (::token/lexeme (::ast/method super-expr))
        method (find-method lox-class method-name)]
    (if method
      (assoc state :result (lox-fn-bind method object))
      (raise-error (::ast/method super-expr) 
                     (str "Undefined property '" method-name "'.")))))

;; TODO: Polymorphism please!!!
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
    (satisfies? LoxCallable value) (to-string value)
    :else "<???>"))

(defmethod evaluate ::ast/print-stmt [state print-stmt]
  (let [state (evaluate state (::ast/expression print-stmt))]
    (assoc state :result (println (stringify (:result state))))))

(defmethod evaluate ::ast/if-stmt [state if-stmt]
  (let [state (evaluate state (::ast/condition if-stmt))]
    (if (truthy? (:result state))
      (evaluate state (::ast/then-branch if-stmt))
      (if-let [else-branch (::ast/else-branch if-stmt)]
        (evaluate state else-branch)
        (assoc state :result nil)))))

(defmethod evaluate ::ast/while-stmt [state while-stmt]
  (let [state (evaluate state (::ast/condition while-stmt))]
    (if (truthy? (:result state))
      (recur (evaluate state (::ast/body while-stmt)) while-stmt)
      (assoc state :result nil))))

(defmethod evaluate ::ast/fun-stmt [state fun-stmt]
  (let [f (->LoxFunction fun-stmt (:environment state) false)]
    (declare-variable state (::ast/name-token fun-stmt) f)))

(defmethod evaluate ::ast/return-stmt [state return-stmt]
  (let [value (::ast/value return-stmt)
        state (if value (evaluate state value) (assoc state :result nil))]
    (throw (ex-info "return" {::return? true
                              ::state state}))))

(defn- evaluate-superclass [state class-stmt]
  (if-let [superclass (::ast/superclass class-stmt)]
    (let [state (evaluate state superclass)
          value (:result state)]
      (if (instance? LoxClass value)
        state
        (raise-error (::ast/name-token superclass) "Superclass must be a class.")))
    (assoc state :result nil)))

(defmethod evaluate ::ast/class-stmt [state class-stmt]
  (let [name-token (::ast/name-token class-stmt)
        state (evaluate-superclass state class-stmt)
        superclass (:result state)
        env (:environment state)
        env (if superclass 
              (env-declare! (push-scope env) "super" superclass) 
              env)
        methods (->> (::ast/methods class-stmt)
                     (map (fn [fun-stmt]
                            (let [name (::token/lexeme (::ast/name-token fun-stmt))]
                              [name (->LoxFunction fun-stmt env (= name "init"))])))
                     (into {}))
        lox-class (->LoxClass (::token/lexeme name-token) superclass methods)]
    (declare-variable state name-token lox-class)))


(defn interpret [state statements locals]
  (try
    (evaluate-statements (update state :locals merge locals) statements)
    (catch clojure.lang.ExceptionInfo e
      (if-let [lox-error (::error (ex-data e))]
        (-> state
            (update :errors conj lox-error)
            (assoc :result nil))
        (throw e)))))
  
(require '[cloxure.scanner :as scanner])
(require '[cloxure.parser :as parser])
(require '[cloxure.resolver :as resolver])

(defn- test-interpreter [code]
  (let [{errors :errors tokens :tokens} (scanner/scan code)]
    (if (seq errors)
      errors
      (let [{errors :errors statements :statements} (parser/parse tokens)]
        (if (seq errors)
          errors
          (let [{errors :errors locals :locals} (resolver/locals statements)]
            (if (seq errors)
              errors
              (let [state (new-interpreter-state)
                    {errors :errors} (interpret state statements locals)]
                (when (seq errors)
                  (println "ERROR", (prn-str errors)))))))))))

(comment
  (test-interpreter
   "1;"))

(comment
  (test-interpreter
   "1; true; \"hello\";"))

(comment
  (test-interpreter
   "(2);"))

(comment
  (test-interpreter
   "-4;"))

(comment
  (test-interpreter
   "4 * 4;"))

(comment
  (test-interpreter
   "true or false;"))

(comment
  (test-interpreter
   "print 16;"))

(comment
  (test-interpreter
   "print 1; print 2; print \"hello\"; print 1 + 2; 10 + 20;"))

(comment
  (test-interpreter
   "var a; a;"))

(comment
  (test-interpreter
   "var a = 4; a * 5;"))

(comment
  (test-interpreter
   "a = 4;"))

(comment
  (test-interpreter
   "var a = 4; a = 2; a * 5;"))

(comment
  (test-interpreter
   "{var a = 1; print a; a = a + 2; print a;}"))

(comment
  (test-interpreter
   "{var a = 1; {var a = a + 1;}}"))

(comment
  (test-interpreter
   "var a = 1; { var a = 2; print a;}"))

(comment
  (test-interpreter
   "var a = 1; var b = 2; print a + b;"))

(comment
  (test-interpreter
   "var a = 1; a = 3; print a;"))

(comment
  (test-interpreter
   "var a = 1; b = 3; print a;"))

(comment
  (test-interpreter
   "var part1 = \"hello \"; var part2 = \"world\"; print part1 + part2;"))


(comment
  (test-interpreter
   "var a = 1; { var a = 2; print a;} print a;"))

(comment
  (test-interpreter
   "var a = 1; { a = 2; } print a;"))

(comment
  (test-interpreter
   "var a = 1; { a = 2; { a = 3; }} print a;"))

(comment
  (test-interpreter
   "var a = 1; { a = 2; { var a = 3; print a; }} print a;"))

(comment
  (test-interpreter
   "var a = 1; { var a = a + 1; print a; } print a;"))

(comment
  (test-interpreter
   "if (1 == 2) print \"equal\"; else print \"not equal\";"))

(comment
  (test-interpreter
   "if (true) { print 1; print 2;}"))

(comment
  (test-interpreter
   "if (false) { print 1; print 2;} else print 3;"))

(comment
  (test-interpreter
   "var a = 1; if (a == 1) { print 1; print 2;} else print 3;"))

(comment
  (test-interpreter
   "var a = true; var b = false; print a or b; print a and b;"))

(comment
  (test-interpreter
   "var i = 0; while (i < 5) { i = i + 1; print i;} print \"end\";"))

(comment
  (test-interpreter
   "var i = 0; while (i != 0) { print \"not\"; } print \"end\";"))

(comment
  (test-interpreter
   "var i = 0; while (i < 5) { b = 2; }"))

(comment
  (test-interpreter
   "for (var i = 0; i < 5; i = i + 1) { print i; }"))

(comment
  (test-interpreter
   "print clock();"))

(comment
  (test-interpreter
   "var before = time(); var i = 100000; while (i > 0) { i = i - 1; } print (time() - before);"))


(comment
  (test-interpreter
   "fun hello() { print \"hello\"; } hello();"))

(comment
  (test-interpreter
   "fun hello(name) { print name; } hello();"))

(comment
  (test-interpreter
   "fun hello(name) { print name; } hello(\"manuel\");"))

(comment
  (test-interpreter
   "fun sum(a, b) { print a + b; } sum(4);"))

(comment
  (test-interpreter
   "fun sum(a, b) { print a + b; } sum(4, 5);"))

(comment
  (test-interpreter
   "var a = 10; fun sum1(b) { var a = 1; print a + b; } sum1(4); print a;"))

(comment
  (test-interpreter
   "fun sum(a, b) { return a + b; } print sum(4, 6);"))

(comment
  (test-interpreter
   "fun test(age) { if (age > 18 ) return; return \"old!\"; } print test(1); print test(20);"))

(comment
  (test-interpreter
   "fun test(age) { return; } print test(1);"))

(comment
  (test-interpreter
   "return 20;"))

(comment
  (test-interpreter "
fun makeCounter() {
  var i = 0;
  fun count() {
    i = i + 1;
    print i;
  }

  return count;
}

var counter = makeCounter();
counter();
counter();"))

(comment
  (test-interpreter "
fun makeCounter(start) {
    var i = start;
    fun count() {
        i = i + 1;
        return i;
    }
    return count;
}

var counter = makeCounter(5);
print counter();
print counter();"))

(comment
  (test-interpreter "
var x = 1;
fun test () {
    print x;
}
test ();
x = 2;
test ();"))

(comment
  (test-interpreter "
class DevonshireCream {
  serveOn() {
    return \"Scones\";
  }
}

print DevonshireCream;"))

(comment
  (test-interpreter "
class Bagel {}
var bagel = Bagel ();
print bagel;"))

(comment
  (test-interpreter "
class Bagel {}
var bagel = Bagel ();
print bagel.flavor;"))

(comment
  (test-interpreter "
class Bagel {}
var bagel = Bagel ();
bagel.flavor = 1;
print bagel.flavor;"))

(comment
  (test-interpreter "
class Bagel {}
var bagel = Bagel ();
bagel.flavor = 1;
bagel.flavor = bagel.flavor + 2;
print bagel.flavor;"))

(comment
  (test-interpreter "
class Bacon {
  eat() {
    print \"Crunch crunch crunch!\";
  }
}

Bacon().eat();"))

(comment
  (test-interpreter "
class Person {
  sayName() {
    print this.name;
  }
}
var person = Person();
person.name = \"Manuel\";
person.sayName();
"))

(comment
  (test-interpreter "
fun test() {
    return this.something;
}
"))

(comment
  (test-interpreter "
class Person {
  init() {
    this.name = \"manuel\";
  }
}
var person = Person();
print person.name;
"))

(comment
  (test-interpreter "
class Foo {
  init() {
    return;
  }
}
"))

(comment
  (test-interpreter "
class Foo {
  init() {
    return 1;
  }
}
"))

(comment
  (test-interpreter "
class Foo {
  init() {
    print this;
  }
}

var foo = Foo();
print foo.init();
"))

(comment
  (test-interpreter "class Oops < Oops {}"))

(comment
  (test-interpreter "class Oops < NotExists {}"))

(comment
  (test-interpreter "
var NotAClass = \"I am totally not a class\";
class Subclass < NotAClass {} // ?!
"))

(comment
  (test-interpreter "
class A { one() { print 1; } }
class B < A {}
var b = B();
b.one();
"))

(comment
  (test-interpreter "
class Doughnut {
  cook() {
    print \"Fry until golden brown.\";
  }
}

class BostonCream < Doughnut {
  cook() {
    super.cook();
    print \"Pipe full of custard and coat with chocolate.\";
  }
}

BostonCream().cook();
"))

(comment
  (test-interpreter "
class A {
  method() {
    print \"A method\";
  }
}

class B < A {
  method() {
    print \"B method\";
  }

  test() {
    super.method();
  }
}

class C < B {}

C().test();
"))

(comment
  (test-interpreter "
class Foo {
  getClosure() {
    fun f() {
      fun g() {
        fun h() {
          return this.toString();
        }
        return h;
      }
      return g;
    }
    return f;
  }

  toString() { return \"Foo\"; }
}

var closure = Foo().getClosure();
print closure()()();
"))

(comment
  (test-interpreter "
fun g() {
  var a = 1;
  print a;
}
g();
"))

(comment
  (test-interpreter "
\"s\" + nil;
"))


(comment
  (test-interpreter "
class Foo { a(){} }
print Foo().a;
"))

(comment
  (test-interpreter "
class Base {
  init(a) {
    this.a = a;
  }
}

class Derived < Base {
  init(a) {
    super.init(a);
  }
}

var derived = Derived(\"a\");
"))