(ns cloxure.interpreter)

; TODO: this is for debugging purposes, remove later.
(set! *print-level* 5)

(defprotocol LoxCallable
  "Represent a callable object in Lox"
  (arity [this])
  (call [this state arguments]))

(def time-builtin
  (reify LoxCallable
    (call [this state _]
      (assoc state :result (/ (System/currentTimeMillis) 1000.0)))
    (arity [this] 0)))

(def builtins
  {"time" time-builtin})

(defn new-interpreter-state []
  (let [globals (atom (assoc builtins :enclosing nil))]
    {:result nil
     :errors []
     :globals globals
     :environment globals
     :locals {}}))

(defn- runtime-error [token message]
  (throw (ex-info "Lox Runtime Error" 
                  {:type :runtime :token token :message message})))

(defn- env-ancestor [env distance]
  (if (zero? distance)
    env
    (recur (:enclosing @env) (dec distance))))

(defn- env-get-at [env distance name]
  (let [env (env-ancestor env distance)]
    (get @env name)))

(defn- env-get [env name-token]
  (let [name (:text name-token)]
   (if (contains? @env name)
     (get @env name)
     (runtime-error name-token (format "Undefined variable '%s'." name)))))

(defn- env-declare! [env name value]
  (swap! env assoc name value)
  env)

(defn- env-assign! [env name-token value]
  (let [name (:text name-token)]
    (if (contains? @env name)
     (swap! env assoc name value)
     (runtime-error name-token (format "Undefined variable '%s'." name)))))

(defn- declare-variable [state name-token value]
  (let [env (:environment state)]
    (env-declare! env (:text name-token) value)
    state))

(defn- lookup-variable [state name-token expr]
  (if-let [distance (get-in state [:locals expr])]
    (assoc state :result (env-get-at (:environment state) distance (:text state)))
    (assoc state :result (env-get (:globals state) name-token))))

(defn- assign-variable [state name-token expr value]
  (let [distance (get-in state [:locals expr])
        env (if (nil? distance)
              (:globals state)
              (env-ancestor (:environment state) distance))]
    (env-assign! env name-token value)
    state))

(defn- new-scope [parent-env]
  (atom {:enclosing parent-env}))

(defn- truthy? [value]
  (cond
    (nil? value) false
    (instance? Boolean value) value
    :else true))

(defn- numeric-operation [op-token operator left right]
  (if (and (instance? Double left) (instance? Double right))
    (operator left right)
    (runtime-error op-token "Operand must be a number")))

(defmulti evaluate
  "Interprets a Lox AST"
  (fn [_ node] (:type node)))

(defmethod evaluate :literal [state literal-expr]
  (assoc state :result (:value literal-expr)))

(defmethod evaluate :group [state group-expr]
  (evaluate state (:expression group-expr)))

(defmethod evaluate :unary [state unary-expr]
  (let [state (evaluate state (:right unary-expr))
        right (:result state)
        operator (:type (:operator unary-expr))]
    (assoc state :result (case operator
                           :minus (- right)
                           :bang (not (truthy? right))))))

(defmethod evaluate :binary [state binary-expr]
  (let [state (evaluate state (:left binary-expr))
        left (:result state)
        state (evaluate state (:right binary-expr))
        right (:result state)
        operator (:type (:operator binary-expr))]
    (assoc state :result
           (case operator
             :bang_equal (not= left right)
             :equal_equal (= left right)
             :greater (numeric-operation operator > left right)
             :greater_equal (numeric-operation operator >= left right)
             :less (numeric-operation operator < left right)
             :less_equal (numeric-operation operator <= left right)
             :minus (numeric-operation operator - left right)
             :plus (cond
                     (and (instance? Double left) (instance? Double right))
                     (+ left right)
                     (and (instance? String left) (instance? String right))
                     (str left right)
                     :else
                     (runtime-error operator 
                                    "Operands must be two numbers or two strings."))
             :slash (numeric-operation operator / left right)
             :star (numeric-operation operator * left right)))))

(defmethod evaluate :logical [state logical-expr]
  (let [state (evaluate state (:left logical-expr))
        left (:result state)
        operator (:type (:operator logical-expr))
        left-truthy? (truthy? left)]
    (cond
      (and (= operator :or) left-truthy?) state
      (and (= operator :and) (not left-truthy?)) state
      :else (evaluate state (:right logical-expr)))))

(defmethod evaluate :print-stmt [state print-stmt]
  (let [state (evaluate state (:expression print-stmt))]
    (assoc state :result (println (:result state)))))

(defmethod evaluate :var-stmt [state var-stmt]
  (let [initializer (:initializer var-stmt)
        state (if initializer
                (evaluate state initializer)
                (assoc state :result nil))]
    (-> state
        (declare-variable (:name-token var-stmt) (:result state))
        (assoc :result nil))))

(defmethod evaluate :variable [state var-expr]
  (lookup-variable state (:name-token var-expr) var-expr))

(defmethod evaluate :assign [state assign-expr]
  (let [state (evaluate state (:value-expr assign-expr))
        value (:result state)]
    (assign-variable state (:name-token assign-expr) assign-expr value)))

(defn- evaluate-statements [state statements]
  (reduce evaluate state statements))

(defmethod evaluate :block [state block-stmt]
  (let [env (:environment state)]
    (-> state
        (assoc :environment (new-scope env))
        (evaluate-statements (:statements block-stmt))
        (assoc :environment env)
        (assoc :result nil))))

(defn- evaluate-args [state args]
  (reduce
   (fn [state arg]
     (let [all-results (:result state)
           state (evaluate state arg)]
       (assoc state :result (conj all-results (:result state)))))
   (assoc state :result [])
   args))

(defmethod evaluate :call [state call-expr]
  (let [state (evaluate state (:callee call-expr))
        callee (:result state)
        state (evaluate-args state (:arguments call-expr))
        arguments (:result state)]
    (cond
      (not (satisfies? LoxCallable callee))
      (runtime-error (:paren call-expr) "Can only call functions and classes.")

      (not= (arity callee) (count arguments))
      (runtime-error (:paren call-expr)
                     (format "Expected %d arguments but got %d"
                             (:arity callee)
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
      (let [{type :type state :state} (ex-data e)]
        (if (= type :return)
          (if initializer? (assoc state :result (env-get-at closure 0 "this")) state)
          (throw e))))))

(defrecord LoxFunction [declaration closure initializer?]
  LoxCallable
  (arity [this] (-> this :declaration :params count))
  (call [this state args]
        (let [declaration (:declaration this)
              env (:environment state)
              closure (:closure this)]
          (-> state
              (assoc :environment (new-scope closure))
              (declare-fn-args (:params declaration) args)
              (execute-fn-body (:body declaration) closure (:initializer? this))
              (assoc :environment env)))))

(defn lox-fn-bind [lox-fn instance]
  (let [env (new-scope (:closure lox-fn))]
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
        name (:text name-token)
        method (find-method (:lox-class object) name)]
    (cond
      method (lox-fn-bind method object)
      (contains? @fields name) (get @fields name)
      :else (runtime-error name-token (str "Undefined property '" name "'.")))))

(defrecord LoxClass [name superclass methods]
  LoxCallable
  (arity [this]
    (if-let [initializer (find-method this "init")]
      (arity initializer)
      0))
  (call [this state args]
    (let [instance (new-instance this)
          initializer (find-method this "init")
          state (if initializer
                  (call (lox-fn-bind initializer instance) state args)
                  state)]
      (assoc state :result instance))))

(defn- instance-set! [object name-token value]
  (swap! (:fields object) assoc (:text name-token) value))

(defmethod evaluate :this-expr [state this-expr]
  (lookup-variable state (:keyword this-expr) this-expr))

(defmethod evaluate :get-expr [state get-expr]
  (let [name-token (:name-token get-expr)
        state (evaluate state (:object get-expr))
        object (:result state)]
    (if (instance? LoxInstance object)
      (assoc state :result (instance-get object name-token))
      (runtime-error name-token "Only instances have properties."))))

(defmethod evaluate :set-expr [state set-expr]
  (let [name-token (:name-token set-expr)
        state (evaluate state (:object set-expr))
        object (:result state)]
    (if (instance? LoxInstance object)
      (let [state (evaluate state (:value set-expr))
            value (:result state)]
        (instance-set! object name-token value)
        (assoc state :result value))
      (runtime-error name-token "Only instances have fields"))))

(defmethod evaluate :super [state super-expr]
  (let [distance (get-in state [:locals super-expr])
        env (:environment state)
        lox-class (env-get-at env distance "super")
        env (env-ancestor (:environment state) (dec distance))
        object (env-get-at env (dec distance) "this")
        method-name (:text (:method super-expr))
        method (find-method lox-class method-name)]
    (if method
      (assoc state :result (lox-fn-bind method object))
      (runtime-error (:method super-expr) 
                     (str "Undefined property '" method-name "'.")))))

(defmethod evaluate :if-stmt [state if-stmt]
  (let [state (evaluate state (:condition if-stmt))]
    (if (truthy? (:result state))
      (evaluate state (:then-branch if-stmt))
      (if-let [else-branch (:else-branch if-stmt)]
        (evaluate state else-branch)
        (assoc state :result nil)))))

(defmethod evaluate :while-stmt [state while-stmt]
  (let [state (evaluate state (:condition while-stmt))]
    (if (truthy? (:result state))
      (recur (evaluate state (:body while-stmt)) while-stmt)
      (assoc state :result nil))))

(defmethod evaluate :fun-stmt [state fun-stmt]
  (let [f (->LoxFunction fun-stmt (:environment state) false)]
    (declare-variable state (:name fun-stmt) f)))

(defmethod evaluate :return-stmt [state return-stmt]
  (let [value (:value return-stmt)
        state (if value (evaluate state value) (assoc state :result nil))]
    (throw (ex-info "return" {:type :return
                              :state state}))))

(defn- evaluate-superclass [state class-stmt]
  (if-let [superclass (:superclass class-stmt)]
    (let [state (evaluate state superclass)
          value (:result state)]
      (if (instance? LoxClass value)
        state
        (runtime-error (:name-token superclass) "Superclass must be a class.")))
    (assoc state :result nil)))

(defmethod evaluate :class-stmt [state class-stmt]
  (let [name-token (:name-token class-stmt)
        state (evaluate-superclass state class-stmt)
        superclass (:result state)
        state (declare-variable state name-token nil) ; TODO ?? predeclare necesssary?
        env (:environment state)
        env (if superclass 
              (env-declare! (new-scope env) "super" superclass) 
              env)
        methods (->> (:methods class-stmt)
                     (map (fn [fun-stmt]
                            (let [name (:text (:name fun-stmt))]
                              [name (->LoxFunction fun-stmt env (= name "init"))])))
                     (into {}))
        lox-class (->LoxClass (:text name-token) superclass methods)]
    (assign-variable state name-token class-stmt lox-class)))


(defn interpret [state statements locals]
  (try
    (evaluate-statements (update state :locals merge locals) statements)
    (catch clojure.lang.ExceptionInfo e
      (case (:type (ex-data e))
        :runtime (-> state
                     (update :errors conj (ex-data e))
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
   "print time();"))

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




