(ns cloxure.interpreter)

(defn- lox-callable [name arity f]
  {:name name
   :arity arity
   :lox-fn f})

(def time-builtin 
  (lox-callable
   "time" 0
   (fn [state _]
     (assoc state :result (/ (System/currentTimeMillis) 1000.0)))))

(def builtins
  {"time" time-builtin})

(defn- new-state [locals]
  (let [globals (atom (assoc builtins :enclosing nil))]
    {:result nil
     :globals globals
     :environment globals
     :locals locals}))

(defn- runtime-error [message]
  (throw (ex-info message {:type :runtime})))

(defn env-ancestor [env distance]
  (if (zero? distance)
    env
    (recur (:enclosing @env) (dec distance))))

(defn- env-get [env name-token]
  (let [name (:text name-token)]
    (if (contains? @env name)
      (get @env name)
      (runtime-error (format "Undefined variable %s" name)))))

(defn- env-assign! [env name-token value]
  (let [name (:text name-token)]
    (if (contains? @env name)
      (swap! env assoc name value)
      (runtime-error (format "Undefined variable %s" name)))))

(defn- define-variable [state name-token value]
  (let [env (:environment state)]
    (swap! env assoc (:text name-token) value)
    state))

(defn- lookup-variable [state name-token expr]
  (let [distance (get-in state [:locals expr])
        env (if (nil? distance) 
              (:globals state) 
              (env-ancestor (:environment state) distance))]
    (assoc state :result (env-get env name-token))))

(defn- assign-variable [state name-token expr value]
  (let [distance (get-in state [:locals expr])
        env (if (nil? distance)
              (:globals state)
              (env-ancestor (:environment state) distance))]
    (env-assign! env name-token value)
    state))

(defn- push-scope [state]
  (let [env (:environment state)]
    (assoc state :environment (atom {:enclosing env}))))

(defn- pop-scope [state]
  (let [env (:environment state)]
    (assoc state :environment (:enclosing @env))))

(defn- truthy? [value]
  (cond
    (nil? value) false
    (instance? Boolean value) value
    :else true))

(defn- lox-fn? [value]
  (and (map? value) (fn? (:lox-fn value))))

(defn- require-num [value]
  (if (instance? Double value)
    value
    (runtime-error "Operand must be a number")))

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
             :greater (> (require-num left) (require-num right))
             :greater_equal (>= (require-num left) (require-num right))
             :less (< (require-num left) (require-num right))
             :less_equal (<= (require-num left) (require-num right))
             :minus (- (require-num left) (require-num right))
             :plus (cond
                     (and (instance? Double left) (instance? Double right))
                     (+ left right)
                     (and (instance? String left) (instance? String right))
                     (str left right)
                     :else
                     (runtime-error "Operands must be two numbers or two strings."))
             :slash (/ (require-num left) (require-num right))
             :star (* (require-num left) (require-num right))))))

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
        (define-variable (:name-token var-stmt) (:result state))
        (assoc :result nil))))

(defmethod evaluate :variable [state var-expr]
  (lookup-variable state (:name-token var-expr) var-expr))

(defmethod evaluate :assign [state assign-expr]
  (let [state (evaluate state (:value-expr assign-expr))
        value (:result state)]
    (assign-variable state (:name-token assign-expr) assign-expr value)))

; TODO: improve using reduce
(defmethod evaluate :block [state block-stmt]
  (loop [statements (:statements block-stmt)
         state (push-scope state)]
    (if (empty? statements)
      (-> state
          (pop-scope)
          (assoc :result nil))
      (recur (rest statements) (evaluate state (first statements))))))

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
      (not (lox-fn? callee))
      (runtime-error "Can only call functions and classes.")

      (not= (:arity callee) (count arguments))
      (runtime-error (format "Expected %d arguments but got %d"
                             (:arity callee)
                             (count arguments)))

      :else ((:lox-fn callee) state arguments))))

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

(defn- declare-fn-args [state params args]
  (reduce (fn [state i]
            (define-variable state (nth params i) (nth args i)))
          state
          (range (count params))))

(defn- execute-fn-body [state statements]
  (try
    (let [state (reduce evaluate state statements)]
      (assoc state :result nil))
    (catch clojure.lang.ExceptionInfo e
      (let [{type :type state :state} (ex-data e)]
        (if (= type :return) 
          state 
          (throw e))))))

(defn- lox-function [fun-stmt]
  (let [{:keys [name params body]} fun-stmt]
    (lox-callable
     (:text name)
     (count params)
     (fn [state args]
       (-> state
         (push-scope)
         (declare-fn-args params args)
         (execute-fn-body body)
         (pop-scope))))))

(defmethod evaluate :fun-stmt [state fun-stmt]
  (let [f (lox-function fun-stmt)]
    (define-variable state (:name fun-stmt) f)))

(defmethod evaluate :return-stmt [state return-stmt]
  (throw (ex-info "return" {:type :return
                            :state (evaluate state (:value return-stmt))})))

(defn interpret [statements locals]
  (loop [state (new-state locals)
         statements statements]
    (if (empty? statements)
      (:result state)
      (let [state (try 
                    (evaluate state (first statements))
                    (catch clojure.lang.ExceptionInfo e
                      (println "ERROR" (ex-message e) (prn-str (ex-data e)))
                      (assoc state :result nil)))]
        (recur state (rest statements))))))

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
              (interpret statements locals))))))))

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