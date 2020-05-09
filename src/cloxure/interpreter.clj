(ns cloxure.interpreter)

(defn- runtime-error [message]
  (throw (ex-info message {:type :runtime})))

(defn- env-get [env name-token]
  (let [name (:text name-token)]
    (if (contains? env name)
      (get env name)
      (if-let [enclosing (:enclosing env)]
        (env-get enclosing name-token)
        (runtime-error (format "Undefined variable %s" name))))))

(defn- env-define [env name-token value]
  (assoc env (:text name-token) value))

(defn- env-assign [env name-token value]
  (let [name (:text name-token)]
    (if (contains? env name)
      (assoc env name value)
      (if (:enclosing env)
        (update env :enclosing env-assign name-token value)
        (runtime-error (format "Undefined variable %s" name))))))

(defn- env-enclosed [env]
  {:enclosing env})

(defmulti evaluate
  "Interprets a Lox AST"
  (fn [node _] (:type node)))

(defmethod evaluate :literal [literal env]
  [(:value literal) env])

(defmethod evaluate :group [group env]
  (evaluate (:expression group) env))

(defn- truthy? [value]
  (cond
    (nil? value) false
    (instance? Boolean value) value
    :else true))

(defmethod evaluate :unary [unary env]
  (let [[value env] (evaluate (:right unary) env)
        op (:type (:operator unary))]
    [(case op
       :minus (- value)
       :bang (not (truthy? value)))
     env]))

(defn- require-num [value]
  (if (instance? Double value)
    value
    (runtime-error "Operand must be a number")))

(defmethod evaluate :binary [binary env]
  (let [[left env] (evaluate (:left binary) env)
        [right env] (evaluate (:right binary) env)
        op (:type (:operator binary))]
    [(case op
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
       :star (* (require-num left) (require-num right)))
     env]))

(defmethod evaluate :print-stmt [{e :expression} env]
  (let [[value env] (evaluate e env)]
    [(println value) env]))

(defmethod evaluate :var-stmt [var-stmt env]
  (let [name-token (:name-token var-stmt)
        [value env] (evaluate (:initializer var-stmt) env)]
    [nil (env-define env name-token value)]))

(defmethod evaluate :variable [var env]
  [(env-get env (:name-token var)) env])

(defmethod evaluate :assign [assign env]
  (let [{name-token :name-token value-expr :value-expr} assign
        [value env] (evaluate value-expr env)]
    [value (env-assign env name-token value)]))

(defmethod evaluate :block [block env]
  (loop [statements (:statements block)
         env (env-enclosed env)]
    (if (empty? statements)
      [nil (:enclosing env)]
      (let [stmt (first statements)
            [_ env] (evaluate stmt env)]
        (recur (rest statements) env)))))

(defn interpret [statements env]
  (if (empty? statements)
    env
    (let [stmt (first statements)
          [value env] (try
                        (evaluate stmt env)
                        (catch clojure.lang.ExceptionInfo e
                          (println "ERROR" (.getMessage e) (prn-str (ex-data e)))
                          [nil env]))]
      (prn value)
      (recur (rest statements) env))))

(require '[cloxure.scanner :as scanner])
(require '[cloxure.parser :as parser])

(defn- test-interpreter [code]
  (let [{errors :errors tokens :tokens} (scanner/scan code)]
    (if (seq errors)
      errors
      (let [{errors :errors statements :statements} (parser/parse tokens)]
        (if (seq errors)
          errors
          (interpret statements {}))))))

(comment 
  (test-interpreter 
   "print 1; print 2; print \"hello\"; print 1 + 2; 10 + 20;"))

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
