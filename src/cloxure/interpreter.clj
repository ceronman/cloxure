(ns cloxure.interpreter)

(defn- new-state [locals]
  {:result nil
   :environment nil
   :globals {}
   :locals locals})

(defn- runtime-error [message]
  (throw (ex-info message {:type :runtime})))

(defn- env-get [env name-token]
  (let [name (:text name-token)]
    (if (contains? env name)
      (get env name)
      (runtime-error (format "Undefined variable %s" name)))))

(defn- env-get-at [env name-token distance]
  (if (zero? distance)
    (env-get env name-token)
    (recur (:enclosing env) name-token (dec distance))))

(defn- env-assign [env name-token value]
  (let [name (:text name-token)]
    (if (contains? env name)
      (assoc env name value)
      (runtime-error (format "Undefined variable %s" name)))))

(defn- env-assign-at [env name-token distance value]
  (if (zero? distance)
    (env-assign env name-token value)
    (update env :enclosing env-assign-at name-token (dec distance) value)))

(defn- declare-variable [state name-token value]
  (if (:environment state)
    (update state :environment assoc (:text name-token) value)
    (update state :globals assoc (:text name-token) value)))

(defn- lookup-variable [state name-token expr]
  (if-let [distance (get-in state [:locals expr])]
    (assoc state :result (env-get-at (:environment state) name-token distance))
    (assoc state :result (env-get (:globals state) name-token))))

(defn- assign-variable [state name-token expr value]
  (if-let [distance (get-in state [:locals expr])]
    (update state :environment env-assign-at name-token distance value)
    (update state :globals env-assign name-token value)))

(defn- push-scope [state]
  (assoc state :environment {:enclosing (:environment state)}))

(defn- pop-scope [state]
  (assoc state :environment (:enclosing (:environment state))))

(defn- truthy? [value]
  (cond
    (nil? value) false
    (instance? Boolean value) value
    :else true))

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
        (declare-variable (:name-token var-stmt) (:result state))
        (assoc :result nil))))

(defmethod evaluate :variable [state var-expr]
  (lookup-variable state (:name-token var-expr) var-expr))

(defmethod evaluate :assign [state assign-expr]
  (let [state (evaluate state (:value-expr assign-expr))
        value (:result state)]
    (assign-variable state (:name-token assign-expr) assign-expr value)))

(defmethod evaluate :block [state block-stmt]
  (loop [statements (:statements block-stmt)
         state (push-scope state)]
    (if (empty? statements)
      (-> state
          (pop-scope)
          (assoc :result nil))
      (recur (rest statements) (evaluate state (first statements))))))

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

(defn interpret [statements locals]
  (loop [state (new-state locals)
         statements statements]
    (if (empty? statements)
      (:result state)
      (let [state (try 
                    (evaluate state (first statements))
                    (catch clojure.lang.ExceptionInfo e
                      (println "ERROR" (.getMessage e) (prn-str (ex-data e)))
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
