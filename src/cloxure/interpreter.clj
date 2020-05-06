(ns cloxure.interpreter)

(defn- runtime-error [message]
  (throw (ex-info message {:type :runtime})))

(defn- env-get [env token-name]
  (let [name (:text token-name)]
    (if (contains? env name)
      (get env name)
      (runtime-error (format "Undefined variable %s" name)))))

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

(defn interpret [statements]
  (loop [statements statements
         env {}]
    (when (seq statements)
      (let [stmt (first statements)
            _ (prn stmt)
            [value env] (try
                          (evaluate stmt env)
                          (catch clojure.lang.ExceptionInfo e
                            (println "ERROR" (.getMessage e) (prn-str (ex-data e)))
                            [nil env]))]
        (prn value)
        (recur (rest statements) env)))))

(require '[cloxure.scanner :as scanner])
(require '[cloxure.parser :as parser])

(defn- test-interpreter [code]
  (let [{errors :errors tokens :tokens} (scanner/scan code)]
    (if (seq errors)
      errors
      (let [{errors :errors statements :statements} (parser/parse tokens)]
        (if (seq errors)
          errors
          (interpret statements))))))

(comment (test-interpreter "print 1; print 2; print \"hello\"; print 1 + 2; 10 + 20;"))
