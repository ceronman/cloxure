(ns cloxure.interpreter)

(defmulti evaluate
  "Interprets a Lox AST"
  :type)

(defmethod evaluate :literal [literal]
  (:value literal))

(defmethod evaluate :group [group]
  (evaluate (:expression group)))

(defn- truthy? [value]
  (cond
    (nil? value) false
    (instance? Boolean value) value
    :else true))

(defmethod evaluate :unary [unary]
  (let [value (evaluate (:right unary))
        op (:type (:operator unary))]
    (case op
      :minus (- value)
      :bang (not (truthy? value)))))

(defn- require-num [value]
  (if (instance? Double value)
    value
    (throw (ex-info "Operand must be a number" {:type :runtime}))))

(defmethod evaluate :binary [binary]
  (let [left (evaluate (:left binary))
        right (evaluate (:right binary))
        op (:type (:operator binary))]
    (case op
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
              (throw (ex-info 
                      "Operands must be two numbers or two strings." 
                      {:type :runtime})))
      :slash (/ (require-num left) (require-num right))
      :star (* (require-num left) (require-num right)))))

(defn interpret [expr]
  (try
    (let [value (evaluate expr)]
      (prn value))
    (catch clojure.lang.ExceptionInfo e
      (println "ERROR" (.getMessage e) (prn-str (ex-data e))))))