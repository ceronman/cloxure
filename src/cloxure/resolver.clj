(ns cloxure.resolver)

(defn- new-resolver []
  {:scopes '()
   :locals {}
   :errors []})

(defn- error [resolver token message]
  (let [e {:line (:line token) :message message :loc (:text token)}]
    (update resolver :errors conj e)))

(defn- add-var [resolver name-token ready?]
  (let [[scope & others] (:scopes resolver)]
    (if scope
      (let [new-scope (assoc scope (:text name-token) ready?)]
        (assoc resolver :scopes (conj others new-scope)))
      resolver)))

(defn- add-local [resolver node pos]
  (update resolver :locals assoc node pos))

(defn- resolve-local [resolver node name-token]
  (loop [pos 0
         scopes (:scopes resolver)]
    (cond
      (empty? scopes) resolver
()      (contains? (peek scopes) (:text name-token)) (add-local resolver node pos)
      :else (recur (inc pos) (rest scopes)))))

(defmulti resolve-locals
  "Statically resolves scoping of variables"
  (fn [_ node] (:type node)))

(defn resolve-statements [resolver statements]
  (reduce resolve-locals resolver statements))

(defmethod resolve-locals :block [resolver node]
  (let [resolver (update resolver :scopes conj {})
        resolver (resolve-statements resolver (:statements node))]
    (update resolver :scopes pop)))

(defmethod resolve-locals :var-stmt [resolver node]
  (let [name-token (:name-token node)
        resolver (add-var resolver name-token false)
        initializer (:initializer node)
        resolver (if initializer (resolve-locals resolver initializer) resolver)]
    (add-var resolver name-token true)))

(defmethod resolve-locals :variable [resolver node]
  (let [name-token (:name-token node)
        [scope & _] (:scopes resolver)
        resolver (if (and scope (false? (get scope (:text name-token))))
                   (error resolver
                          name-token
                          "Cannot read local variable in its own initializer.")
                   resolver)]
    (resolve-local resolver node name-token)))

(defmethod resolve-locals :assign [resolver node]
  (let [resolver (resolve-locals resolver (:value-expr node))]
    (resolve-local resolver node (:name-token node))))

(defmethod resolve-locals :if-stmt [resolver node]
  (let [resolver (resolve-locals resolver (:condition node))
        resolver (resolve-locals resolver (:then-branch node))]
    (if-let [else-branch (:else-branch node)]
      (resolve-locals resolver else-branch)
      resolver)))

(require '[cloxure.scanner :as scanner])
(require '[cloxure.parser :as parser])

(defn- test-resolver [code]
  (let [{errors :errors tokens :tokens} (scanner/scan code)]
    (if (seq errors)
      errors
      (let [{errors :errors statements :statements} (parser/parse tokens)]
        (if (seq errors)
          errors
          (let [{errors :errors locals :locals} (resolve-statements (new-resolver) statements)]
            (if (seq errors)
              errors
              locals)))))))

(comment (test-resolver
          "var a;"))

(comment (test-resolver
          "{ var a; a; }"))

(comment (test-resolver
          "{ var a; { a; } }"))

(comment (test-resolver
          "{ var a; { var a = a; } }"))

(comment (test-resolver
          "{ var a; var b; a = b; }"))

(comment (test-resolver
          "{ var a; if (a) { a = b; } }"))
