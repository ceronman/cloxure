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
      (update resolver
              :scopes
              conj others (assoc scope (:text name-token) ready?))
      resolver)))

(defn- add-local [resolver node pos]
  (update resolver :locals assoc node pos))

(defn- resolve-local [resolver node name-token]
  (loop [pos 0
         scopes (:scopes resolver)]
    (cond
      (empty? scopes) resolver
      (contains? (peek scopes) (:text name-token)) (add-local resolver node pos)
      :else (recur (inc pos) (rest scopes)))))

(defmulti static-resolve
  "Statically resolves scoping of variables"
  (fn [node _] (:type node)))

(defn resolve-statements [resolver statements]
  (reduce static-resolve resolver statements))

(defmethod static-resolve :block [resolver node]
  (let [resolver (update resolver :scopes conj {})
        resolver (resolve-statements resolver (:statements node))]
    (update resolver :scopes pop)))

(defmethod static-resolve :var-stmt [resolver node]
  (let [name-token(:name-token node)
        resolver (add-var resolver name-token false)
        initializer (:initializer node)
        resolver (if initializer (static-resolve initializer resolver) resolver)]
    (add-var resolver name-token true)))

(defmethod static-resolve :variable [resolver node]
  (let [name-token (:name-token node)
        [scope & _] (:scopes resolver)
        resolver (if (and scope (false? (get scope (:text name-token))))
                   (error resolver 
                          name-token 
                          "Cannot read local variable in its own initializer.")
                   resolver)]
    (resolve-local resolver node name-token)))

(require '[cloxure.scanner :as scanner])
(require '[cloxure.parser :as parser])

(defn- test-resolver [code]
  (let [{errors :errors tokens :tokens} (scanner/scan code)]
    (if (seq errors)
      errors
      (let [{errors :errors statements :statements} (parser/parse tokens)]
        (if (seq errors)
          errors
          (:locals (resolve-statements (new-resolver) statements)))))))

(comment (test-resolver
          "var a;"))

(comment (test-resolver
          "{ var a; a; }"))

(comment (test-resolver
          "{ var a; { a; } }"))
