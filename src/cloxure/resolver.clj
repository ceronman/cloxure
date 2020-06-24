(ns cloxure.resolver)

(defn- new-resolver []
  {:scopes '()
   :locals {}
   :errors []
   :current-fn nil
   :current-class nil})

(defn- error [resolver token message]
  (let [e {:line (:line token) :message message :loc (:text token)}]
    (update resolver :errors conj e)))

(defn- add-var [resolver name-token ready?]
  (let [[scope & others] (:scopes resolver)]
    (if scope
      (let [new-scope (assoc scope (:text name-token) ready?)]
        (assoc resolver :scopes (conj others new-scope)))
      resolver)))

(defn- add-vars [resolver name-tokens ready?]
  (reduce #(add-var %1 %2 ready?) resolver name-tokens))

(defn- add-local [resolver node pos]
  (update resolver :locals assoc node pos))

(defn- resolve-local [resolver node name-token]
  (loop [pos 0
         scopes (:scopes resolver)]
    (cond
      (empty? scopes) resolver
      (contains? (peek scopes) (:text name-token)) (add-local resolver node pos)
      :else (recur (inc pos) (rest scopes)))))

(defn- begin-scope [resolver]
  (update resolver :scopes conj {}))

(defn- end-scope [resolver]
  (update resolver :scopes pop))

(defmulti resolve-locals
  "Statically resolves scoping of variables"
  (fn [_ node] (:type node)))

(defn- resolve-statements [resolver statements]
  (reduce resolve-locals resolver statements))

(defmethod resolve-locals :block [resolver node]
  (-> resolver
      (begin-scope)
      (resolve-statements (:statements node))
      (end-scope)))

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
  (-> resolver
      (resolve-locals (:value-expr node))
      (resolve-local node (:name-token node))))

(defn- resolve-function [resolver fun-stmt fn-type]
  (let [prev-fn (:current-fn resolver)]
    (-> resolver
        (begin-scope)
        (assoc :current-fn fn-type)
        (add-vars (:params fun-stmt) true)
        (resolve-statements (:body fun-stmt))
        (assoc :current-fn prev-fn)
        (end-scope))))

(defmethod resolve-locals :fun-stmt [resolver node]
  (-> resolver
      (add-var (:name node) true)
      (resolve-function node :function)))

(defn- resolve-methods [resolver methods]
  (reduce
   (fn [resolver method]
     (if (= (-> method :name :text) "init")
       (resolve-function resolver method :initializer)
       (resolve-function resolver method :method)))
   resolver
   methods))

(defn- resolve-superclass [resolver class-stmt]
  (let [name (:text (:name-token class-stmt))
        superclass (:superclass class-stmt)]
    (if superclass
      (if (= name (:text (:name-token superclass)))
        (error resolver 
               (:name-token superclass) 
               "A class cannot inherit from itself.")
        (-> resolver
            (resolve-locals superclass)
            (begin-scope)
            (add-var {:text "super"} true)))
      resolver)))

(defmethod resolve-locals :class-stmt [resolver node]
  (let [prev-class (:current-class resolver)]
    (-> resolver
        (add-var (:name node) true)
        (resolve-superclass node)
        (begin-scope)
        (add-var {:text "this"} true) ;; TODO: hacky!
        (assoc :current-class (if (:superclass node) :subclass :class))
        (resolve-methods (:methods node))
        (assoc :current-class prev-class)
        (end-scope)
        (cond-> (:superclass node) (end-scope)))))

(defmethod resolve-locals :if-stmt [resolver node]
  (-> resolver
      (resolve-locals (:condition node))
      (resolve-locals (:then-branch node))
      (cond-> (:else-branch node) (resolve-locals (:else-branch node)))))

(defmethod resolve-locals :print-stmt [resolver node]
  (resolve-locals resolver (:expression node)))

(defmethod resolve-locals :while-stmt [resolver node]
  (-> resolver
      (resolve-locals (:condition node))
      (resolve-locals (:body node))))

(defmethod resolve-locals :binary [resolver node]
  (-> resolver
      (resolve-locals (:left node))
      (resolve-locals (:right node))))

(defmethod resolve-locals :logical [resolver node]
  (-> resolver
      (resolve-locals (:left node))
      (resolve-locals (:right node))))

(defmethod resolve-locals :unary [resolver node]
  (resolve-locals resolver (:right node)))

(defmethod resolve-locals :group [resolver node]
  (resolve-locals resolver (:expression node)))

(defmethod resolve-locals :literal [resolver _]
  resolver)

(defmethod resolve-locals :call [resolver call-expr]
  (let [resolver (resolve-locals resolver (:callee call-expr))]
    (reduce resolve-locals resolver (:arguments call-expr))))

(defmethod resolve-locals :get-expr [resolver get-expr]
  (resolve-locals resolver (:object get-expr)))

(defmethod resolve-locals :set-expr [resolver get-expr]
  (-> resolver
      (resolve-locals (:object get-expr))
      (resolve-locals (:value get-expr))))

(defmethod resolve-locals :this-expr [resolver this-expr]
  (if (nil? (:current-class resolver))
    (error resolver
           (:keyword this-expr)
           "Cannot use 'this' outside of a class.")
    (resolve-local resolver this-expr (:keyword this-expr))))

(defmethod resolve-locals :super [resolver super-expr]
  (case (:current-class resolver)
    :subclass (resolve-local resolver super-expr (:keyword super-expr))
    nil (error resolver (:keyword super-expr) 
               "Cannot use 'super' outside of a class.")
    (error resolver (:keyword super-expr) 
           "Cannot use 'super' in a class with no superclass.")))

(defmethod resolve-locals :return-stmt [resolver return-stmt]
  (if (nil? (:current-fn resolver))
    (error resolver
           (:keyword return-stmt)
           "Cannot return from top-level code.")
    (if (:value return-stmt)
      (if (= (:current-fn resolver) :initializer)
        (error resolver
               (:keyword return-stmt)
               "Cannot return a value from an initializer.")
        (resolve-locals resolver (:value return-stmt)))
      resolver)))

(require '[cloxure.scanner :as scanner])
(require '[cloxure.parser :as parser])

(defn locals [statements]
  (-> (new-resolver)
      (resolve-statements statements)))

(defn- test-resolver [code]
  (let [{errors :errors tokens :tokens} (scanner/scan code)]
    (if (seq errors)
      errors
      (let [{errors :errors statements :statements} (parser/parse tokens)]
        (if (seq errors)
          errors
          (let [{errors :errors locals :locals} (locals statements)]
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

(comment (test-resolver
          "{ var a; if (a) { a = b; } else { a = c; }}"))

(comment (test-resolver
          "{ var a; print a; }"))

(comment (test-resolver
          "{ var a; { print a; } }"))

(comment (test-resolver
          "{ var a; { a + b; a or a; while (true) {a;} !a; (a); 1;} }"))

(comment (test-resolver
          "fun test(one, two) { print one; }"))

(comment (test-resolver
          "return 1;"))

(comment (test-resolver "
class Eclair {
  cook() {
    super.cook();
    print 1;
  }
}
"))

(comment (test-resolver "
super.notEvenInAClass ();
"))
