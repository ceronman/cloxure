(ns cloxure.parser
  (:require [cloxure.ast :as ast])
  (:require [cloxure.scanner :as scanner]))

;; Grammar
;; -----------------------------------------------------------------
;; 
;;    program   → statement* EOF ;
;;    statement → exprStmt
;;                | printStmt ;
;;    exprStmt  → expression ";" ;
;;    printStmt → "print" expression ";" ;
;;    expression     → equality ;
;;    equality       → comparison (("!=" | "==") comparison) * ;
;;    comparison     → addition ((">" | ">=" | "<" | "<=") addition) * ;
;;    addition       → multiplication (("-" | "+") multiplication) * ;
;;    multiplication → unary (("/" | "*") unary) * ;
;;    unary          → ("!" | "-") unary
;;                     | primary ;
;;    primary        → NUMBER | STRING | "false" | "true" | "nil"
;;                     | "(" expression ")" ;
;;                
;;------------------------------------------------------------------

(defn- new-parser [tokens]
  {:tokens tokens 
   :errors []
   :statements []
   :current 0
   :expr nil})

(defn- current-token [parser]
  (nth (:tokens parser) (:current parser)))

(defn- is-at-end? [parser]
  (= (:type (current-token parser)) :eof))

(defn- advance [parser]
  (update parser :current inc))

(defn- check? [parser token-type]
  (and (not (is-at-end? parser)) (= (:type (current-token parser)) token-type)))

(defn- match? [parser & token-types]
  (some #(check? parser %) token-types))

(defn- error [parser token message]
  (let [e {:line (:line token) :message message :loc (:text token)}
        parser (update parser :errors conj e)]
    (throw (ex-info "Parsing error" parser))))

(defn- consume [parser token-type message]
  (if (check? parser token-type)
    (advance parser)
    (error parser (current-token parser) message)))

(defn- synchronize [parser]
  (cond (is-at-end? parser) parser
        (match? parser :semicolon) (advance parser)
        (match? parser 
                :class :fun :var :for :if :while :print :return) parser
        :else (recur (advance parser))))

(defn- add-expr [parser expr]
  (assoc parser :expr expr))

(defn- add-literal [parser value]
  (advance (add-expr parser (ast/literal value))))

(declare expression)

(defn- primary [parser]  
  (cond 
    (match? parser :false) (add-literal parser false)
    (match? parser :true) (add-literal parser true)
    (match? parser :nil) (add-literal parser nil)
    (match? parser :number :string) (add-literal parser (:literal (current-token parser)))
    (match? parser :lparen) (let [middle (expression (advance parser))]
                              (add-expr (consume middle :rparen "Expect ')' after expression.") 
                                        (ast/group (:expr middle))))
    :else (error parser (current-token parser) "Expected expression")))

(defn- unary [parser]
  (if (match? parser :bang :minus)
    (let [operator (current-token parser)
          right (unary (advance parser))]
      (add-expr right (ast/unary operator (:expr right))))
    (primary parser)))

(defn- binary* [parser base-expr & operators]
  (loop [left (base-expr parser)]
    (if (apply match? left operators)
      (let [op (current-token left)
            right (base-expr (advance left))]
        (recur (add-expr right (ast/binary (:expr left) op (:expr right)))))
      left)))

(defn- multiplication [parser]
  (binary* parser unary :slash :star))

(defn- addition [parser]
 (binary* parser multiplication :minus :plus))

(defn- comparison [parser]
  (binary* parser addition :greater :greater_equal :less :less_equal))

(defn- equality [parser]
  (binary* parser comparison :bang_equal :equal_equal))

(defn- expression [parser]
  (equality parser))

(defn- print-stmt [parser]
  (let [after-expression (expression parser)
        after-semicolon (consume after-expression :semicolon "Expect ';' after value.")]
    (add-expr after-semicolon (ast/print-stmt (:expr after-expression)))))

(defn- expression-stmt [parser]
  (let [after-expression (expression parser)
        after-semicolon (consume after-expression :semicolon "Expect ';' after value.")]
    (add-expr after-semicolon (:expr after-semicolon))))

(defn- statement [parser]
  (if (match? parser :print)
    (print-stmt (advance parser))
    (expression-stmt parser)))

(defn- dbg [{tokens :tokens current :current}]
  (println (reduce str (map-indexed (fn [i t] (format (if (= i current) " [%s] " " %s ") (:text t))) tokens))))

(defn parse [tokens]
  (loop [parser (new-parser tokens)]
    (if (is-at-end? parser)
      parser
      (recur
       (try
         (let [after-statement (statement parser)]
           (-> after-statement
               (update :statements conj (:expr after-statement))
               (assoc :expr nil)))
         (catch clojure.lang.ExceptionInfo e
           (synchronize (ex-data e))))))))

(defn- test-parser [code]
  (let [{errors :errors tokens :tokens} (scanner/scan code)]
    (if (seq errors)
      errors
      (let [{errors :errors statements :statements} (parse tokens)]
        (if (seq errors)
          errors
          (map ast/pretty-print statements))))))

(comment (test-parser "\""))
(comment (test-parser "1;2;3;\"hello\";"))
(comment (test-parser "1+2;"))
(comment (test-parser "1 == 1;"))
(comment (test-parser "1 + 2 * 3 + 4;"))
(comment (test-parser "true != false;"))
(comment (test-parser "(1 + 2) * 3;"))
(comment (test-parser "1++;2++;"))
(comment (test-parser "print 1; print 2;"))
(comment (test-parser "1+2;"))
(comment (test-parser "(100 + 200 + 1"))
(comment (test-parser "1)"))
(comment (test-parser "(1 + nil)"))
(comment (test-parser "\"hello\" == \"world\""))


