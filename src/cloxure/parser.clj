(ns cloxure.parser
  (:require [cloxure.ast :as ast])
  (:require [cloxure.scanner :as scanner]))

;; Grammar
;; -----------------------------------------------------------------
;; 
;;    program        → declaration* EOF ;
;;    declaration    → varDecl
;;                     | statement ;
;;    statement      → exprStmt
;;                     | forStmt
;;                     | ifStmt
;;                     | printStmt
;;                     | whileStmt
;;                     | block ;
;;    exprStmt       → expression ";" ;
;;    ifStmt         → "if" "(" expression ")" statement ("else" statement) ? ;
;;    printStmt      → "print" expression ";" ;
;;    whileStmt      → "while" "(" expression ")" statement ;
;;    forStmt        → "for" "(" (varDecl | exprStmt | ";")
;;                               expression? ";"
;;                               expression? ")" statement ;
;;    block          → "{" declaration* "}" ;
;;    varDecl        → "var" IDENTIFIER ("=" expression) ? ";" ;
;;    expression     → assignment ;
;;    assignment     → identifier "=" assignment
;;                     | logic_or ;
;;    logic_or       → logic_and ("or" logic_and) * ;
;;    logic_and      → equality ("and" equality) * ;
;;    equality       → comparison (("!=" | "==") comparison) * ;
;;    comparison     → addition ((">" | ">=" | "<" | "<=") addition) * ;
;;    addition       → multiplication (("-" | "+") multiplication) * ;
;;    multiplication → unary (("/" | "*") unary) * ;
;;    unary          → ("!" | "-") unary
;;                     | primary ;
;;    primary        → NUMBER | STRING | "false" | "true" | "nil"
;;                     | "(" expression ")" 
;;                     | IDENTIFIER ;
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
    (match? parser :identifier) (advance (add-expr parser (ast/variable (current-token parser))))
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

(defn- logical* [parser base-expr & operators]
  (loop [left (base-expr parser)]
    (if (apply match? left operators)
      (let [op (current-token left)
            right (base-expr (advance left))]
        (recur (add-expr right (ast/logical (:expr left) op (:expr right)))))
      left)))

(defn- logical-and [parser]
  (logical* parser equality :and))

(defn- logical-or [parser]
  (logical* parser logical-and :or))

(defn- assignment [parser]
  (let [left (logical-or parser)
        left-expr (:expr left)]
    (if (match? left :equal)
      (let [after-equals (advance left)
            after-value (assignment after-equals)]
        (if (= (:type left-expr) :variable)
          (add-expr after-value (ast/assign (:name-token left-expr) (:expr after-value)))
          (error after-equals (current-token after-equals) "Invalid assignment target.")))
      left)))

(defn- expression [parser]
  (assignment parser))

(defn- print-stmt [parser]
  (let [after-expression (expression parser)
        after-semicolon (consume after-expression :semicolon "Expect ';' after value.")]
    (add-expr after-semicolon (ast/print-stmt (:expr after-expression)))))

(defn- expression-stmt [parser]
  (let [after-expression (expression parser)
        after-semicolon (consume after-expression :semicolon "Expect ';' after value.")]
    (add-expr after-semicolon (:expr after-semicolon))))

(declare declaration)

(defn- block [parser]
  (loop [parser parser
         statements []]
    (if (check? parser :rbrace)
      (add-expr (consume parser :rbrace "Expect '}' after block.")
                (ast/block statements))
      (let [after-declaration (declaration parser)]
        (recur after-declaration (conj statements (:expr after-declaration)))))))

(declare if-stmt)
(declare while-stmt)
(declare for-stmt)

(defn- statement [parser]
  (cond 
    (match? parser :for) (for-stmt (advance parser))
    (match? parser :if) (if-stmt (advance parser))
    (match? parser :print) (print-stmt (advance parser))
    (match? parser :while) (while-stmt (advance parser))
    (match? parser :lbrace) (block (advance parser))
    :else (expression-stmt parser)))

(defn- var-declaration [after-var]
  (let [after-identifier (consume after-var :identifier "Expect variable name.")
        initializer? (match? after-identifier :equal)
        before-semicolon (if initializer? (expression (advance after-identifier)) after-identifier)
        after-semicolon (consume before-semicolon :semicolon "Expect ';' after value.")]
    (add-expr after-semicolon (ast/var-stmt (current-token after-var)
                                            (if initializer? (:expr before-semicolon) nil)))))

(defn- declaration [parser]
  (if (match? parser :var)
    (var-declaration (advance parser))
    (statement parser)))

(defn- if-stmt [parser]
  (let [after-lparen (consume parser :lparen 
                              "Expect '(' after 'if'.")
        condition (expression after-lparen)
        after-rparen (consume condition :rparen 
                              "Expect ')' after if condition.")
        after-then (statement after-rparen)
        after-else (when (check? after-then :else)
                      (statement (advance after-then)))]
    (add-expr (or after-else after-then)
              (ast/if-stmt (:expr condition)
                           (:expr after-then)
                           (:expr after-else)))))

(defn- while-stmt [parser]
  (let [after-lparen (consume parser :lparen "Expect '(' after 'while'.")
        condition (expression after-lparen)
        after-rparen (consume condition :rparen "Expect ')' after condition.")
        after-body (statement after-rparen)]
    (add-expr after-body 
              (ast/while-stmt (:expr condition) 
                              (:expr after-body)))))

(defn- for-stmt [parser]
  (let [after-lparen (consume parser :lparen "Expect '(' after 'for'.")
        initializer (cond
                      (match? after-lparen :semicolon) (advance (add-expr after-lparen nil))
                      (match? after-lparen :var) (var-declaration (advance after-lparen))
                      :else (expression-stmt after-lparen))
        condition (if (match? initializer :semicolon)
                    (add-literal initializer true)
                    (expression-stmt initializer))
        increment (if (match? condition :rparen)
                    (add-expr condition nil)
                    (expression condition))
        after-rparen (consume increment :rparen "Expect ')' after for clauses.")
        body (statement after-rparen)
        expr (:expr body)
        expr (if-let [increment-expr (:expr increment)]
               (ast/block [expr increment-expr])
               expr)
        expr (ast/while-stmt (:expr condition) expr)
        expr (if-let [initializer-expr (:expr initializer)]
               (ast/block [initializer-expr expr])
               expr)]
    (add-expr body expr)))

(defn parse [tokens]
  (loop [parser (new-parser tokens)]
    (if (is-at-end? parser)
      parser
      (recur
       (try
         (let [after-statement (declaration parser)]
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
          (doseq [s statements]
            (println (ast/pretty-print s))))))))

(comment (test-parser "\""))
(comment (test-parser "1;"))
(comment (test-parser "var a = 1; a = 2;"))
(comment (test-parser "var a = 1; a+b = 1;"))
(comment (test-parser "var a = 1; print a;"))
(comment (test-parser "1;2;3;\"hello\";"))
(comment (test-parser "1+2;"))
(comment (test-parser "1 == 1;"))
(comment (test-parser "1 + 2 * 3 + 4;"))
(comment (test-parser "true != false;"))
(comment (test-parser "(1 + 2) * 3;"))
(comment (test-parser "var hello;"))
(comment (test-parser "var hello = 1;"))
(comment (test-parser "var hello = 1 + 2;"))
(comment (test-parser "1++;2++;"))
(comment (test-parser "print 1; print 2;"))
(comment (test-parser "1+2;"))
(comment (test-parser "(100 + 200 + 1"))
(comment (test-parser "1)"))
(comment (test-parser "(1 + nil)"))
(comment (test-parser "\"hello\" == \"world\""))
(comment (test-parser "{}"))
(comment (test-parser "{1; 2; 3;}"))
(comment (test-parser "{print 1+2;}"))
(comment (test-parser "if (true) print 1; else print 2;"))
(comment (test-parser "if (true) print 1;"))
(comment (test-parser "if (true) {print 1; print 2;}"))
(comment (test-parser "if (true) {print 1; print 2;} else { print 3; }"))
(comment (test-parser "if (true print 1;"))
(comment (test-parser "if (print 1;)"))
(comment (test-parser "else"))
(comment (test-parser "if (true) if (false) print 1; else print 2;"))
(comment (test-parser "a == 1 or b == 2 and b > 3;"))
(comment (test-parser "while (true) print 1;"))
(comment (test-parser "while (true) { print 1; print 2; }"))
(comment (test-parser "while 1 == 2 { print 1; print 2; }"))
(comment (test-parser "for (;;) print 1;"))
(comment (test-parser "for (i = 0; i < 10; i = i + 1) print i;"))
(comment (test-parser "for (var i = 0; i < 10; i = i + 1) print i;"))
(comment (test-parser "var i = 0; for (; i < 10; ) print i;"))




