(ns cloxure.parser
  (:require [cloxure.token :as token]
            [cloxure.ast :as ast]))

;; Grammar
;; -----------------------------------------------------------------
;; 
;;    program        → declaration* EOF ;
;;    declaration    → classDecl
;;                     | funDecl
;;                     | varDecl
;;                     | statement ;
;;    classDecl      → "class" IDENTIFIER ("<" IDENTIFIER) ?
;;                     "{" function* "}" ;
;;    funDecl        → "fun" function ;
;;    function       → IDENTIFIER "(" parameters? ")" block ;
;;    parameters     → IDENTIFIER ("," IDENTIFIER) * ;
;;    statement      → exprStmt
;;                     | forStmt
;;                     | ifStmt
;;                     | printStmt
;;                     | returnStmt
;;                     | whileStmt
;;                     | block ;
;;    returnStmt     → "return" expression? ";" ;
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
;;    assignment     → (call ".") ? IDENTIFIER "=" assignment
;;                     | logic_or;
;;    logic_or       → logic_and ("or" logic_and) * ;
;;    logic_and      → equality ("and" equality) * ;
;;    equality       → comparison (("!=" | "==") comparison) * ;
;;    comparison     → addition ((">" | ">=" | "<" | "<=") addition) * ;
;;    addition       → multiplication (("-" | "+") multiplication) * ;
;;    multiplication → unary (("/" | "*") unary) * ;
;;    unary          → ("!" | "-") unary | call ;
;;    call           → primary ("(" arguments? ")" | "." IDENTIFIER) * ;
;;    primary        → "true" | "false" | "nil" | "this"
;;                     | NUMBER | STRING | IDENTIFIER | "(" expression ")"
;;                     | "super" "." IDENTIFIER ;
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
  (= (::token/type (current-token parser)) ::token/eof))

(defn- advance [parser]
  (update parser :current inc))

(defn- check? [parser token-type]
  (and (not (is-at-end? parser)) (= (::token/type (current-token parser)) token-type)))

(defn- match? [parser & token-types]
  (some #(check? parser %) token-types))

(defn- add-error [parser message]
  (update parser :errors conj {:type :parser
                               :token (current-token parser)
                               :message message}))

(defn- error [parser message]
  (let [parser (add-error parser message)]
    (throw (ex-info "Parsing error" parser))))

(defn- consume [parser token-type message]
  (if (check? parser token-type)
    (advance parser)
    (error parser message)))

(defn- synchronize [parser]
  (cond (is-at-end? parser) parser
        (match? parser ::token/semicolon) (advance parser)
        (match? parser
                ::token/class ::token/fun ::token/var ::token/for ::token/if
                ::token/while ::token/print ::token/return) parser
        :else (recur (advance parser))))

(defn- add-expr [parser expr]
  (let [expr (assoc expr ::ast/location (:current parser))]
    (assoc parser :expr expr)))

(defn- add-literal [parser value]
  (advance (add-expr parser (ast/literal value))))

(declare expression)

(defn- add-super [parser]
  (let [after-dot (consume (advance parser) ::token/dot
                           "Expect '.' after 'super'.")
        after-idt (consume after-dot ::token/identifier
                           "Expect superclass method name.")]
    (add-expr after-idt (ast/super (current-token parser) (current-token after-dot)))))

(defn- primary [parser]
  (cond
    (match? parser ::token/false) (add-literal parser false)
    (match? parser ::token/true) (add-literal parser true)
    (match? parser ::token/nil) (add-literal parser nil)
    (match? parser ::token/number ::token/string) (add-literal parser (::token/literal (current-token parser)))
    (match? parser ::token/super) (add-super parser)
    (match? parser ::token/this) (add-expr (advance parser) (ast/this-expr (current-token parser)))

    ;; TODO: Weird advance pos
    (match? parser ::token/identifier) (advance (add-expr parser (ast/variable (current-token parser))))
    (match? parser ::token/lparen) (let [middle (expression (advance parser))]
                              (add-expr (consume middle ::token/rparen "Expect ')' after expression.")
                                        (ast/group (:expr middle))))
    :else (error parser "Expect expression.")))

(defn- finish-call [after-lparen callee]
  (loop [parser after-lparen
         arguments []]
    (if (match? parser ::token/rparen)
      (add-expr (advance parser) (ast/call callee (current-token parser) arguments))
      (let [after-arg (expression parser)
            after-arg (if (>= (count arguments) 255)
                        (advance (add-error parser "Cannot have more than 255 arguments."))
                        after-arg)
            arguments (conj arguments (:expr after-arg))]
        (if (match? after-arg ::token/comma)
          (recur (advance after-arg) arguments)
          (let [after-rparen (consume after-arg ::token/rparen "Expect ')' after arguments.")]
            (add-expr after-rparen (ast/call callee (current-token after-arg) arguments))))))))

(defn- call [parser]
  (loop [parser (primary parser)]
    (cond
      (match? parser ::token/lparen)
      (recur (finish-call (advance parser) (:expr parser)))

      (match? parser ::token/dot)
      (let [after-dot (advance parser)
            after-identifier (consume after-dot ::token/identifier
                                      "Expect property name after '.'.")]
        (recur (add-expr after-identifier
                         (ast/get-expr (:expr after-identifier) (current-token after-dot)))))
      :else
      parser)))

(defn- unary [parser]
  (if (match? parser ::token/bang ::token/minus)
    (let [operator (current-token parser)
          right (unary (advance parser))]
      (add-expr right (ast/unary operator (:expr right))))
    (call parser)))

(defn- binary* [parser base-expr & operators]
  (loop [left (base-expr parser)]
    (if (apply match? left operators)
      (let [op (current-token left)
            right (base-expr (advance left))]
        (recur (add-expr right (ast/binary (:expr left) op (:expr right)))))
      left)))

(defn- multiplication [parser]
  (binary* parser unary ::token/slash ::token/star))

(defn- addition [parser]
  (binary* parser multiplication ::token/minus ::token/plus))

(defn- comparison [parser]
  (binary* parser addition ::token/greater ::token/greater_equal ::token/less ::token/less_equal))

(defn- equality [parser]
  (binary* parser comparison ::token/bang_equal ::token/equal_equal))

(defn- logical* [parser base-expr & operators]
  (loop [left (base-expr parser)]
    (if (apply match? left operators)
      (let [op (current-token left)
            right (base-expr (advance left))]
        (recur (add-expr right (ast/logical (:expr left) op (:expr right)))))
      left)))

(defn- logical-and [parser]
  (logical* parser equality ::token/and))

(defn- logical-or [parser]
  (logical* parser logical-and ::token/or))

(defn- assignment [parser]
  (let [left (logical-or parser)
        left-expr (:expr left)]
    (if (match? left ::token/equal)
      (let [after-equals (advance left)
            after-value (assignment after-equals)]
        (case (::ast/type left-expr)
          ::ast/variable (add-expr after-value (ast/assign (::ast/name-token left-expr)
                                                           (:expr after-value)))
          ::ast/get-expr (add-expr after-value (ast/set-expr (::ast/object left-expr)
                                                             (::ast/name-token left-expr)
                                                             (:expr after-value)))

          (error left "Invalid assignment target.")))
      left)))

(defn- expression [parser]
  (assignment parser))

(defn- print-stmt [parser]
  (let [after-expression (expression parser)
        after-semicolon (consume after-expression ::token/semicolon "Expect ';' after value.")]
    (add-expr after-semicolon (ast/print-stmt (:expr after-expression)))))

(defn- return-stmt [parser]
  (let [keyword (current-token parser)
        after-keyword (advance parser)
        after-value (if (match? after-keyword ::token/semicolon)
                      (assoc after-keyword :expr nil)
                      (expression after-keyword))
        after-semicolon (consume after-value ::token/semicolon "Expect ';' after return value.")]
    (add-expr after-semicolon (ast/return-stmt keyword (:expr after-value)))))

(defn- expression-stmt [parser]
  (let [after-expression (expression parser)
        after-semicolon (consume after-expression ::token/semicolon "Expect ';' after expression.")]
    (add-expr after-semicolon (:expr after-semicolon))))

(declare declaration)

(defn- block [parser]
  (loop [parser parser
         statements []]
    (if (check? parser ::token/rbrace)
      (add-expr (consume parser ::token/rbrace "Expect '}' after block.")
                (ast/block statements))
      (let [after-declaration (declaration parser)]
        (recur after-declaration (conj statements (:expr after-declaration)))))))

(declare if-stmt)
(declare while-stmt)
(declare for-stmt)

(defn- statement [parser]
  ; TODO use condp?
  (cond
    (match? parser ::token/for) (for-stmt (advance parser))
    (match? parser ::token/if) (if-stmt (advance parser))
    (match? parser ::token/print) (print-stmt (advance parser))
    (match? parser ::token/return) (return-stmt parser)
    (match? parser ::token/while) (while-stmt (advance parser))
    (match? parser ::token/lbrace) (block (advance parser))
    :else (expression-stmt parser)))

(defn- var-declaration [after-var]
  (let [after-identifier (consume after-var ::token/identifier "Expect variable name.")
        initializer? (match? after-identifier ::token/equal)
        before-semicolon (if initializer? (expression (advance after-identifier)) after-identifier)
        after-semicolon (consume before-semicolon ::token/semicolon "Expect ';' after expression.")]
    (add-expr after-semicolon (ast/var-stmt (current-token after-var)
                                            (if initializer? (:expr before-semicolon) nil)))))
(defn- parameters [after-lparen]
  (if (match? after-lparen ::token/rparen)
    [(advance after-lparen) []]
    (loop [parser after-lparen
           params []]
      (let [after-param (consume parser ::token/identifier "Expect parameter name.")
            after-param (if (>= (count params) 255)
                          (advance (add-error parser "Cannot have more than 255 parameters."))
                          after-param)
            params (conj params (current-token parser))]
        (if (match? after-param ::token/comma)
          (recur (advance after-param) params)
          [(consume after-param ::token/rparen "Expect ')' after parameters.") params])))))

(defn- function [after-fun kind]
  (let [after-name (consume after-fun ::token/identifier
                            (str "Expect " kind " name."))
        name-token (current-token after-fun)
        after-lparen (consume after-name ::token/lparen
                              (str "Expect '(' after " kind " name."))
        [after-rparen params] (parameters after-lparen)
        after-lbrace (consume after-rparen ::token/lbrace (str "Expect '{' before " kind " body."))
        after-block (block after-lbrace)
        body (::ast/statements (:expr after-block))]
    (add-expr after-block (ast/fun-stmt name-token params body))))

(defn- class-declaration [after-class]
  (let [after-name (consume after-class ::token/identifier "Expect class name.")
        name (current-token after-class)
        extends? (match? after-name ::token/less)
        after-super (if extends?
                      (consume (advance after-name) ::token/identifier "Expect superclass name.")
                      after-name)
        superclass (when extends? (ast/variable (current-token (advance after-name))))
        after-lbrace (consume after-super ::token/lbrace "Expect '{' before class body.")]
    (loop [parser after-lbrace
           methods []]
      (if (or (check? parser ::token/rbrace) (is-at-end? parser))
        (let [after-rbrace (consume parser ::token/rbrace "Expect '}' after class body.")]
          (add-expr after-rbrace (ast/class-stmt name superclass methods)))
        (let [after-method (function parser "method")]
          (recur after-method (conj methods (:expr after-method))))))))

(defn- declaration [parser]
  (try 
    (cond
      (match? parser ::token/class) (class-declaration (advance parser))
      (match? parser ::token/fun) (function (advance parser) "function")
      (match? parser ::token/var) (var-declaration (advance parser))
      :else (statement parser))
    (catch clojure.lang.ExceptionInfo e
      (-> (ex-data e)
          (synchronize)
          (assoc :expr nil)))))

(defn- if-stmt [parser]
  (let [after-lparen (consume parser ::token/lparen
                              "Expect '(' after 'if'.")
        condition (expression after-lparen)
        after-rparen (consume condition ::token/rparen
                              "Expect ')' after if condition.")
        after-then (statement after-rparen)
        after-else (when (check? after-then ::token/else)
                     (statement (advance after-then)))]
    (add-expr (or after-else after-then)
              (ast/if-stmt (:expr condition)
                           (:expr after-then)
                           (:expr after-else)))))

(defn- while-stmt [parser]
  (let [after-lparen (consume parser ::token/lparen "Expect '(' after 'while'.")
        condition (expression after-lparen)
        after-rparen (consume condition ::token/rparen "Expect ')' after condition.")
        after-body (statement after-rparen)]
    (add-expr after-body
              (ast/while-stmt (:expr condition)
                              (:expr after-body)))))

(defn- for-stmt [parser]
  (let [after-lparen (consume parser ::token/lparen "Expect '(' after 'for'.")
        initializer (cond
                      (match? after-lparen ::token/semicolon) (advance (add-expr after-lparen nil))
                      (match? after-lparen ::token/var) (var-declaration (advance after-lparen))
                      :else (expression-stmt after-lparen))
        condition (if (match? initializer ::token/semicolon)
                    (add-literal initializer true)
                    (expression-stmt initializer))
        increment (if (match? condition ::token/rparen)
                    (add-expr condition nil)
                    (expression condition))
        after-rparen (consume increment ::token/rparen "Expect ')' after for clauses.")
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
       (let [after-statement (declaration parser)]
         (-> after-statement
             (update :statements conj (:expr after-statement))
             (assoc :expr nil)))))))
