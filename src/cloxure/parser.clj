(ns cloxure.parser
  "Parser for the Lox programming language."
  (:require
   [cloxure.token :as token]
   [cloxure.ast :as ast]
   [cloxure.error :refer [token-error]]))

;; -----------------------------------------------------------------
;; Grammar:
;; -----------------------------------------------------------------
;; declaration    → classDecl
;;                   | funDecl
;;                   | varDecl
;;                   | statement ;
;; 
;; classDecl      → 'class' IDENTIFIER ('<' IDENTIFIER) ?
;;                   '{' function* '}' ;
;; funDecl        → 'fun' function ;
;; varDecl        → 'var' IDENTIFIER ('=' expression) ? ';' ;
;; -----------------------------------------------------------------
;; statement      → exprStmt
;;                   | forStmt
;;                   | ifStmt
;;                   | printStmt
;;                   | returnStmt
;;                   | whileStmt
;;                   | block ;

;; exprStmt       → expression ';' ;
;; forStmt        → 'for' '(' (varDecl | exprStmt | ';')
;;                  expression? ';'
;;                  expression? ')' statement ;
;; ifStmt         → 'if' '(' expression ')' statement ('else' statement) ? ;
;; printStmt      → 'print' expression ';' ;
;; returnStmt     → 'return' expression? ';' ;
;; whileStmt      → 'while' '(' expression ')' statement ;
;; block          → '{' declaration* '}' ;
;; -----------------------------------------------------------------
;; expression     → assignment ;

;; assignment     → (call '.') ? IDENTIFIER '=' assignment
;;                  | logic_or;

;; logic_or       → logic_and ('or' logic_and) * ;
;; logic_and      → equality ('and' equality) * ;
;; equality       → comparison (('!=' | '==') comparison) * ;
;; comparison     → addition (('>' | '>=' | '<' | '<=') addition) * ;
;; addition       → multiplication (('-' | '+') multiplication) * ;
;; multiplication → unary (('/' | '*') unary) * ;

;; unary          → ('!' | '-') unary | call ;
;; call           → primary ('(' arguments? ')' | '.' IDENTIFIER) * ;
;; primary        → 'true' | 'false' | 'nil' | 'this'
;;                   | NUMBER | STRING | IDENTIFIER | '(' expression ')'
;;                   | 'super' '.' IDENTIFIER ;
;; -----------------------------------------------------------------
;; function       → IDENTIFIER '(' parameters? ')' block ;
;; parameters     → IDENTIFIER (',' IDENTIFIER) * ;
;; arguments      → expression (',' expression) * ;
;; -----------------------------------------------------------------                 

(defn- new-parser [tokens]
  {:tokens tokens
   :errors []
   :statements []
   :current 0
   :expr nil})

(defn- current-token [parser]
  (nth (:tokens parser) (:current parser)))

(defn- prev-token [parser]
  (nth (:tokens parser) (dec (:current parser))))

(defn- at-end? [parser]
  (= (::token/type (current-token parser)) ::token/eof))

(defn- advance [parser]
  (update parser :current inc))

(defn- check? [parser & token-types]
  (and (not (at-end? parser))
       (some #(= (::token/type (current-token parser)) %) token-types)))

(defn- add-error [parser message]
  (update parser :errors conj (token-error (current-token parser) message)))

(defn- error-and-throw [parser message]
  (let [parser (add-error parser message)]
    (throw (ex-info "Parsing error" parser))))

(defn- consume [parser token-type message]
  (if (check? parser token-type)
    (advance parser)
    (error-and-throw parser message)))

(defn- synchronize [parser]
  (cond (at-end? parser) parser
        (check? parser ::token/semicolon) (advance parser)
        (check? parser
                ::token/class ::token/fun ::token/var ::token/for ::token/if
                ::token/while ::token/print ::token/return) parser
        :else (recur (advance parser))))

(defn- add-expr [parser expr]
  (let [expr (assoc expr ::ast/location (:current parser))]
    (assoc parser :expr expr)))

(defn- add-literal [parser value]
  (advance (add-expr parser (ast/literal value))))

; Predeclare expression as it will be used by primary. 
(declare expression)

(defn- primary
  "Parses a `primary` expression.

   primary → 'true' | 'false' | 'nil' | 'this'
             | NUMBER | STRING | IDENTIFIER | '(' expression ')'
             | 'super' '.' IDENTIFIER ;"
  [parser]
  (cond
    (check? parser ::token/true) (add-literal parser true)
    (check? parser ::token/false) (add-literal parser false)
    (check? parser ::token/nil) (add-literal parser nil)

    (check? parser ::token/this)
    (add-expr (advance parser) (ast/this-expr (current-token parser)))

    (check? parser ::token/number ::token/string)
    (add-literal parser (::token/literal (current-token parser)))

    (check? parser ::token/identifier)
    (add-expr (advance parser) (ast/variable (current-token parser)))

    (check? parser ::token/lparen)
    (let [middle (expression (advance parser))]
      (add-expr (consume middle ::token/rparen "Expect ')' after expression.")
                (ast/group (:expr middle))))

    (check? parser ::token/super)
    (let [after-dot (consume (advance parser) ::token/dot
                             "Expect '.' after 'super'.")
          after-idt (consume after-dot ::token/identifier
                             "Expect superclass method name.")]
      (add-expr after-idt (ast/super (current-token parser) (current-token after-dot))))

    :else
    (error-and-throw parser "Expect expression.")))

(defn- arguments
  "Parses an `arguments` element.
   
  arguments → expression (''expression) * ;"
  [after-lparen]
  (if (check? after-lparen ::token/rparen)
    (assoc (advance after-lparen) :expr [])
    (loop [parser after-lparen
           args []]
      (let [after-arg (expression parser)
            after-arg (if (>= (count args) 255)
                        (advance 
                         (add-error parser "Cannot have more than 255 arguments."))
                        after-arg)
            args (conj args (:expr after-arg))]
        (if (check? after-arg ::token/comma)
          (recur (advance after-arg) args)
          (assoc (consume after-arg ::token/rparen
                          "Expect ')' after arguments.")
                 :expr args))))))

(defn- call
  "Parses a `call` expression.
   
   call      → primary ('(' arguments? ')' | '.' IDENTIFIER) * ;
   arguments → expression (''expression) * ;"
  [parser]
  (loop [parser (primary parser)]
    (cond
      (check? parser ::token/lparen)
      (let [after-args (arguments (advance parser))]
        (recur (add-expr after-args (ast/call (:expr parser) 
                                              (prev-token after-args) 
                                              (:expr after-args)))))

      (check? parser ::token/dot)
      (let [after-dot (advance parser)
            after-identifier (consume after-dot ::token/identifier
                                      "Expect property name after '.'.")]
        (recur (add-expr after-identifier
                         (ast/get-expr (:expr parser) (current-token after-dot)))))
      :else
      parser)))

(defn- unary 
  "Parses a `unary` operation expression.
   
   unary → ('!' | '-') unary | call ;"
  [parser]
  (if (check? parser ::token/bang ::token/minus)
    (let [operator (current-token parser)
          right (unary (advance parser))]
      (add-expr right (ast/unary operator (:expr right))))
    (call parser)))

(defn- binary-operation [parser base-expr & operators]
  (loop [left (base-expr parser)]
    (if (apply check? left operators)
      (let [op (current-token left)
            right (base-expr (advance left))]
        (recur (add-expr right (ast/binary (:expr left) op (:expr right)))))
      left)))

(defn- multiplication 
  "Parses a `multiplication` expression.
   
   multiplication → unary (('/' | '*') unary) * ;"
  [parser]
  (binary-operation parser unary ::token/slash ::token/star))

(defn- addition 
  "Parses an `addition` expression.
   
   addition → multiplication (('-' | '+') multiplication) * ;"
  [parser]
  (binary-operation parser multiplication ::token/minus ::token/plus))

(defn- comparison 
  "Parses a `comparison` expression.
   
   comparison → addition (('>' | '>=' | '<' | '<=') addition) * ;"
  [parser]
  (binary-operation parser addition 
                    ::token/greater ::token/greater_equal 
                    ::token/less ::token/less_equal))

(defn- equality 
  "Parses an `equality` expression.
   
   equality → comparison (('!=' | '==') comparison) * ;"
  [parser]
  (binary-operation parser comparison ::token/bang_equal ::token/equal_equal))

(defn- logical-operation [parser base-expr & operators]
  (loop [left (base-expr parser)]
    (if (apply check? left operators)
      (let [op (current-token left)
            right (base-expr (advance left))]
        (recur (add-expr right (ast/logical (:expr left) op (:expr right)))))
      left)))

(defn- logical-and 
  "Parses a `logic_and` expression.
   
   logic_and → equality ('and' equality) * ;"
  [parser]
  (logical-operation parser equality ::token/and))

(defn- logical-or 
  "Parses a `logic_or` expression.
   
   logic_or → logic_and ('or' logic_and) * ;"
  [parser]
  (logical-operation parser logical-and ::token/or))

(defn- assignment 
  "Parses an `assignment` expression.
   
   assignment → (call '.') ? IDENTIFIER '=' assignment
                | logic_or;"
  [parser]
  (let [left (logical-or parser)
        left-expr (:expr left)]
    (if (check? left ::token/equal)
      (let [after-equals (advance left)
            after-value (assignment after-equals)]
        (case (::ast/type left-expr)
          ::ast/variable
          (add-expr after-value (ast/assign (::ast/name-token left-expr)
                                            (:expr after-value)))

          ::ast/get-expr
          (add-expr after-value (ast/set-expr (::ast/object left-expr)
                                              (::ast/name-token left-expr)
                                              (:expr after-value)))

          (error-and-throw left "Invalid assignment target.")))
      left)))

(defn- expression
  "Parses an `expression` element.
   
   expression → assignment ;"
  [parser]
  (assignment parser))

; Predeclare declaration as it will be used by previous rules 
(declare declaration)
(declare var-declaration)
(declare statement)

(defn- block 
  "Parses a `block` statement.
   
   block → '{' declaration* '}' ;
   "
  [after-lbrace]
  (loop [parser after-lbrace
         statements []]
    (if-not (or (at-end? parser) (check? parser ::token/rbrace))
      (let [after-declaration (declaration parser)]
        (recur after-declaration (conj statements (:expr after-declaration))))
      (add-expr (consume parser ::token/rbrace "Expect '}' after block.")
                (ast/block statements)))))

(defn- while-stmt 
  "Parses a `whileStmt` statement.
   
   whileStmt → 'while' '(' expression ')' statement ;"
  [after-while]
  (let [after-lparen (consume after-while ::token/lparen "Expect '(' after 'while'.")
        condition (expression after-lparen)
        after-rparen (consume condition ::token/rparen "Expect ')' after condition.")
        after-body (statement after-rparen)]
    (add-expr after-body (ast/while-stmt (:expr condition)
                                         (:expr after-body)))))

(defn- return-stmt 
  "Parses a `returnStmt` statement.
   
  returnStmt → 'return' expression? ';' ;"
  [parser]
  (let [keyword (current-token parser)
        after-keyword (advance parser)
        after-value (if (check? after-keyword ::token/semicolon)
                      (assoc after-keyword :expr nil)
                      (expression after-keyword))
        after-semicolon (consume after-value ::token/semicolon 
                                 "Expect ';' after return value.")]
    (add-expr after-semicolon (ast/return-stmt keyword (:expr after-value)))))

(defn- print-stmt 
  "Parses a `printStmt` statement.
   
   printStmt → 'print' expression ';' ;"
  [after-print]
  (let [after-expression (expression after-print)
        after-semicolon (consume after-expression ::token/semicolon 
                                 "Expect ';' after value.")]
    (add-expr after-semicolon (ast/print-stmt (:expr after-expression)))))

(defn- if-stmt 
  "Parses an `ifStmt` statement.
   
   ifStmt → 'if' '(' expression ')' statement ('else' statement) ? ;"
  [after-if]
  (let [after-lparen (consume after-if ::token/lparen
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
(defn- expression-stmt
  "Parses an `exprStmt` element.
   
   exprStmt → expression ';' ;"
  [parser]
  (let [after-expression (expression parser)
        after-semicolon (consume after-expression ::token/semicolon
                                 "Expect ';' after expression.")]
    (add-expr after-semicolon (:expr after-semicolon))))

(defn- for-stmt 
  "Parses a `forStmt` statement.
   It's just syntatic sugar for a `whileStmt`, there are no ast nodes for `forStmst`
   
   forStmt → 'for' '(' (varDecl | exprStmt | ';')
                  expression? ';'
                  expression? ')' statement ;"
  [after-for]
  (let [after-lparen (consume after-for ::token/lparen "Expect '(' after 'for'.")
        initializer (cond
                      (check? after-lparen ::token/semicolon)
                      (advance (add-expr after-lparen nil))

                      (check? after-lparen ::token/var)
                      (var-declaration (advance after-lparen))

                      :else
                      (expression-stmt after-lparen))
        condition (if (check? initializer ::token/semicolon)
                    (add-literal initializer true)
                    (expression-stmt initializer))
        increment (if (check? condition ::token/rparen)
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

(defn- statement 
  "Parses a `statement` element.
   
   statement → exprStmt
               | forStmt
               | ifStmt
               | printStmt
               | returnStmt
               | whileStmt
               | block ;"
  [parser]
  (cond
    (check? parser ::token/for) (for-stmt (advance parser))
    (check? parser ::token/if) (if-stmt (advance parser))
    (check? parser ::token/print) (print-stmt (advance parser))
    (check? parser ::token/return) (return-stmt parser)
    (check? parser ::token/while) (while-stmt (advance parser))
    (check? parser ::token/lbrace) (block (advance parser))
    :else (expression-stmt parser)))

(defn- var-declaration 
  "Parses a `varDecl` declaration.
   
   varDecl → 'var' IDENTIFIER ('=' expression) ? ';' ;"
  [after-var]
  (let [after-identifier (consume after-var ::token/identifier 
                                  "Expect variable name.")
        initializer? (check? after-identifier ::token/equal)
        before-semicolon (if initializer? 
                           (expression (advance after-identifier)) 
                           after-identifier)
        after-semicolon (consume before-semicolon ::token/semicolon 
                                 "Expect ';' after expression.")]
    (add-expr after-semicolon
              (ast/var-stmt (current-token after-var)
                            (if initializer? (:expr before-semicolon) nil)))))

(defn- parameters 
  "Parses a `parameters` element.
   
   parameters → IDENTIFIER (''IDENTIFIER) * ;"
  [after-lparen]
  (if (check? after-lparen ::token/rparen)
    (assoc (advance after-lparen) :expr [])
    (loop [parser after-lparen
           params []]
      (let [after-param (consume parser ::token/identifier "Expect parameter name.")
            after-param (if (>= (count params) 255)
                          (advance (add-error parser 
                                              "Cannot have more than 255 parameters."))
                          after-param)
            params (conj params (current-token parser))]
        (if (check? after-param ::token/comma)
          (recur (advance after-param) params)
          (assoc (consume after-param ::token/rparen 
                          "Expect ')' after parameters.") 
                 :expr params))))))

(defn- function
  "Parses a `function` element.
   
   function → IDENTIFIER '(' parameters? ')' block ;"
  [after-fun kind]
  (let [after-name (consume after-fun ::token/identifier
                            (str "Expect " kind " name."))
        name-token (current-token after-fun)
        after-lparen (consume after-name ::token/lparen
                              (str "Expect '(' after " kind " name."))
        after-params (parameters after-lparen)
        after-lbrace (consume after-params ::token/lbrace 
                              (str "Expect '{' before " kind " body."))
        after-block (block after-lbrace)
        body (::ast/statements (:expr after-block))]
    (add-expr after-block (ast/fun-stmt name-token (:expr after-params) body))))

(defn- fun-declaration 
  "Parses a `funDecl` declaration.
   
   funDecl → 'fun' function ;"
  [after-fun]
  (function after-fun "function"))

(defn- class-declaration 
  "Parses a `classDecl` declaration.
   
   classDecl → 'class' IDENTIFIER ('<' IDENTIFIER) ?
               '{' function* '}' ;"
  [after-class]
  (let [after-name (consume after-class ::token/identifier "Expect class name.")
        name (current-token after-class)
        extends? (check? after-name ::token/less)
        after-super (if extends?
                      (consume (advance after-name) ::token/identifier "Expect superclass name.")
                      after-name)
        superclass (when extends? (ast/variable (current-token (advance after-name))))
        after-lbrace (consume after-super ::token/lbrace "Expect '{' before class body.")]
    (loop [parser after-lbrace
           methods []]
      (if (or (check? parser ::token/rbrace) (at-end? parser))
        (let [after-rbrace (consume parser ::token/rbrace "Expect '}' after class body.")]
          (add-expr after-rbrace (ast/class-stmt name superclass methods)))
        (let [after-method (function parser "method")]
          (recur after-method (conj methods (:expr after-method))))))))

(defn- declaration 
  "Parses a top level `declaration`.
   
   declaration → classDecl
                  | funDecl
                  | varDecl
                  | statement ;"
  [parser]
  (try
    (cond
      (check? parser ::token/class) (class-declaration (advance parser))
      (check? parser ::token/fun) (fun-declaration (advance parser))
      (check? parser ::token/var) (var-declaration (advance parser))
      :else (statement parser))
    (catch clojure.lang.ExceptionInfo e
      (-> (ex-data e)
          (synchronize)
          (assoc :expr nil)))))

(defn parse 
  "Parses a Lox program from a list of tokens.
   See cloxure.scanner for how to generate tokens from source code.
   Generates an Abstract Syntax Tree (AST) (see cloxure.ast) for the source code.
   Return a vector of [statements, errors] where the statements is a vector
   of statement/declaration nodes."
  [tokens]
  (loop [parser (new-parser tokens)]
    (if (at-end? parser)
      [(:statements parser) (:errors parser)]
      (recur
       (let [after-statement (declaration parser)]
         (-> after-statement
             (update :statements conj (:expr after-statement))
             (assoc :expr nil)))))))
