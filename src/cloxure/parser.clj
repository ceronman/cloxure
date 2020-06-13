(ns cloxure.parser
  (:require [cloxure.ast :as ast])
  (:require [cloxure.scanner :as scanner]))

;; Grammar
;; -----------------------------------------------------------------
;; 
;;    program        → declaration* EOF ;
;;    declaration    → classDecl
;;                     | funDecl
;;                     | varDecl
;;                     | statement ;
;;    classDecl      → "class" IDENTIFIER "{" function* "}" ;
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

(defn- add-error [parser message]
  (let [token (current-token parser)
        e {:line (:line token) :message message :loc (:text token)}]
    (update parser :errors conj e)))

(defn- error [parser message]
  (let [parser (add-error parser message)]
    (throw (ex-info "Parsing error" parser))))

(defn- consume [parser token-type message]
  (if (check? parser token-type)
    (advance parser)
    (error parser message)))

(defn- synchronize [parser]
  (cond (is-at-end? parser) parser
        (match? parser :semicolon) (advance parser)
        (match? parser 
                :class :fun :var :for :if :while :print :return) parser
        :else (recur (advance parser))))

(defn- add-expr [parser expr]
  (let [expr (assoc expr :loc (:current parser))]
    (assoc parser :expr expr)))

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
    :else (error parser "Expected expression")))

(defn- finish-call [after-lparen callee]
  (loop [parser after-lparen
         arguments []]
    (if (match? parser :rparen)
      (add-expr (advance parser) (ast/call callee (current-token parser) arguments))
      (let [after-arg (expression parser)
            after-arg (if (>= (count arguments) 255)
                        (add-error after-arg "Cannot have more than 255 arguments.")
                        after-arg)
            arguments (conj arguments (:expr after-arg))]
        (if (match? after-arg :comma)
          (recur (advance after-arg) arguments)
          (let [after-rparen (consume after-arg :rparen "Expect ')' after arguments.")]
            (add-expr after-rparen (ast/call callee (current-token after-arg) arguments))))))))

(defn- call [parser]
  (loop [parser (primary parser)]
    (cond 
      (match? parser :lparen)
      (recur (finish-call (advance parser) (:expr parser)))
      
      (match? parser :dot)
      (let [after-dot (advance parser)
            after-identifier (consume after-dot :identifier 
                                      "Expect property name after '.'.")]
        (recur (add-expr after-identifier 
                   (ast/get-expr (:expr after-identifier) (current-token after-dot)))))
      :else
      parser)))

(defn- unary [parser]
  (if (match? parser :bang :minus)
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
        (case (:type left-expr) 
          :variable (add-expr after-value (ast/assign (:name-token left-expr) 
                                                      (:expr after-value)))
          :get-expr (add-expr after-value (ast/set-expr (:object left-expr)
                                                        (:name-token left-expr)
                                                        (:expr after-value)))
          
          (error after-equals "Invalid assignment target.")))
      left)))

(defn- expression [parser]
  (assignment parser))

(defn- print-stmt [parser]
  (let [after-expression (expression parser)
        after-semicolon (consume after-expression :semicolon "Expect ';' after value.")]
    (add-expr after-semicolon (ast/print-stmt (:expr after-expression)))))

(defn- return-stmt [parser]
  (let [keyword (current-token parser)
        after-keyword (advance parser)
        after-value (if (match? after-keyword :semicolon)
                      (assoc after-keyword :expr (ast/literal nil))
                      (expression after-keyword))
        after-semicolon (consume after-value :semicolon "Expect ';' after return value.")]
    (add-expr after-semicolon (ast/return-stmt keyword (:expr after-value)))))

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
  ; TODO use condp?
  (cond 
    (match? parser :for) (for-stmt (advance parser))
    (match? parser :if) (if-stmt (advance parser))
    (match? parser :print) (print-stmt (advance parser))
    (match? parser :return) (return-stmt parser)
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
(defn- parameters [after-lparen]
  (if (match? after-lparen :rparen)
    [(advance after-lparen) []]
    (loop [parser after-lparen
           params []]
      (let [after-param (consume parser :identifier "Expect parameter name.")
            after-param (if (>= (count params) 255)
                          (add-error after-param "Cannot have more than 255 parameters.")
                          after-param)
            params (conj params (current-token parser))]
        (if (match? after-param :comma)
          (recur (advance after-param) params)
          [(consume after-param :rparen "Expect ')' after parameters.") params])))))

(defn- function [after-fun kind]
  (let [after-name (consume after-fun :identifier
                            (str "Expect " kind " name."))
        name (current-token after-fun)
        after-lparen (consume after-name :lparen
                              (str "Expect '(' after " kind " name."))
        [after-rparen params] (parameters after-lparen)
        after-lbrace (consume after-rparen :lbrace (str "Expect '{' before " kind " body."))
        after-block (block after-lbrace)
        body (:statements (:expr after-block))]
    (add-expr after-block (ast/fun-stmt name params body))))

(defn- class-declaration [after-class]
  (let [after-name (consume after-class :identifier "Expect class name.")
        name (current-token after-class)
        after-lbrace (consume after-name :lbrace "Expect '{' before class body.")]
    (loop [parser after-lbrace
           methods []]
      (if (or (check? parser :rbrace) (is-at-end? parser))
        (let [after-rbrace (consume parser :rbrace "Expect '}' after class body.")]
          (add-expr after-rbrace (ast/class-stmt name methods)))
        (let [after-method (function parser "method")]
          (recur after-method (conj methods (:expr after-method))))))))

(defn- declaration [parser]
  (cond 
    (match? parser :class) (class-declaration (advance parser))
    (match? parser :fun) (function (advance parser) "function")
    (match? parser :var) (var-declaration (advance parser))
    :else (statement parser)))

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
            (prn s)
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
(comment (test-parser "hello;"))
(comment (test-parser "hello();"))
(comment (test-parser "hello(1);"))
(comment (test-parser "hello(1+2);"))
(comment (test-parser "hello(1,2);"))
(comment (test-parser "hello(1,2, true);"))
(comment (test-parser "curry(1)(2)(3);"))
(comment (test-parser "curry(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                             21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                             41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
                             61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                             81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,
                             101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,
                             116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,
                             131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,
                             146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,
                             161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,
                             177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,
                             193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,
                             209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,
                             225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,
                             241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256);"))

(comment (test-parser "fun hello() { print 1; }"))
(comment (test-parser "fun hello(one, two, three) { print 1; }"))
(comment (test-parser "fun()"))
(comment (test-parser "fun hello() {}"))
(comment (test-parser "fun hello() { return; }"))
(comment (test-parser "fun hello() { return 1; }"))

(comment (test-parser "class"))
(comment (test-parser "class Blah"))
(comment (test-parser "class Blah {"))
(comment (test-parser "class Emptry {}"))

(comment (test-parser "
class Breakfast {
  cook() {
    print 1;
  }
}
"))


(comment (test-parser "
class Breakfast {
  cook() {
    print \"Eggs a-fryin'!\";
  }

  serve(who) {
    print \"Enjoy your breakfast, \" + who + \".\";
  }
}
"))

(comment (test-parser "foo.bar;"))
(comment (test-parser "egg.scramble(3).with(\"cheddar\");"))
(comment (test-parser "someObject.someProperty = value;"))
(comment (test-parser "egg.scramble(3).with(\"cheddar\").prop = true;"))
