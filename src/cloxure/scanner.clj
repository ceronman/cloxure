(ns cloxure.scanner
  "Scanner/Tokenizer for the Lox programming language."
  (:require
   [cloxure.token :as token]
   [cloxure.error :refer [line-error]]))

;; -----------------------------------------------------------------
;; Lexical Grammar:
;; -----------------------------------------------------------------
;; NUMBER         → DIGIT+ ("." DIGIT+) ? ;
;; STRING         → "\"" <any char except "\"" >* "\"" ;
;; IDENTIFIER     → ALPHA (ALPHA | DIGIT) * ;
;; ALPHA          → "a" ... "z" | "A" ... "Z" | "_" ;
;; DIGIT          → "0" ... "9" ;
;; -----------------------------------------------------------------

(def reserved-words
  "List of reserved words of the Lox language."
  #{"and"
    "class"
    "else"
    "false"
    "for"
    "fun"
    "if"
    "nil"
    "or"
    "print"
    "return"
    "super"
    "this"
    "true"
    "var"
    "while"})

(defn- new-scanner [source]
  {::source source
   ::errors []
   ::start 0
   ::current 0
   ::line 1
   ::tokens []})

(defn- at-end? [scanner]
  (>= (::current scanner) (count (::source scanner))))

(defn- current-char [scanner]
  (nth (::source scanner) (::current scanner)))

(defn- current-lexeme [scanner]
  (subs (::source scanner) (::start scanner) (::current scanner)))

(defn- advance [scanner]
  (update scanner ::current inc))

(defn- add-token
  ([scanner token-type]
   (add-token scanner token-type nil))
  ([scanner token-type literal]
   (update scanner ::tokens conj (token/token token-type
                                             (current-lexeme scanner)
                                             literal
                                             (::line scanner)))))

(defn- add-error [scanner message]
  (update scanner ::errors conj (line-error (::line scanner) message)))

(defn- match [scanner expected]
  (and (not (at-end? scanner)) (= (current-char scanner) expected)))

(defn- peek-next [scanner]
  (let [next (inc (::current scanner))
        source (::source scanner)]
    (when (< next (count source))
      (nth source next))))

(defn- digit? [character]
  (and character
       (>= (int character) (int \0))
       (<= (int character) (int \9))))

(defn- alpha? [char]
  (when char
    (let [code (int char)]
      (or (and (>= code (int \a)) (<= code (int \z)))
          (and (>= code (int \A)) (<= code (int \Z)))
          (= char \_)))))

(defn- alphanum? [char]
  (or (digit? char) (alpha? char)))

(defn- skip-comment [scanner]
  (if (or (at-end? scanner) (= (current-char scanner) \newline))
    scanner
    (recur (advance scanner))))

(defn- add-string [scanner]
  (cond
    (at-end? scanner) (add-error scanner "Unterminated string.")

    (= (current-char scanner) \")  (let [advanced (advance scanner)]
                                     (add-token advanced
                                                ::token/string
                                                (subs (::source advanced)
                                                      (inc (::start advanced))
                                                      (dec (::current advanced)))))

    :else (recur (advance (if (= (current-char scanner) \newline)
                            (update scanner ::line inc)
                            scanner)))))

(defn- add-number [scanner]
  (if (or (at-end? scanner) (not (digit? (current-char scanner))))
    (if (and (match scanner \.) (digit? (peek-next scanner)))
      (recur (advance scanner))
      (add-token scanner ::token/number (Double/parseDouble (current-lexeme scanner))))
    (recur (advance scanner))))

(defn- add-identifier [scanner]
  (if (or (at-end? scanner) (not (alphanum? (current-char scanner))))
    (let [text (current-lexeme scanner)]
      (if (reserved-words text)
        (add-token scanner (token/from-keyword text))
        (add-token scanner ::token/identifier)))
    (recur (advance scanner))))

(defn- scan-token [scanner]
  (let [char (current-char scanner)
        scanner (advance scanner)]
    (case char
      \( (add-token scanner ::token/lparen)
      \) (add-token scanner ::token/rparen)
      \{ (add-token scanner ::token/lbrace)
      \} (add-token scanner ::token/rbrace)
      \, (add-token scanner ::token/comma)
      \. (add-token scanner ::token/dot)
      \- (add-token scanner ::token/minus)
      \+ (add-token scanner ::token/plus)
      \; (add-token scanner ::token/semicolon)
      \* (add-token scanner ::token/star)
      \! (if (match scanner \=)
           (add-token (advance scanner) ::token/bang_equal)
           (add-token scanner ::token/bang))
      \= (if (match scanner \=)
           (add-token (advance scanner) ::token/equal_equal)
           (add-token scanner ::token/equal))
      \< (if (match scanner \=)
           (add-token (advance scanner) ::token/less_equal)
           (add-token scanner ::token/less))
      \> (if (match scanner \=)
           (add-token (advance scanner) ::token/greater_equal)
           (add-token scanner ::token/greater))
      \/ (if (match scanner \/)
           (skip-comment (advance scanner))
           (add-token scanner ::token/slash))
      (\return \space \tab) scanner
      \newline (update scanner ::line inc)
      \" (add-string scanner)

      (cond
        (digit? char) (add-number scanner)
        (alpha? char) (add-identifier scanner)
        :else (add-error scanner (str "Unexpected character."))))))

(defn- next-token [scanner]
  (assoc scanner ::start (::current scanner)))

(defn scan
  "Generates a list of tokens from a Lox source code.
   It accepts a string containing Lox source code and it returns a vector 
   of [tokens, errors], where tokens is a list of maps representing the tokens
   (see cloxure.token) and errors are a list of errors if any occoured."
  [source]
  (loop [scanner (new-scanner source)]
    (if (at-end? scanner)
      (let [scanner (add-token scanner ::token/eof)]
        [(::tokens scanner) (::errors scanner)])
      (recur (-> scanner
                 (scan-token)
                 (next-token))))))
