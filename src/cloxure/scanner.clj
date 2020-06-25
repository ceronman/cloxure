(ns cloxure.scanner
  (:require [clojure.string :as string]))

(def keywords 
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
  {:source source
   :errors []
   :start 0
   :current 0
   :line 1
   :tokens []})

(defn- is-at-end? [scanner]
  (>= (:current scanner) (count (:source scanner))))

(defn- current-char [scanner]
  (nth (:source scanner) (:current scanner)))

(defn- current-text [scanner]
  (subs (:source scanner) (:start scanner) (:current scanner)))

(defn- advance [scanner]
  (update scanner :current inc))

(defn- token
  ([type text]
   (token type text nil 1))
  ([type text literal line]
   {:type type
    :text text
    :literal literal
    :line line}))

(defn- add-token
  ([scanner token-type]
   (add-token scanner token-type nil))
  ([scanner token-type literal]
   (update scanner :tokens conj (token token-type
                                       (current-text scanner)
                                       literal
                                       (:line scanner)))))

(defn- error [scanner message]
  (update scanner :errors conj {:type :scanner
                                :line (:line scanner) 
                                :message message}))

(defn- match [scanner expected]
  (and (not (is-at-end? scanner)) (= (current-char scanner) expected)))

(defn- peek-next [scanner]
  (let [next (inc (:current scanner))
        source (:source scanner)]
    (when (< next (count source))
      (nth source next))))

(defn- is-digit? [char]
  (and char
       (>= (int char) (int \0))
       (<= (int char) (int \9))))

(defn- is-alpha? [char]
  (when char
    (let [code (int char)]
      (or (and (>= code (int \a)) (<= code (int \z)))
          (and (>= code (int \A)) (<= code (int \Z)))
          (= char \_)))))

(defn- is-alphanumeric [char]
  (or (is-digit? char) (is-alpha? char)))

(defn- skip-comment [scanner]
  (if (or (is-at-end? scanner) (= (current-char scanner) \newline))
    scanner
    (recur (advance scanner))))

(defn- add-string [scanner]
  (cond 
    (is-at-end? scanner) (error scanner "Unterminated string")
    (= (current-char scanner) \") (let [advanced (advance scanner)]
                                    (add-token advanced :string (subs (:source advanced)
                                                                     (inc (:start advanced))
                                                                     (dec (:current advanced)))))
    :else (recur 
           (advance (if (= char \newline)
                      (update scanner :line inc)
                      scanner)))))

(defn- add-number [scanner]
  (if (or (is-at-end? scanner) (not (is-digit? (current-char scanner))))
    (if (and (match scanner \.) (is-digit? (peek-next scanner)))
      (recur (advance scanner))
      (add-token scanner :number (Double/parseDouble (current-text scanner))))
    (recur (advance scanner))))

(defn- add-identifier [scanner]
  (if (or (is-at-end? scanner) (not (is-alphanumeric (current-char scanner))))
    (let [text (current-text scanner)]
      (if-let [kw (keywords text)]
        (add-token scanner (keyword kw))
        (add-token scanner :identifier)))
    (recur (advance scanner))))

(defn- scan-token [scanner]
  (let [char (current-char scanner)
        scanner (advance scanner)]
    (case char
      \( (add-token scanner :lparen)
      \) (add-token scanner :rparen)
      \{ (add-token scanner :lbrace)
      \} (add-token scanner :rbrace)
      \, (add-token scanner :comma)
      \. (add-token scanner :dot)
      \- (add-token scanner :minus)
      \+ (add-token scanner :plus)
      \; (add-token scanner :semicolon)
      \* (add-token scanner :star)
      \! (if (match scanner \=)
           (add-token (advance scanner) :bang_equal)
           (add-token scanner :bang))
      \= (if (match scanner \=)
           (add-token (advance scanner) :equal_equal)
           (add-token scanner :equal))
      \< (if (match scanner \=)
           (add-token (advance scanner) :less_equal)
           (add-token scanner :less))
      \> (if (match scanner \=)
           (add-token (advance scanner) :greater_equal)
           (add-token scanner :greater))
      \/ (if (match scanner \/)
           (skip-comment (advance scanner))
           (add-token scanner :slash))
      (\return \space \tab) scanner
      \newline (update scanner :line inc)
      \" (add-string scanner)
      
      (cond 
        (is-digit? char) (add-number scanner)
        (is-alpha? char) (add-identifier scanner)
        :else (error scanner (str "Unexpected character."))))))

(defn- next-token [scanner]
  (assoc scanner :start (:current scanner)))

(defn scan [source]
  (loop [scanner (new-scanner source)]
    (if (is-at-end? scanner)
      (add-token scanner :eof)
      (recur (-> scanner
                 (scan-token)
                 (next-token))))))

(defn scanner->str [scanner]
  (let [errors (:errors scanner)]
    (if (seq errors)
      (string/join "\n" (map #(format "ERROR Line %s: %s" (:line %) (:message %)) errors))
      (map #(str (:type %)
                 (cond
                   (:literal %) (str "=" (:literal %))
                   (= (:type %) :identifier) (str "=" (:text %))
                   :else "")) (:tokens scanner)))))

(defn- scanner-test [source]
  (println (scanner->str (scan source))))

(comment (scanner-test "a"))
(comment (scanner-test "()"))
(comment (scanner-test "()&%"))
(comment (scanner-test "()!+="))
(comment (scanner-test "()!="))
(comment (scanner-test "()!=//This is a comment"))
(comment (scanner-test "()!=//This is a comment[]()"))
(comment (scanner-test "//This is a comment\n{}()"))
(comment (scanner-test "//This is a comment"))
(comment (scanner-test "+  -  (  )"))
(comment (scanner-test "\n//comment\n\n&"))
(comment (scanner-test "\"hello\" + \"world\""))
(comment (scanner-test "123 + 125.12"))
(comment (scanner-test "123 + 125.11. .5"))
(comment (scanner-test "hello.world()"))
(comment (scanner-test "hello.world"))
(comment (scanner-test "\"hello.world\""))
(comment (scanner-test "a + b + 4"))
(comment (scanner-test "a or b + c"))
(comment (scanner-test "if (a + b) { x = true }"))
(comment (scanner-test "\"t\""))
(comment (scanner-test "\"Unterminated string"))
(comment (scanner-test "\""))