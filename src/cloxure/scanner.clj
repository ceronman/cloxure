(ns cloxure.scanner)

(defn new-scanner [source]
  {:source source
   :start 0
   :current 0
   :line 0
   :tokens []})

(defn is-at-end [scanner]
  (>= (:current scanner) (count (:source scanner))))

(defn current-char [scanner]
  (nth (:source scanner) (:current scanner)))

(defn advance [scanner]
  (update scanner :current inc))

(defn add-token
  ([scanner token-type]
   (add-token scanner token-type nil))
  ([scanner token-type literal]
   (let [text (subs (:source scanner) (:start scanner) (:current scanner))
         token {:type token-type
                :text text
                :literal literal
                :line (:line scanner)}]
     (update scanner :tokens conj token))))

(defn scan-token [scanner]
  (case (current-char scanner)
    \( (add-token (advance scanner) :lparen)
    \) (add-token (advance scanner) :rparen)
    \{ (add-token (advance scanner) :lbrace)
    \} (add-token (advance scanner) :rbrace)
    \, (add-token (advance scanner) :comma)
    \. (add-token (advance scanner) :dot)
    \- (add-token (advance scanner) :minus)
    \+ (add-token (advance scanner) :plus)
    \; (add-token (advance scanner) :semicolon)
    \* (add-token (advance scanner) :star)))

(defn next-token [scanner]
  (assoc scanner :start (:current scanner)))

(defn scan-tokens [source]
  (loop [scanner (new-scanner source)]
    (if (is-at-end scanner)
      (add-token scanner :eof)
      (recur (-> scanner
                 (scan-token)
                 (next-token))))))