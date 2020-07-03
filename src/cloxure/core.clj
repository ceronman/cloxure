(ns cloxure.core
  (:require [cloxure.scanner :as scanner])
  (:require [cloxure.token :as token])
  (:require [cloxure.parser :as parser])
  (:require [cloxure.resolver :as resolver])
  (:require [cloxure.interpreter :as interpreter]))

(defn- print-error [fmt & args]
  (binding [*out* *err*]
    (apply printf (str fmt "\n") args)
    (flush)))

(defn- exit-with-error [message code]
  (print-error message)
  (System/exit code))

(defn- format-error [error]
  (case (:type error)
    (:parser :resolver)
    (let [token (:token error)
          line (::token/line token)
          location (case (::token/type token)
                     ::token/eof "end"
                     (format "'%s'" (::token/lexeme token)))]
      (format "[line %d] Error at %s: %s" line location (:message error)))

    :scanner
    (format "[line %d] Error: %s" (:line error) (:message error))

    :runtime
    (format "%s\n[line %d]" (:message error) (::token/line (:token error)))

    (throw (ex-info "Unkown error" error))))

(defn- report-errors [state errors]
  (doseq [error errors]
    (print-error (format-error error)))
  (assoc state :errors []))

(defn- run [state source]
  (let [{tokens :tokens scanner-errors :errors} (scanner/scan source)
        {statements :statements parser-errors :errors} (parser/parse tokens)
        errors (concat scanner-errors parser-errors)]
    (if (seq errors)
      (report-errors state errors)
      (let [{:keys [errors locals]} (resolver/locals statements)]
        (if (seq errors)
          (report-errors state errors)
          (let [state (interpreter/interpret state statements locals)]
            (if (seq (:errors state))
              (report-errors state (:errors state))
              state)))))))

(defn run-file [filename]
  (try
    (let [source (slurp filename)
          state (interpreter/new-interpreter-state)]
      (run state source))
    (catch java.io.FileNotFoundException e 
      (exit-with-error (.getMessage e) 65))))

(defn- run-prompt []
  (loop [state (interpreter/new-interpreter-state)]
    (print "> ")
    (flush)
    (let [line (read-line)]
      (when line
        (recur (run state line))))))


(defn -main [& args]
  (cond
    (> (count args) 1) (exit-with-error "Usage: cloxure [script]" 64)
    (= (count args) 1) (run-file (first args))
    :else (run-prompt)))
