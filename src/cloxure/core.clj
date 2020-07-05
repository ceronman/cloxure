(ns cloxure.core
  (:require [cloxure.error :as error])
  (:require [cloxure.scanner :as scanner])
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

(defn- report-errors [state errors]
  (doseq [error errors]
    (print-error (error/fmt error)))
  (assoc state ::interpreter/errors []))

(defn- run [state source]
  (let [[tokens scanner-errors] (scanner/scan source)
        [statements parser-errors] (parser/parse tokens)
        errors (concat scanner-errors parser-errors)]
    (if (seq errors)
      (report-errors state errors)
      (let [[locals errors] (resolver/locals statements)]
        (if (seq errors)
          (report-errors state errors)
          (let [state (interpreter/interpret state statements locals)]
            (if (seq (::interpreter/errors state))
              (report-errors state (::interpreter/errors state))
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
