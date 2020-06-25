(ns cloxure.core
  (:require [cloxure.scanner :as scanner])
  (:require [cloxure.parser :as parser])
  (:require [cloxure.resolver :as resolver])
  (:require [cloxure.interpreter :as interpreter]))

(defn- exit-with-error [message code]
  (binding [*out* *err*]
    (println message)
    (System/exit code)))

(defn- report-errors [state errors]
  (println "ERROR" (prn-str errors))
  (assoc state :errors []))

(defn- run [state source]
  (let [{:keys [errors tokens]} (scanner/scan source)]
    (if (seq errors)
      (report-errors state errors)
      (let [{:keys [errors statements]} (parser/parse tokens)]
        (if (seq errors) 
          (report-errors state errors)
          (let [{:keys [errors locals]} (resolver/locals statements)]
            (if (seq errors)
              (report-errors state errors)
              (let [state (interpreter/interpret state statements locals)]
                (if (seq (:errors state))
                  (report-errors state (:errors state))
                  state)))))))))

(defn- run-file [filename]
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
