(ns cloxure.core
  (:require [cloxure.scanner :as scanner])
  (:require [cloxure.parser :as parser])
  (:require [cloxure.interpreter :as interpreter]))

(defn error [message code]
  (binding [*out* *err*]
    (println message)
    (System/exit code)))

(defn run [source env]
  (let [{errors :errors tokens :tokens} (scanner/scan source)]
    (if (seq errors)
      (do (println "ERROR" (prn-str errors)) env)
      (let [{errors :errors statements :statements} (parser/parse tokens)]
        (if (seq errors) 
          (do (println "ERROR" (prn-str errors)) env)
          (interpreter/interpret statements env))))))

(defn run-file [filename]
  (try
    (let [source (slurp filename)]
      (run source {}))
    (catch java.io.FileNotFoundException e 
      (error (.getMessage e) 65))))

(defn- run-prompt []
  (loop [env {}]
    (print "> ")
    (flush)
    (let [line (read-line)]
      (when line
        (recur (run line env))))))


(defn -main [& args]
  (cond
    (> (count args) 1) (error "Usage: cloxure [script]" 64)
    (= (count args) 1) (run-file (first args))
    :else (run-prompt)))
