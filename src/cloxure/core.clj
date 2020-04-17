(ns cloxure.core
  (:require [cloxure.scanner :as scanner]))

(defn error [message code]
  (binding [*out* *err*]
    (println message)
    (System/exit code)))

(defn run [source]
  (let [scanner (scanner/scan source)]
    (println (scanner/scanner->str scanner))))

(defn run-file [filename]
  (try
    (let [source (slurp filename)]
      (run source))
    (catch java.io.FileNotFoundException e 
      (error (.getMessage e) 65))))

(defn run-prompt []
  (print "> ")
  (flush)
  (let [line (read-line)]
    (when line
      (run line) 
      (recur))))

(defn -main [& args]
  (cond
    (> (count args) 1) (error "Usage: cloxure [script]" 64)
    (= (count args) 1) (run-file (first args))
    :else (run-prompt)))
