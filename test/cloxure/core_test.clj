(ns cloxure.core-test
  (:require [clojure.test :refer [is testing]]
            [cloxure.core :refer [run-file]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def output-re #"// expect: ?(.*)")
(def error-re #"// (Error.*)")
(def error-line-re #"// \[(?:java )?line (\d+)\] (Error.*)")
(def runtime-error-re #"// expect runtime error: (.+)")

(defn- add-pattern [expected [n line]]
  (if-let [matched (re-find output-re line)]
    (update expected :out conj (nth matched 1))
    (if-let [matched (re-find error-line-re line)]
      (update expected :err conj
              (format "[line %s] %s" (nth matched 1) (nth matched 2)))
      (if-let [matched (re-find error-re line)]
        (update expected :err conj
                (format "[line %d] %s" (inc n) (nth matched 1)))
        (if-let [matched (re-find runtime-error-re line)]
          (update expected :err conj
                  (nth matched 1)
                  (format "[line %d]" (inc n)))
          expected)))))

(defn- parse-comments [file]
  (->> (slurp file)
       (str/split-lines)
       (map-indexed vector)
       (reduce add-pattern {:err [] :out []})))

(defn- stream->lines [stream]
  (let [as-str (.toString stream)]
    (if (str/blank? as-str) 
      []
      (str/split-lines as-str))))

(defn- run-test-file [file]
  (let [expected (parse-comments file)
        out-writer (java.io.StringWriter.)
        err-writer (java.io.StringWriter.)]
    (binding [*out* out-writer
              *err* err-writer]
      (run-file file))
    (let [out (stream->lines out-writer)
          err (stream->lines err-writer)]
      (testing (.getPath file)
        (is (= (:out expected) out) "stdout mismatch")
        (is (= (:err expected) err) "stderr mismatch")))))

(defn add-test
  "dynamically generated deftests."
  [name test-fn]
  (intern 'cloxure.core-test (with-meta (symbol name) {:test test-fn}) (fn [])))

(doseq [file (file-seq (io/file "test/resources"))]
  (when (not (.isDirectory file))
    (let [filename (.getName file)]
      (add-test (str "test_" (subs filename 0 (- (count filename) 4))) 
                #(run-test-file file)))))
