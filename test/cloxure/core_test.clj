(ns cloxure.core-test
  (:require [clojure.test :refer :all]
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
                  (format "[line %d]" n))
          expected)))))

(defn- parse-comments [file]
  (->> (slurp file)
       (str/split-lines)
       (map-indexed vector)
       (reduce add-pattern {:err [] :out []})))

(defn- check-output [expected result]
  (doseq [[line-expected line] (map vector expected result)]
    (is (= line-expected line) "Output mismatch")))

(defn- run-test-file [file]
  (let [expected (parse-comments file)
        out-writer (java.io.StringWriter.)
        err-writer (java.io.StringWriter.)]
    (binding [*out* out-writer
              *err* err-writer]
      (run-file file))
    (let [out (str/split-lines (.toString out-writer))
          err (str/split-lines (.toString err-writer))]
      (testing (.getPath file)
       (check-output (:out expected) out)
       (check-output (:err expected) err)))))

;; (def test-file
;;   (run-test-file (io/file "test/resources/if/class_in_then.lox")))

(deftest all
  (doseq [file (file-seq (io/file "test/resources"))]
    (when (not (.isDirectory file))
      (run-test-file file))))


