(defproject cloxure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main cloxure.core
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :profiles {:dev {:resource-paths ["test/resources"]}}
  :plugins [[lein-eftest "0.5.9"]])
