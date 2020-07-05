(ns cloxure.environment
  "Scopes and enviroment for the Lox interpreter."
  (:require
   [cloxure.token :as token]
   [cloxure.error :refer [runtime-error]]))

(defn- ancestor [env distance]
  (if (zero? distance)
    env
    (recur (:enclosing @env) (dec distance))))

(defn get-name-at [env distance name]
  (let [env (ancestor env distance)]
    (get @env name)))

(defn get-name [env name-token]
  (let [name (::token/lexeme name-token)]
    (if (contains? @env name)
      (get @env name)
      (runtime-error name-token (format "Undefined variable '%s'." name)))))

(defn declare-name! [env name value]
  (swap! env assoc name value)
  env)

(defn assign-name! [env name-token value]
  (let [name (::token/lexeme name-token)]
    (if (contains? @env name)
      (swap! env assoc name value)
      (runtime-error name-token (format "Undefined variable '%s'." name)))))

(defn assign-name-at! [env name-token distance value]
  (let [env (ancestor env distance)]
    (assign-name! env name-token value)))

(defn push-scope [env]
  (atom {:enclosing env}))

(defn pop-scope [env]
  (:enclosing (deref env)))