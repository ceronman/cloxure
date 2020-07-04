(ns cloxure.error
  "Representation and formating of Lox errors."
  (:require [cloxure.token :as token]))

(defmulti fmt
  "Formats an error as string"
  ::type)

(defn line-error [line message]
  {::type ::line ::line line ::message message})

(defmethod fmt ::line [error]
  (format "[line %d] Error: %s" (::line error) (::message error)))

(defn token-error [token message]
  {::type ::token ::token token ::message message})

(defmethod fmt ::token [error]
  (let [token (::token error)
        line (::token/line token)
        location (case (::token/type token)
                   ::token/eof "end"
                   (format "'%s'" (::token/lexeme token)))]
    (format "[line %d] Error at %s: %s" line location (::message error))))

(defn runtime-error [token message]
  {::type ::runtime ::token token ::message message })

(defmethod fmt ::runtime [error]
  (format "%s\n[line %d]" (::message error) (::token/line (::token error))))
