(ns cloxure.token)

(defn token
  ([type text]
   (token type text nil 1))
  ([type text literal line]
   {::type type
    ::lexeme text
    ::literal literal
    ::line line}))

(defn from-keyword [kw]
  (keyword "cloxure.token" kw))