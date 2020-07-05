(ns cloxure.callable)

(defprotocol LoxCallable
  "Represent a callable object in Lox"
  (arity [this])
  (call [this state arguments])
  (to-string [this]))