# Cloxure

A Clojure implementation of the Lox programming language from the amazing book
[Crafting Interpreters](https://craftinginterpreters.com/) by Bob Nystrom.

This is a tree-walking implementation based on the reference *jlox* 
implementation written in Java.

I have tried to keep the code semantically as close to *jlox* as possible, but
adapted to Clojure's functional style. This means that a lot of components from
the interpreter don't use mutable state, but instead use a state passing
pattern using immutable data structures. The exception to this is the code for
storing scoped variables and object properties, which uses Clojure atoms and
hence are mutable references. Implementing an interpreter for an imperative
language such as Lox using purely functional code is possible, but it requires
a lot of extra work, specially around handling lexical closures. So I decided
to be pragmatic as Clojure itself and just use atoms for those parts.

Another difference from the original implementation, is that instead of using
the visitor pattern for tree walking, I use the more natural *multimethods*. In
other cases where polymorphism is needed, such as in *LoxCallable*, I have used
a *protocol* instead.

The code passes the entire test suite form Lox. The test files have been copy
pasted form the original [Crafting Interpreters
repository](https://github.com/munificent/craftinginterpreters).

The benchmarks have also been copied. As expected, the performance of *Cloxure*
is even worse than *jlox*. This is mostly due to the fact that on every
evaluation there is a new copy of the state being created. Even with Clojure's
persistent data structures, this is considerably slower than just using mutable
state. Performance is not the goal of this implementation, but rather learn
more about Clojure, interpreters and functional programming.

## How to run

To compile and run this project you need 
[Leiningen](https://leiningen.org/).

To run the REPL, just go for:

```
lein run
```

To run a specific file:

```
lein run program.lox
```

## Running the tests

To run the tests just run:

```
lein test
```

## Byte-code VM implementation

I also wrote another implementation of Lox using a byte-code VM Rust called
[Loxido](https://github.com/ceronman/loxido).

## Copyright note

Most of the test and benchmark files are copied from [Crafting Interpreters
repository](https://github.com/munificent/craftinginterpreters). The copyright
for these belongs to Bob Nystrom and are copied here because their license
allows it (MIT).

The source code for this implementation is copyright of Manuel Ceron.
And it's also licensed as MIT (see [LICENCE](LICENSE) for details.)