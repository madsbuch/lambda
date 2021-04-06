# Haskell Lambda Interpreter
[![Build Status](https://travis-ci.com/madsbuch/lambda.svg?branch=master)](https://travis-ci.com/madsbuch/lambda)

A Haskell implementation of an interpreter of the lambda
calculus.

# Quick Start
To start a REPL, pull this repository, have Haskell stack
installed and do the following:

```
$ stack build
$ stack test
$ stack exec lambda-exe
Lambda Calculus Interpreter!
λ: (\x.x) (\y.y y)
(λy.(y) (y))
```

# Running Tests
A basic test suite is implemented and can run by
the usual `stack test`. The tests support a correct
implementation. However, they do not guarantee it.
Some day, when time is, it might be interesting to
implement the evaluator in a proof assistant.