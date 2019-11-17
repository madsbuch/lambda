# Haskell Lambda Interpreter
[![Build Status](https://travis-ci.com/madsbuch/lambda.svg?branch=master)](https://travis-ci.com/madsbuch/lambda)

A Haskell implementation of an interpreter of the lambda
calculus.

```
$ stack build
$ stack exec lambda-exe
Lambda Calculus Interpreter!
λ: (\x.x) (\y.y)
(λy.y)
```
