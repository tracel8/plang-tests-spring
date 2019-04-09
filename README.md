# plang-tests-spring
Put Tests for Programming Languages here

## Installing HUnit

When you downloaded Haskell, if you chose the all-in-one installer ("Haskell Platform") you should already have the `cabal` tool, which you can use from the command line. Ensure Cabal is up to date with `cabal --version` and then run `cabal install HUnit`.

## Importing Tests

In order to run the tests, first clone this repository onto your local machine. Then, from the GHCi, load your assignment file first, then the respective testing file:

```
Prelude> :l Assignment1/a1c.hs
[1 of 1] Compiling A1c
...
*A1c> :add plang-tests-spring/a1_tests.hs
```

Make sure you use the `:add` command and not the `:l` command twice! If you use the `:l` command twice, the tests will not be able to import your code. 

## Running Tests

```
*Main> runTestTT tests
-> results
```