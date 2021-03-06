# change

A lab exercise for the module CI285 Introduction to Functional Programming at
the University of Brighton.

This program "makes change". That is, it works out which combination
of coins is required to make a given amount of money. While the
program is running it repeatedly asks the user for a number until the
user enters an empty line:

```
$ git clone https://github.com/jimburton/change
$ cd change
$ cabal run change
Preprocessing executable 'change' for change-0.1.0.0...
Running change...
Enter a number and I'll count out the change
127
[Pound,Twenty,Five,Two]
Enter a number and I'll count out the change
13
[Ten,Two,Penny]
Enter a number and I'll count out the change

$
```

In `Change.hs`, the `makeChange` function starts with the total amount of money that
we need to make change for. It calculates the number of highest value
coins that can be taken from this amount, then calls itself
recursively with the remainder amount. Study the code so that you
understand it.

There are two test suites in this project, one that uses HUnit and one
that uses QuickCheck. Read the source files in the `test` directory to
get an idea of how they work. You can run them separately by name
(`cabal test test-hunit` or `cabal test test-quickcheck`) or together
(`cabal test`).

## Exercise

The argument to `Change.makeChange` is the current amount of money that we
are trying to make change for. You will change this program to use the
`State` monad to store the remaining amount as a piece of state to be
read and updated in the `makeChange` function.

Add the import statement `import Control.Monad.State.Lazy` to the top
of `Main.hs` and `Change.hs`. You also need to tell `cabal` about this
dependency. Add `monads-tf` (Monad Transformer Library) to the list of
dependencies:

    build-depends: base, monads-tf
	
Then run `cabal configure` again. Change the type of `makeChange` like so:

    makeChange :: State Int [Coins]

This tells us that `makeChange` is a function in the `State` monad,
that the state being stored is an `Int` and that the result of the
state processor will have the type `[Coin]`. This function will now
begin by using `get` to retrieve the remaining amount, then `getCoin`
and `coinDiv` to work out which coins to add to the list and the new
remaining amount, as before. Use `put` to store the new remaining
amount. Note that the recursive call to `makeChange` is now monadic so
it will look like this:

    rest <- makeChange

Change the `main` function to run `makeChange` using `evalState` and the 
number supplied by the user as the initial state:

    let i     = read str :: Int
	    coins = evalState makeChange i
		
To make the tests work you will also need to change the calls to
`makeChange` in `TestHUnit.hs` and `TestQuickCheck.hs` to use
`evalState`. 
