# Haskell Programming From First Principles Notes

## Chapter 11

* Vigenere and phone exercises need revisiting to understand better. Bit of sloppy code here.

## Chapter 12

* `(zip <*> tail)` should be explained in a later chapter. http://pointfree.io/ was used for generating this.

## Chapter 13

* Use `:browse` to browse a module. For example: `:browse Data.Bool`
* `stack ghci --ghci-options -XNoImplicitPrelude` to start ghci without the prelude module automatically imported
* `ghc -o todo .\todo.hs` to compile a haskell program
* Create a project using `stack new name simple`
* Set the prompt using `:set prompt "Lambda> "`
* You can use `let` in a do block to assign a value. i.e. `let person = mkPerson name (read age :: Integer)`