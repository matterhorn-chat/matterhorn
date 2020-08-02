
Haskell Proficiency for Matterhorn
==================================

If you want to contribute to Matterhorn but are new to Haskell, it will
be important to make sure that you have comfort with a number of Haskell
concepts in order to contribute successfully. Here are general Haskell
topics that it will be important to have comfort with:

* [Haddock syntax](https://github.com/aisamanra/haddock-cheatsheet/blob/master/haddocks.pdf)
* [Let vs. Where](https://wiki.haskell.org/Let_vs._Where)
* Record types (`Foo { field1 :: Ty, field2 :: Ty, ... }`)
* Monads (IO, Maybe, list, Either, State, Reader), "do" notation, combinators ">>=" and ">>"
* Monad Transformers (StateT, MaybeT, ErrorT, ReaderT)
* `Control.Monad`
* Type class instance derivation via `deriving`
* Applicative functor operations (`fmap`, `<$>`, `<*>`, `pure`)
* Collection types (Data.Map, Data.HashMap, Data.Set, Data.Sequence)
* File I/O (System.IO)
* [Argument parsing](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Console-GetOpt.html)
* Error-handling
  * Exceptions: https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html
  * General strategies for handling errors: http://book.realworldhaskell.org/read/error-handling.html
* Concurrent programming:
  * http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent.html
  * https://hackage.haskell.org/package/async
* Lenses: https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html
* Working with processes: http://hackage.haskell.org/package/process-1.6.8.2/docs/System-Process.html
* Data.Text: higher-performance and more general alternative to the built-in String type from the Prelude: http://hackage.haskell.org/package/text
* Type classes: the creation of new type classes and instances
* Vty library for building terminal applications: http://hackage.haskell.org/package/vty
* Brick library for building terminal applications (higher-level, built on top of Vty): https://github.com/jtdaugherty/brick

