# First steps

In this guide we'll take a look at a few core tools that are installed
with the Haskell toolchain, namely, `ghc`, `runghc` and `ghci`.
These tools can be used to compile, interpret or explore Haskell programs.

First, let's start by opening your system's command line interface
and running `ghc --version` to make sure we have successfully
installed a Haskell toolchain:

```
➜ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.10.7
```

If this fails, consult [the Getting started page](install.md) for information on
how to install Haskell on your computer.

This guide is partly based on [Gil Mizrahi's blog](https://gilmi.me/blog/post/2021/08/14/hs-core-tools).

## Compiling programs with ghc

Running `ghc` invokes the Glasgow Haskell Compiler (GHC), and can be used to
compile Haskell modules and programs into native executables and libraries.

Create a new Haskell source file named `hello.hs`,
and write the following code in it:

```hs
main = putStrLn "Hello, Haskell!"
```

Now, we can compile the program by invoking `ghc` with the file name:

```sh
➜ ghc hello.hs
[1 of 1] Compiling Main             ( hello.hs, hello.o )
Linking hello ...
```

For more in-depth information about the files `ghc` produces,
follow the [GHC user guide](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/using.html#getting-started-compiling-programs) guide.

Now we run our program:

```sh
➜ ./hello
Hello, Haskell!
```

Alternatively, we can skip the compilation phase by using the command `runghc`:

```sh
➜ runghc hello.hs
Hello, Haskell!
```

`runghc` interprets the source file instead of compiling it and does not
create build artifacts. This makes it very useful when developing programs
and can help accelerate the feedback loop. More information about `runghc`
can be found in the
[GHC user guide](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/runghc.html).

### Turning on warnings

The `-Wall` flag will enable GHC to emit warnings about our code.

```sh
➜ ghc -Wall hello.hs -fforce-recomp
[1 of 1] Compiling Main             ( hello.hs, hello.o )

hello.hs:1:1: warning: [-Wmissing-signatures]
    Top-level binding with no type signature: main :: IO ()
  |
1 | main = putStrLn "Hello, Haskell!"
  | ^^^^
Linking hello ...
```

While Haskell can infer
the types of most expressions, it is recommended that top-level definitions
are annotated with their types.

Now our `hello.hs` source file should look like this:

```hs
main :: IO ()
main = putStrLn "Hello, world!"
```

And now GHC will compile `hello.hs` without warnings.

## An interactive environment

GHC provides an interactive environment in a form of a
Read-Evaluate-Print Loop (REPL) called GHCi.
To enter the environment run the program `ghci`.

```sh
➜ ghci
GHCi, version 9.0.2: https://www.haskell.org/ghc/  :? for help
ghci>
```

It provides an interactive prompt where Haskell expressions can be written and
evaluated.

For example:

```sh
ghci> 1 + 1
2
ghci> putStrLn "Hello, world!"
Hello, world!
```

We can define new names:

```sh
ghci> double x = x + x
ghci> double 2
4
```

We can write multi-line code by surrounding it with `:{` and `:}`:

```hs
ghci> :{
| map f list =
|     case list of
|         [] -> []
|         x : xs -> f x : map f xs
| :}
ghci> map (+1) [1, 2, 3]
[2,3,4]

```

We can import Haskell source files using the `:load` command (`:l` for short):

```sh
ghci> :load hello.hs
[1 of 1] Compiling Main             ( hello.hs, interpreted )
Ok, one module loaded.
ghci> main
Hello, Haskell!
```

As well as import library modules:

```sh
ghci> import Data.Bits
ghci> shiftL 32 1
64
ghci> clearBit 33 0
32
```

We can even ask what the type of an expression is using the `:type` command
(`:t` for short):

```sh
λ> :type putStrLn
putStrLn :: String -> IO ()
```

To exit `ghci`, use the `:quit` command (or `:q` for short)

```sh
ghci> :quit
Leaving GHCi.
```

A more thorough introduction to GHCi can be found in the
[GHC user guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html).

### Using external packages in ghci

By default, GHCi can only load and use packages that are
[included with the GHC installation](https://downloads.haskell.org/ghc/9.4.2/docs/users_guide/9.4.2-notes.html#included-libraries).

However, users of the [cabal-install](https://www.haskell.org/cabal) and
[stack](http://haskellstack.org) build tools can download and load external packages
very easily using the following commands:

cabal-install:

```sh
cabal repl --build-depends async,say
```

Stack:

```sh
stack exec --package async --package say -- ghci
```

And the modules of the relevant packages will be available for import:

```sh
GHCi, version 9.0.1: https://www.haskell.org/ghc/  :? for help
ghci> import Control.Concurrent.Async
ghci> import Say
ghci> concurrently_ (sayString "Hello") (sayString "World")
Hello
World
```

Stack users can also use this feature with `runghc` and `ghc` by replacing
`ghci` in the command above, and cabal-install users can generate an
environment file that will make `async` and `say` visible for GHC tools
in the current directory using this command:

```sh
cabal install --lib async say --package-env .
```

Many more packages are waiting for you on [Hackage](https://hackage.haskell.org).

## Creating a proper package with modules

The previous methods to compile Haskell code are for quick experiments and small
programs. Usually in Haskell, we create cabal projects, where build tools such as
`cabal-install` or `stack` will install necessary dependencies and compile modules
in correct order. For simplicity's sake, this section will only use `cabal-install`.

To get started, run:

```sh
mkdir haskell-project
cd haskell-project
cabal init --interactive
```

If you let it generate a simple project with sensible defaults, then you should have these files:

* `src/MyLib.hs`: the library module of your project
* `app/Main.hs`: the entry point of your project
* `haskell-project.cabal`: the "cabal" file, describing your project, its dependencies and how it's built

To build the project, run:

```sh
cabal build
```

To run the main executable, run:

```sh
➜ cabal run
Hello, Haskell!
someFunc
```

### Adding dependencies

Now let's add a dependency and adjust our library module. Open `haskell-project.cabal`
and find the library section:

```
library
    exposed-modules:  MyLib

    -- Modules included in this library but not exported.
    -- other-modules:

    -- LANGUAGE extensions used by modules in this package.
    -- other-extensions:
    build-depends:    base ^>=4.14.3.0
    hs-source-dirs:   src
    default-language: Haskell2010
```

The interesting parts here are `exposed-modules` and `build-depends`.
To add a dependency, it should look like this:

```
    build-depends:    base ^>=4.14.3.0
	                , directory
```

Now open `src/MyLib.hs` and change it to:

```hs
module MyLib (someFunc) where

import System.Directory

someFunc :: IO ()
someFunc = do
  contents <- listDirectory "src"
  putStrLn (show contents)
```

### Adding modules

To add a module to your package, adjust `exposed-modules`, like so

```
    exposed-modules:  MyLib
	                  OtherLib
```

then create `src/OtherLib.hs` with the following contents:

```hs
module OtherLib where

otherFunc :: String -> Int
otherFunc str = length str
```

To use this function interactively, we can run:

```sh
➜ cabal repl
ghci> import OtherLib
ghci> otherFunc "Hello Haskell"
13
```

For further information about how to manage Haskell projects
see the [Cabal user guide](https://cabal.readthedocs.io/en/stable/getting-started.html).

# Where to go from here

<div class="text-center main-buttons">
<a href="https://hackage.haskell.org/" class="btn btn-primary" role="button">Discover Haskell packages</a>
<a href="https://hackage.haskell.org/package/base" class="btn btn-primary" role="button">The standard library</a>
<a href="https://haskell-language-server.readthedocs.io/en/stable/installation.html" class="btn btn-primary" role="button">Editor setup with HLS</a>
<a href="https://play.haskell.org/" class="btn btn-primary" role="button">Online playground</a>
</div>

## How to learn Haskell proper

To learn Haskell, try any of those:

- A beginner friendly [4-lectures course](https://github.com/haskell-beginners-2022/course-plan) with exercises (by [Dmitrii Kovanikov](https://chshersh.com/))
- An in-depth university [CIS 194 Haskell course](https://www.cis.upenn.edu/~cis194/spring13/) including exercises (by [Brent Yorgey](https://byorgey.wordpress.com/))

## Projects to contribute to

* [https://github.com/haskell/haskell-language-server](https://github.com/haskell/haskell-language-server)
* [https://github.com/haskell/cabal](https://github.com/haskell/cabal)
* [https://github.com/commercialhaskell/stack](https://github.com/commercialhaskell/stack)
* [https://github.com/haskell/ghcup-hs](https://github.com/haskell/ghcup-hs)
* [https://github.com/jgm/pandoc](https://github.com/jgm/pandoc)
* [https://github.com/simonmichael/hledger](https://github.com/simonmichael/hledger)
* [https://github.com/koalaman/shellcheck](https://github.com/koalaman/shellcheck)
