# Haskell CPython C bindings [![Build Status](https://travis-ci.org/zsedem/haskell-cpython.svg?branch=main)](https://travis-ci.org/zsedem/haskell-cpython)

This library provides C bindings to more or less all of the python3 C API.

> WARNING: Note that the python 3 C API might be quite stable, BUT the [ABI](https://docs.python.org/3/c-api/stable.html) is not, which
means if you compiled with a certain minor version (3.7.1) you should run your program
with the same minor version (3.7.x). (Using docker or nix to package your program is enough
to avoid this problem)

## Writing a Haskell wrapper over a Python module

The easiest way to get started is to `import CPython.Simple`

The `Simple` API surface is fairly small, so if you're doing something fancy you may need to dip into other parts of `CPython`

### General Info

`initialize :: IO ()` kicks off talking to Python, and will need to be called before using other functions.

The `ToPy` and `FromPy` instances are what let us convert Haskell values to and from the corresponding Python values. There are `easyToPy` and `easyFromPy` helpers to help you easily write your own instances in common cases. If you find an instance for some common Haskell type is missing, please submit a PR!

`Arg` is a type representing an argument to a Python function, which lets us use various Haskell types in the same list of arguments.

```haskell
sampleArgs :: [Arg]
sampleArgs =
  [ arg (7 :: Integer)
  , arg ("hello" :: Text)
  ]
```

### Calling Functions

The most common use case is to call some Python function, which we can do with `call`

```haskell
call
  :: FromPy a
  => Text -- ^ module name
  -> Text -- ^ function name
  -> [Arg] -- ^ args
  -> [(Text, Arg)] -- ^ keyword args
  -> IO a
```

For example, if we wanted to wrap Python's `random.randint(low, high)`, we could write this:

```haskell
randint :: Integer -> Integer -> IO Integer
randint low high =
  call "random" "randint" [arg low, arg high] []
```

Because of the `FromPy` instance in `call`'s type signature, we can infer what to do to convert a Python value back into Haskell, if given the type

```haskell
uniform :: Integer -> Integer -> IO Double
uniform low high =
  call "random" "uniform" [arg low, arg high] []
```

We can also use the `TypeApplications` language extension to do this, if needed

```haskell
call @Double "random" "uniform" [arg low, arg high] []
```

Calling a function with mixed positional and keyword arguments is also fairly straightforward:

```haskell
moveToDuration :: Integer -> Integer -> Double -> IO ()
moveToDuration x y seconds =
  call "pyautogui" "moveTo" [arg x, arg y] [("duration", arg seconds)]
```

### Getting and Setting Attributes

`getAttribute` lets us get the value of an attribute of some Python module

```haskell
getAttribute
  :: FromPy a
  => Text -- ^ module name
  -> Text -- ^ attribute name
  -> IO a
```

Here, we get the value of `random.BPF`

```haskell
getBpf :: IO Integer
getBpf = getAttribute "random" "BPF"
```

Likewise, `setAttribute` lets us set the value of an attribute

```haskell
setAttribute
  :: ToPy a
  => Text -- ^ module name
  -> Text -- ^ attribute name
  -> a -- ^ value to set attribute to
  -> IO ()
```

Here's how we can set `random.BPF` to some given number `n`

```haskell
setBpf :: Integer -> IO ()
setBpf n = setAttribute "random" "BPF" n
```

## Using the Low Level API

Sometimes it might be useful to use the less simpler API, especially if you are
already familiar with the [CPython C API](https://docs.python.org/3/c-api/index.html).
This API comes with one-on-one connections between the C API methods and the Haskell methods,
but you won't have to write FFI code directly (like calling incref/decref for Python GC).

After you are familiar with the concepts from the C API, you can search for
methods in the [API docs](http://hackage.haskell.org/package/cpython-3.5.0) on hackage

These examples below should help you start with using the API, by showing the 
equivalent haskell code to implement the same as the python example.

### Using `builtins.sum` function
```haskell
sumWithPy :: [Integer] -> IO Int
sumWithPy intlist = do
    testList <- traverse toObj intlist >>= PyList.toList >>= (return . Py.toObject)
    builtinsModule <- Py.importModule "builtins"
    sumFunc <- PyUnicode.toUnicode "sum" >>= Py.getAttribute builtinsModule
    args <- PyTuple.toTuple [testList]
    kwargs <- PyDict.new
    Py.call sumFunc args kwargs >>= castToNumber >>= Py.toInteger >>= PyInt.fromInteger
  where
    castToNumber obj = do x <- Py.castToNumber obj
                          return $ fromMaybe (error "not a number returned from the sum") x
    toObj integer = fmap Py.toObject $ PyInt.toInteger integer
```
This example should show you how different it is to call python from strongly typed code, because you have to
handle every bit of the errors, like getting an attribute of a module or just creating new python objects.

```python
intlist = [1, 10, 100, 42]
sum(intlist)
```
### Printing traceback from python
This example is an approach to handle python exceptions, like python would do it, so if an exception comes, we print a traceback
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main ) where

import qualified CPython as Py
import qualified CPython.Types.Module as Py
import qualified CPython.Types.Dictionary as PyDict
import qualified CPython.Types.List as PyList
import qualified CPython.Types.Tuple as PyTuple
import qualified CPython.Types.Unicode as PyUnicode
import qualified CPython.Types.Exception as PyExc
import Data.Text()
import Control.Exception(handle)

main :: IO ()
main = handle pyExceptionHandler $ do
  Py.initialize
  callingSomePython
  Py.finalize
  where
    pyExceptionHandler :: PyExc.Exception -> IO ()
    pyExceptionHandler exception = handle pyExceptionHandlerWithoutPythonTraceback $ do
        tracebackModule <- Py.importModule "traceback"
        print_exc <- PyUnicode.toUnicode "print_exception" >>= Py.getAttribute tracebackModule
        kwargs <- PyDict.new
        args <- case PyExc.exceptionTraceback exception of
          Just tb -> PyTuple.toTuple [PyExc.exceptionType exception, PyExc.exceptionValue exception, tb]
          _ -> PyTuple.toTuple [PyExc.exceptionType exception, PyExc.exceptionValue exception]
        _ <- Py.call print_exc args kwargs
        return ()
    pyExceptionHandlerWithoutPythonTraceback :: PyExc.Exception -> IO ()
    pyExceptionHandlerWithoutPythonTraceback exception = do
        print exception
        putStrLn "Unexpected Python exception (Please report a bug)"

callingSomePython :: IO ()
callingSomePython = do ...
```
