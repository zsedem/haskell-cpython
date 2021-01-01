# Haskell CPython C bindings [![Build Status](https://travis-ci.org/zsedem/haskell-cpython.svg?branch=main)](https://travis-ci.org/zsedem/haskell-cpython)

This library provides the C bindings to more or less all of the python3 C API.

> WARNING: Note that the python 3 C API might be quite stable, BUT the [ABI](https://docs.python.org/3/c-api/stable.html) is not, which
means if you compiled with a certain minor version (3.7.1) you should run your program
with the same minor version (3.7.x). (Using docker or nix to package your program is enough
to avoid this problem)

## Using the Low Level API

We provide a very thin layer between the C API and you to be able to avoid 
writing FFI codes directly (like calling incref/decref for GC). The C API 
documentation can be useful too. [link](https://docs.python.org/3/c-api/index.html)

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
This example should show you how different is to call python from strongly typed code, because you have to
handle every bit of the errors, like geting an attribute of a module or just creating new python objects.

```python
intlist = [1, 10, 100, 42]
sum(intlist)
```
