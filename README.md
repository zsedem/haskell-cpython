# Haskell CPython C bindings [![Build Status](https://travis-ci.org/zsedem/haskell-cpython.svg?branch=main)](https://travis-ci.org/zsedem/haskell-cpython)

This library provides the C bindings to more or less all of the python3 C API.

## Using the Low Level API

We provide a very thin layer between the C API and you to be able to avoid 
writing FFI codes directly (like calling incref/decref for GC). The C API 
documentation can be useful too. [link](https://docs.python.org/3/c-api/index.html)

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
