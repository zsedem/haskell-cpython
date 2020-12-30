# Haskell CPython C bindings [![Build Status](https://travis-ci.org/zsedem/haskell-cpython.svg?branch=3.5)](https://travis-ci.org/zsedem/haskell-cpython)

## Writing a Haskell wrapper over a Python module

The easiest way to get started is to `import CPython.Simple`

The API surface there is fairly small, although if you're doing something fancy you may need to dip into other parts of `CPython`

### General Info

`initialize :: IO ()` kicks off talking to Python, and will need to be called by your users before they use your library.

The `ToPy` and `FromPy` instances are what let us convert Haskell values to and from the corresponding Python values. There are `easyToPy` and `easyFromPy` helpers to help you easily write your own instances in common cases.

`Arg` is a type representing an argument to a Python function, which lets us use various Haskell types in the same list of arguments.

```haskell
sampleArgs :: [Arg]
sampleArgs =
  [ arg (7 :: Integer)
  , arg ("hello" :: Text)
  ]
```

### Calling Functions

The most common use case is to call some Python function, which you can do with `call`

```haskell
call
  :: FromPy a
  => Text -- ^ module name
  -> Text -- ^ function name
  -> [Arg] -- ^ args
  -> [(Text, Arg)] -- ^ keyword args
  -> IO a
```

For example, if I wanted to wrap Python's `random.randint(low, high)`, I could write this:

```haskell
randint :: Integer -> Integer -> IO Integer
randint low high = do
  call "random" "randint" [arg low, arg high] []
```

Because of the `FromPy` instance in `call`'s type signature, we can infer what to do to convert a Python value back into Haskell, if given the type.

```haskell
uniform :: Integer -> Integer -> IO Double
uniform low high = do
  call "random" "uniform" [arg low, arg high] []
```

You can also use the `TypeApplications` language extension to do this, if needed.

```haskell
call @Double "random" "uniform" [arg low, arg high] []
```

Calling a function with mixed positional and keyword arguments is also fairly straightforward.

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
