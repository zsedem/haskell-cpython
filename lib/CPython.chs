{-# LANGUAGE ForeignFunctionInterface #-}

-- Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module CPython
  ( initialize
  , isInitialized
  , finalize
  , ThreadState
  , newInterpreter
  , endInterpreter
  , evalInitThreads
  , saveThread
  , restoreThread
  , GilState
  , gilStateEnsure
  , gilStateRelease
  , withGilStateEnsure
  , getProgramName
  , setProgramName
  , getPrefix
  , getExecPrefix
  , getProgramFullPath
  , getPath
  , getVersion
  , getPlatform
  , getCopyright
  , getCompiler
  , getBuildInfo
  , setArgv
  , getPythonHome
  , setPythonHome
  ) where

#include <hscpython-shim.h>

import           Data.Text (Text)

import           CPython.Internal
import           Control.Concurrent
import           Control.Exception(bracket)

-- | Initialize the Python interpreter. In an application embedding Python,
-- this should be called before using any other Python/C API computations;
-- with the exception of 'setProgramName', 'initThreads',
-- 'releaseLock', and 'acquireLock'. This initializes the table
-- of loaded modules (@sys.modules@), and creates the fundamental modules
-- @builtins@, @__main__@ and @sys@. It also initializes the module search
-- path (@sys.path@). It does not set @sys.argv@; use 'setArgv' for that. This
-- is a no-op when called for a second time (without calling 'finalize'
-- first). There is no return value; it is a fatal error if the initialization
-- fails.
--
-- In a multi-threaded setup you still have to call 'withGilStateEnsure' or 
-- 'gilState_Ensure'
{# fun Py_Initialize as initialize
  {} -> `()' id #}

-- | Return 'True' when the Python interpreter has been initialized, 'False'
-- if not. After 'finalize' is called, this returns 'False' until
-- 'initialize' is called again.
{# fun Py_IsInitialized as isInitialized
  {} -> `Bool' #}

-- | Undo all initializations made by 'initialize' and subsequent use of
-- Python/C API computations, and destroy all sub-interpreters (see
-- 'newInterpreter' below) that were created and not yet destroyed since the
-- last call to 'initialize'. Ideally, this frees all memory allocated by the
-- Python interpreter. This is a no-op when called for a second time (without
-- calling 'initialize' again first). There is no return value; errors during
-- finalization are ignored.
--
-- This computation is provided for a number of reasons. An embedding
-- application might want to restart Python without having to restart the
-- application itself. An application that has loaded the Python interpreter
-- from a dynamically loadable library (or DLL) might want to free all memory
-- allocated by Python before unloading the DLL. During a hunt for memory
-- leaks in an application a developer might want to free all memory
-- allocated by Python before exiting from the application.
--
-- /Bugs and caveats/: The destruction of modules and objects in modules is
-- done in arbitrary order; this may cause destructors (@__del__()@ methods)
-- to fail when they depend on other objects (even functions) or modules.
-- Dynamically loaded extension modules loaded by Python are not unloaded.
-- Small amounts of memory allocated by the Python interpreter may not be
-- freed (if you find a leak, please report it). Memory tied up in circular
-- references between objects is not freed. Some memory allocated by extension
-- modules may not be freed. Some extensions may not work properly if their
-- initialization routine is called more than once; this can happen if an
-- application calls 'initialize' and 'finalize' more than once.
{# fun Py_Finalize as finalize
  {} -> `()' id #}


newtype ThreadState = ThreadState (Ptr ThreadState) deriving (Show)

-- | Create a new sub-interpreter. This is an (almost) totally separate
-- environment for the execution of Python code. In particular, the new
-- interpreter has separate, independent versions of all imported modules,
-- including the fundamental modules @builtins@, @__main__@ and @sys@. The
-- table of loaded modules (@sys.modules@) and the module search path
-- (@sys.path@) are also separate. The new environment has no @sys.argv@
-- variable. It has new standard I/O stream file objects @sys.stdin@,
-- @sys.stdout@ and @sys.stderr@ (however these refer to the same underlying
-- @FILE@ structures in the C library).
--
-- The return value points to the first thread state created in the new
-- sub-interpreter. This thread state is made in the current thread state.
-- Note that no actual thread is created; see the discussion of thread states
-- below. If creation of the new interpreter is unsuccessful, 'Nothing' is
-- returned; no exception is set since the exception state is stored in the
-- current thread state and there may not be a current thread state. (Like
-- all other Python/C API computations, the global interpreter lock must be
-- held before calling this computation and is still held when it returns;
-- however, unlike most other Python/C API computations, there
-- needn&#x2019;t be a current thread state on entry.)
--
-- Extension modules are shared between (sub-)interpreters as follows: the
-- first time a particular extension is imported, it is initialized normally,
-- and a (shallow) copy of its module&#x2019;s dictionary is squirreled away.
-- When the same extension is imported by another (sub-)interpreter, a new
-- module is initialized and filled with the contents of this copy; the
-- extension&#x2019;s @init@ procedure is not called. Note that this is
-- different from what happens when an extension is imported after the
-- interpreter has been completely re-initialized by calling 'finalize' and
-- 'initialize'; in that case, the extension&#x2019;s @init/module/@
-- procedure is called again.
--
-- /Bugs and caveats/: Because sub-interpreters (and the main interpreter)
-- are part of the same process, the insulation between them isn&#x2019;t
-- perfect &#x2014; for example, using low-level file operations like
-- @os.close()@ they can (accidentally or maliciously) affect each
-- other&#x2019;s open files. Because of the way extensions are shared
-- between (sub-)interpreters, some extensions may not work properly; this
-- is especially likely when the extension makes use of (static) global
-- variables, or when the extension manipulates its module&#x2019;s
-- dictionary after its initialization. It is possible to insert objects
-- created in one sub-interpreter into a namespace of another
-- sub-interpreter; this should be done with great care to avoid sharing
-- user-defined functions, methods, instances or classes between
-- sub-interpreters, since import operations executed by such objects may
-- affect the wrong (sub-)interpreter&#x2019;s dictionary of loaded modules.
-- (XXX This is a hard-to-fix bug that will be addressed in a future release.)
--
-- Also note that the use of this functionality is incompatible with
-- extension modules such as PyObjC and ctypes that use the @PyGILState_*()@
-- APIs (and this is inherent in the way the @PyGILState_*()@ procedures
-- work). Simple things may work, but confusing behavior will always be near.
newInterpreter :: IO (Maybe ThreadState)
newInterpreter = do
  ptr <- {# call Py_NewInterpreter as ^ #}
  return $ if ptr == nullPtr
    then Nothing
    else Just $ ThreadState $ castPtr ptr

-- | Destroy the (sub-)interpreter represented by the given thread state.
-- The given thread state must be the current thread state. See the
-- discussion of thread states below. When the call returns, the current
-- thread state is @NULL@. All thread states associated with this
-- interpreter are destroyed. (The global interpreter lock must be held
-- before calling this computation and is still held when it returns.)
-- 'finalize' will destroy all sub-interpreters that haven&#x2019;t been
-- explicitly destroyed at that point.
endInterpreter :: ThreadState -> IO ()
endInterpreter (ThreadState ptr) =
  {# call Py_EndInterpreter as ^ #} $ castPtr ptr

evalInitThreads :: IO ()
evalInitThreads = {# call PyEval_InitThreads as ^ #}

{#enum PyGILState_STATE as GilState {upcaseFirstLetter} deriving(Show) #}

toGilState :: CInt -> GilState
toGilState = toEnum . fromIntegral
fromGilState :: GilState -> CInt
fromGilState = fromIntegral . fromEnum

-- | Ensure that the current thread is ready to call the Python C API regardless
-- of the current state of Python, or of the global interpreter lock. This may
-- be called as many times as desired by a thread as long as each call is matched
-- with a 'gilStateRelease'. In general, other thread-related APIs may be used
-- between 'gilStateEnsure' and gilStateRelease calls as long as the thread state
-- is restored to its previous state before the Release. For this to work
-- the thread must be a bound thread. (consider using `withGilStateEnsure`
-- method)
--
-- Note: Calling this function from a thread when the runtime is finalizing
-- will terminate the thread.
gilStateEnsure :: IO GilState
gilStateEnsure = do
  state <- {# call PyGILState_Ensure as ^ #}
  return $ toGilState state

-- | Release any resources previously acquired. After this call,
-- Python’s -- state will be the same as it was prior to the corresponding
-- 'gilStateEnsure' call (but generally this state will be unknown to
-- the caller, hence the use of the GILState API). Every call to
-- 'gilStateEnsure' must be matched by a call to 'gilStateRelease'
-- on the same bound thread.
gilStateRelease :: GilState -> IO ()
gilStateRelease s =
  {# call PyGILState_Release as ^ #} $ fromGilState s

-- | Ensure that the current thread is ready to call the Python C API
-- regardless of the current state of Python, or of the global interpreter
-- lock.
withGilStateEnsure :: IO a -> IO a
withGilStateEnsure ioOperation =
  bracket gilStateEnsure gilStateRelease $ \_ -> ioOperation

-- | Return the program name set with 'setProgramName', or the default.
getProgramName :: IO Text
getProgramName = pyGetProgramName >>= peekTextW

foreign import ccall safe "hscpython-shim.h Py_GetProgramName"
  pyGetProgramName :: IO CWString

-- | This computation should be called before 'initialize' is called for the
-- first time, if it is called at all. It tells the interpreter the value of
-- the @argv[0]@ argument to the @main@ procedure of the program. This is
-- used by 'getPath' and some other computations below to find the Python
-- run-time libraries relative to the interpreter executable. The default
-- value is @\"python\"@. No code in the Python interpreter will change the
-- program name.
setProgramName :: Text -> IO ()
setProgramName name = withTextW name cSetProgramName

foreign import ccall safe "hscpython-shim.h hscpython_SetProgramName"
  cSetProgramName :: CWString -> IO ()

-- | Return the prefix for installed platform-independent files. This is
-- derived through a number of complicated rules from the program name set
-- with 'setProgramName' and some environment variables; for example, if the
-- program name is @\"\/usr\/local\/bin\/python\"@, the prefix is
-- @\"\/usr\/local\"@. This corresponds to the @prefix@ variable in the
-- top-level Makefile and the /--prefix/ argument to the @configure@ script
-- at build time. The value is available to Python code as @sys.prefix@. It
-- is only useful on UNIX. See also 'getExecPrefix'.
getPrefix :: IO Text
getPrefix = pyGetPrefix >>= peekTextW

foreign import ccall safe "hscpython-shim.h Py_GetPrefix"
  pyGetPrefix :: IO CWString

-- | Return the /exec-prefix/ for installed platform-/dependent/ files. This
-- is derived through a number of complicated rules from the program name
-- set with setProgramName' and some environment variables; for example, if
-- the program name is @\"\/usr\/local\/bin\/python\"@, the exec-prefix is
-- @\"\/usr\/local\"@. This corresponds to the @exec_prefix@ variable in the
-- top-level Makefile and the /--exec-prefix/ argument to the @configure@
-- script at build time. The value is available to Python code as
-- @sys.exec_prefix@. It is only useful on UNIX.
--
-- Background: The exec-prefix differs from the prefix when platform
-- dependent files (such as executables and shared libraries) are installed
-- in a different directory tree. In a typical installation, platform
-- dependent files may be installed in the @\/usr\/local\/plat@ subtree while
-- platform independent may be installed in @\/usr\/local@.
--
-- Generally speaking, a platform is a combination of hardware and software
-- families, e.g. Sparc machines running the Solaris 2.x operating system
-- are considered the same platform, but Intel machines running Solaris
-- 2.x are another platform, and Intel machines running Linux are yet
-- another platform. Different major revisions of the same operating system
-- generally also form different platforms. Non-UNIX operating systems are a
-- different story; the installation strategies on those systems are so
-- different that the prefix and exec-prefix are meaningless, and set to the
-- empty string. Note that compiled Python bytecode files are platform
-- independent (but not independent from the Python version by which they
-- were compiled!).
--
-- System administrators will know how to configure the @mount@ or @automount@
-- programs to share @\/usr\/local@ between platforms while having
-- @\/usr\/local\/plat@ be a different filesystem for each platform.
getExecPrefix :: IO Text
getExecPrefix = pyGetExecPrefix >>= peekTextW

foreign import ccall safe "hscpython-shim.h Py_GetExecPrefix"
  pyGetExecPrefix :: IO CWString

-- | Return the full program name of the Python executable; this is computed
-- as a side-effect of deriving the default module search path from the
-- program name (set by 'setProgramName' above). The value is available to
-- Python code as @sys.executable@.
getProgramFullPath :: IO Text
getProgramFullPath = pyGetProgramFullPath >>= peekTextW

foreign import ccall safe "hscpython-shim.h Py_GetProgramFullPath"
  pyGetProgramFullPath :: IO CWString

-- | Return the default module search path; this is computed from the
-- program name (set by 'setProgramName' above) and some environment
-- variables. The returned string consists of a series of directory names
-- separated by a platform dependent delimiter character. The delimiter
-- character is @\':\'@ on Unix and Mac OS X, @\';\'@ on Windows. The value
-- is available to Python code as the list @sys.path@, which may be modified
-- to change the future search path for loaded modules.
getPath :: IO Text
getPath = pyGetPath >>= peekTextW

foreign import ccall safe "hscpython-shim.h Py_GetPath"
  pyGetPath :: IO CWString

-- | Return the version of this Python interpreter. This is a string that
-- looks something like
--
-- @
--  \"3.0a5+ (py3k:63103M, May 12 2008, 00:53:55) \\n[GCC 4.2.3]\"
-- @
--
-- The first word (up to the first space character) is the current Python
-- version; the first three characters are the major and minor version
-- separated by a period. The value is available to Python code as
-- @sys.version@.
{# fun Py_GetVersion as getVersion
  {} -> `Text' peekText* #}

-- | Return the platform identifier for the current platform. On Unix, this
-- is formed from the &#x201c;official&#x201d; name of the operating system,
-- converted to lower case, followed by the major revision number; e.g., for
-- Solaris 2.x, which is also known as SunOS 5.x, the value is @\"sunos5\"@.
-- On Mac OS X, it is @\"darwin\"@. On Windows, it is @\"win\"@. The value
-- is available to Python code as @sys.platform@.
{# fun Py_GetPlatform as getPlatform
  {} -> `Text' peekText* #}

-- | Return the official copyright string for the current Python version,
-- for example
--
-- @
--  \"Copyright 1991-1995 Stichting Mathematisch Centrum, Amsterdam\"
-- @
--
-- The value is available to Python code as @sys.copyright@.
{# fun Py_GetCopyright as getCopyright
  {} -> `Text' peekText* #}

-- | Return an indication of the compiler used to build the current Python
-- version, in square brackets, for example:
--
-- @
--  \"[GCC 2.7.2.2]\"
-- @
--
-- The value is available to Python code as part of the variable
-- @sys.version@.
{# fun Py_GetCompiler as getCompiler
  {} -> `Text' peekText* #}

-- | Return information about the sequence number and build date and time of
-- the current Python interpreter instance, for example
--
-- @
--  \"#67, Aug  1 1997, 22:34:28\"
-- @
--
-- The value is available to Python code as part of the variable
-- @sys.version@.
{# fun Py_GetBuildInfo as getBuildInfo
  {} -> `Text' peekText* #}

-- | Set @sys.argv@. The first parameter is similar to the result of
-- 'getProgName', with the difference that it should refer to the script
-- file to be executed rather than the executable hosting the Python
-- interpreter. If there isn&#x2019;t a script that will be run, the first
-- parameter can be an empty string. If this function fails to initialize
-- @sys.argv@, a fatal condition is signalled using @Py_FatalError()@.
--
-- This function also prepends the executed script&#x2019;s path to
-- @sys.path@. If no script is executed (in the case of calling @python -c@
-- or just the interactive interpreter), the empty string is used instead.
setArgv :: Text -> [Text] -> IO ()
setArgv argv0 argv =
  mapWith withTextW (argv0 : argv) $ \textPtrs ->
  let argc = fromIntegral $ length textPtrs in
  withArray textPtrs $ pySetArgv argc

foreign import ccall safe "hscpython-shim.h PySys_SetArgv"
  pySetArgv :: CInt -> Ptr CWString -> IO ()

-- | Return the default &#x201c;home&#x201d;, that is, the value set by a
-- previous call to 'setPythonHome', or the value of the @PYTHONHOME@
-- environment variable if it is set.
getPythonHome :: IO (Maybe Text)
getPythonHome = pyGetPythonHome >>= peekMaybeTextW

foreign import ccall safe "hscpython-shim.h Py_GetPythonHome"
  pyGetPythonHome :: IO CWString

-- | Set the default &#x201c;home&#x201d; directory, that is, the location
-- of the standard Python libraries. The libraries are searched in
-- @/home/\/lib\//python version/@ and @/home/\/lib\//python version/@. No
-- code in the Python interpreter will change the Python home.
setPythonHome :: Maybe Text -> IO ()
setPythonHome name = withMaybeTextW name cSetPythonHome

foreign import ccall safe "hscpython-shim.h hscpython_SetPythonHome"
  cSetPythonHome :: CWString -> IO ()
