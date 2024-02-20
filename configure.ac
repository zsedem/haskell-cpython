# Meta information about package to configure
AC_INIT([cpython], [3.5.1], [phillip.seeber@uni-jena.de])

# Check for the cpython.cabal file to be sure we are in the correct directory
AC_CONFIG_SRCDIR([cpython.cabal])

# Verify we have a working C-compiler available
AC_PROG_CC

# Check for Python3 libraries and headers
AM_PATH_PYTHON([3.8])

# Get linking for an embedded python interpreter via `python-config`. Required
# for correct linking
AC_MSG_CHECKING([for Python libs])
AS_IF([$PYTHON-config --libs --embed],
  [ PYTHON_LIBS=$($PYTHON-config --libs --embed) ],
  [AC_MSG_ERROR([Python libs not found])]
)
# Now, actually add the libraries returned by python-config to a CUSTOM_LIBS set.
# We can not use LIBS, as this will ruin M4 macros such as AC_SEARCH_LIBS if we
# interfere with LIBS manually.
PY_LIBS=(${PYTHON_LIBS// / })
for LIB in "${PY_LIBS@<:@@@:>@}"; do
  [[ "${LIB:0:2}" == "-l" ]] && CUSTOM_LIBS="$CUSTOM_LIBS $LIB"
done

# From LIBS and CUSTOM_LIBS strip "-l" to obtain a cabal-compatible list of 
# library names
LIBARR=(${CUSTOM_LIBS//-l/ })
for LIB in "${LIBARR@<:@@@:>@}"; do
  CUSTOM_LIBS_NAMES="$CUSTOM_LIBS_NAMES ${LIB#-l}"
done
LIBARR=(${LIBS//-l/ })
for LIB in "${LIBARR@<:@@@:>@}"; do
  LIBS_NAMES="$LIBS_NAMES ${LIB#-l}"
done

# Output file to create from its corresponding *.in file, in which @VARIABLES@
# will be substituted
AC_SUBST(LIBS_NAMES, $LIBS_NAMES)
AC_SUBST(CUSTOM_LIBS_NAMES, $CUSTOM_LIBS_NAMES)
AC_CONFIG_FILES([cpython.buildinfo])
AC_OUTPUT