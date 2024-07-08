#ifndef HSCPYTHON_SHIM_H
#define HSCPYTHON_SHIM_H

#include <Python.h>

/* Initialization helpers */
void hscpython_SetProgramName(wchar_t *);
void hscpython_SetPythonHome(wchar_t *);

/* Object */
void hscpython_Py_INCREF(PyObject *);
void hscpython_Py_DECREF(PyObject *);
int hscpython_PyObject_DelAttr(PyObject *, PyObject *);
int hscpython_PyObject_TypeCheck(PyObject *, PyTypeObject *);
int hscpython_PyIter_Check(PyObject *);

enum HSCPythonComparisonEnum
{ HSCPYTHON_LT = Py_LT
, HSCPYTHON_LE = Py_LE
, HSCPYTHON_EQ = Py_EQ
, HSCPYTHON_NE = Py_NE
, HSCPYTHON_GT = Py_GT
, HSCPYTHON_GE = Py_GE
};

/* Types */
PyTypeObject *hscpython_PyType_Type();
PyTypeObject *hscpython_PyTuple_Type();
PyTypeObject *hscpython_PyList_Type();
PyTypeObject *hscpython_PyDict_Type();
PyTypeObject *hscpython_PyLong_Type();
PyTypeObject *hscpython_PyFloat_Type();
PyTypeObject *hscpython_PyComplex_Type();
PyTypeObject *hscpython_PyUnicode_Type();
PyTypeObject *hscpython_PyBytes_Type();
PyTypeObject *hscpython_PyByteArray_Type();
PyTypeObject *hscpython_PyCell_Type();
PyTypeObject *hscpython_PyCode_Type();
PyTypeObject *hscpython_PyFunction_Type();
PyTypeObject *hscpython_PyInstanceMethod_Type();
PyTypeObject *hscpython_PyMethod_Type();
PyTypeObject *hscpython_PySet_Type();
PyTypeObject *hscpython_PyFrozenSet_Type();
PyTypeObject *hscpython_PySeqIter_Type();
PyTypeObject *hscpython_PyCallIter_Type();
PyTypeObject *hscpython_PySlice_Type();
PyTypeObject *hscpython_PyModule_Type();
PyTypeObject *hscpython_PyCapsule_Type();

/* Constants */
PyObject *hscpython_Py_None();
PyObject *hscpython_Py_True();
PyObject *hscpython_Py_False();

/* Unicode */
Py_ssize_t hscpython_PyUnicode_GetSize(PyObject *);
wchar_t *hscpython_PyUnicode_AsUnicode(PyObject *);
PyObject *hscpython_PyUnicode_FromUnicode(const wchar_t *, Py_ssize_t);
PyObject *hscpython_PyUnicode_FromEncodedObject(PyObject *, const char *, const char *);
PyObject *hscpython_PyUnicode_AsEncodedString(PyObject *, const char *, const char *);
PyObject *hscpython_PyUnicode_FromObject(PyObject *);
PyObject *hscpython_PyUnicode_Decode(const char *, Py_ssize_t, const char *, const char *);
PyObject *hscpython_PyUnicode_Concat(PyObject *, PyObject *);
PyObject *hscpython_PyUnicode_Split(PyObject *, PyObject *, Py_ssize_t);
PyObject *hscpython_PyUnicode_Splitlines(PyObject *, int);
PyObject *hscpython_PyUnicode_Translate(PyObject *, PyObject *, const char *);
PyObject *hscpython_PyUnicode_Join(PyObject *, PyObject *);
int hscpython_PyUnicode_Tailmatch(PyObject *, PyObject *, Py_ssize_t, Py_ssize_t, int);
Py_ssize_t hscpython_PyUnicode_Find(PyObject *, PyObject *, Py_ssize_t, Py_ssize_t, int);
Py_ssize_t hscpython_PyUnicode_Count(PyObject *, PyObject *, Py_ssize_t, Py_ssize_t);
PyObject *hscpython_PyUnicode_Replace(PyObject *, PyObject *, PyObject *, Py_ssize_t);
PyObject *hscpython_PyUnicode_Format(PyObject *, PyObject *);
int hscpython_PyUnicode_Contains(PyObject *, PyObject *);

/* Lists */
void hscpython_peek_list(PyObject *, Py_ssize_t, PyObject **);
PyObject *hscpython_poke_list(size_t, PyObject **);

/* Tuples */
void hscpython_peek_tuple(PyObject *, Py_ssize_t, PyObject **);
PyObject *hscpython_poke_tuple(size_t, PyObject **);

#endif
