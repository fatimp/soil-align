#define PY_SSIZE_T_CLEAN
#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#include <Python.h>
#include <numpy/arrayobject.h>
#include <stdlib.h>
#include <stdio.h>

struct python_state {
    int initialized;
    PyObject *pynndescent;
    PyObject *create_graph;
};

static struct python_state state = {.initialized = 0};

int nndescent_initialize () {
    if (state.initialized) {
        return 1;
    }

    Py_Initialize ();
    _import_array ();

    state.pynndescent = PyImport_ImportModule ("pynndescent");
    if (state.pynndescent == NULL) {
        fprintf (stderr, "Cannot import pynndescent\n");
        PyErr_Print ();
        goto cleanup;
    }

    state.create_graph = PyObject_GetAttrString (state.pynndescent, "NNDescent");
    if (state.create_graph == NULL || !PyCallable_Check (state.create_graph)) {
        fprintf (stderr, "Cannot find NNDescent\n");
        PyErr_Print ();
        Py_DECREF (state.pynndescent);
        goto cleanup;
    }

    state.initialized = 1;
    return 1;

cleanup:
    Py_Finalize ();
    return 0;
}

void nndescent_deinitialize () {
    if (state.initialized) {
        Py_DECREF (state.create_graph);
        Py_DECREF (state.pynndescent);
        Py_Finalize ();
        state.initialized = 0;
    }
}

void nndescent_find_closest (float *set1, size_t len1,
                             float *set2, size_t len2,
                             size_t nfeatures,
                             void (*callback) (const float *dists,
                                               const int   *indices,
                                               size_t       len)) {
    PyObject *set1_numpy   = NULL;
    PyObject *set2_numpy   = NULL;
    PyObject *graph        = NULL;
    PyObject *query        = NULL;
    PyObject *result       = NULL;
    PyObject *args;

    if (!state.initialized && !nndescent_initialize ()) {
        return;
    }

    npy_intp dims1[2] = { len1, nfeatures };
    set1_numpy = PyArray_SimpleNewFromData (2, dims1, NPY_FLOAT, set1);
    if (set1_numpy == NULL) {
        fprintf(stderr, "Failed to create a numpy array\n");
        PyErr_Print();
        goto cleanup;
    }

    npy_intp dims2[2] = { len2, nfeatures };
    set2_numpy = PyArray_SimpleNewFromData (2, dims2, NPY_FLOAT, set2);
    if (set2_numpy == NULL) {
        fprintf(stderr, "Failed to create a numpy array\n");
        PyErr_Print();
        goto cleanup;
    }

    args = PyTuple_New (1);
    PyTuple_SetItem (args, 0, set2_numpy);  // Reference is stolen
    graph = PyObject_Call (state.create_graph, args, NULL);
    Py_DECREF (args);
    if (graph == NULL) {
        fprintf (stderr, "Failed to call NNDescent\n");
        PyErr_Print();
        goto cleanup;
    }

    query = PyObject_GetAttrString (graph, "query");
    if (query == NULL || !PyCallable_Check (query)) {
        fprintf (stderr, "No query function\n");
        PyErr_Print();
        goto cleanup;
    }

    // Make a query
    PyObject *k = PyLong_FromLong (2);
    args = PyTuple_New (2);
    PyTuple_SetItem (args, 0, set1_numpy); // Reference is stolen
    PyTuple_SetItem (args, 1, k);          // Reference is stolen
    result = PyObject_Call (query, args, NULL);
    Py_DECREF (args);
    if (result == NULL || !PyTuple_Check (result)) {
        fprintf (stderr, "Unable to make a query\n");
        PyErr_Print();
        goto cleanup;
    }

    // References are borrwed
    PyObject *indices = PyTuple_GetItem (result, 0);
    PyObject *dists   = PyTuple_GetItem (result, 1);

    // TODO: Check dtype
    if (!PyArray_Check (indices) || !PyArray_Check (dists)) {
        fprintf (stderr, "Something weird in the result\n");
        PyErr_Print();
        goto cleanup;
    }

    npy_intp len = PyArray_DIM ((PyArrayObject*)indices, 0);
    const float *indices_ptr = PyArray_GETPTR1 ((PyArrayObject*)indices, 0);
    const float *dists_ptr   = PyArray_GETPTR1 ((PyArrayObject*)dists,   0);
    callback (dists_ptr, indices_ptr, len);

cleanup:
    Py_XDECREF (result);
    Py_XDECREF (query);
    Py_XDECREF (graph);
}
