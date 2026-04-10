#define PY_SSIZE_T_CLEAN
#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#include <Python.h>
#include <numpy/arrayobject.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

struct python_state {
    int initialized;
    PyObject *module;
    PyObject *nn;
};

static struct python_state state = {.initialized = 0};

int nn_initialize () {
    if (state.initialized) {
        return 1;
    }

    Py_Initialize ();
    _import_array ();

    state.module = PyImport_ImportModule ("sklearn.neighbors");
    if (state.module == NULL) {
        fprintf (stderr, "Cannot import sklearn\n");
        PyErr_Print ();
        goto cleanup;
    }

    state.nn = PyObject_GetAttrString (state.module, "NearestNeighbors");
    if (state.nn == NULL || !PyCallable_Check (state.nn)) {
        fprintf (stderr, "Cannot find NearestNeighbors\n");
        PyErr_Print ();
        Py_DECREF (state.module);
        goto cleanup;
    }

    state.initialized = 1;
    return 1;

cleanup:
    Py_Finalize ();
    return 0;
}

void nn_deinitialize () {
    if (state.initialized) {
        Py_DECREF (state.nn);
        Py_DECREF (state.module);
        Py_Finalize ();
        state.initialized = 0;
    }
}

void nn_find_closest (float *set1, size_t len1,
                      float *set2, size_t len2,
                      size_t nfeatures,
                      int njobs,
                      void (*callback) (const float    *dists,
                                        const uint64_t *indices,
                                        size_t          len)) {
    PyObject *set1_numpy  = NULL;
    PyObject *set2_numpy  = NULL;
    PyObject *nn          = NULL;
    PyObject *fit         = NULL;
    PyObject *kneighbors  = NULL;
    PyObject *result      = NULL;

    if (!state.initialized && !nn_initialize ()) {
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

    PyObject *args = PyDict_New ();
    PyObject *tuple = PyTuple_New (0);
    PyObject *n_neighbors = PyLong_FromLong (2);
    PyObject *algo = PyUnicode_FromString ("brute");
    PyObject *jobs = PyLong_FromLong (njobs);
    PyDict_SetItemString (args, "n_neighbors", n_neighbors);
    PyDict_SetItemString (args, "algorithm", algo);
    PyDict_SetItemString (args, "n_jobs", jobs);
    nn = PyObject_Call (state.nn, tuple, args);
    // Reference is not stolen when setting a value of a dict
    Py_XDECREF (args);
    Py_XDECREF (tuple);
    Py_XDECREF (n_neighbors);
    Py_XDECREF (algo);
    Py_XDECREF (jobs);

    if (nn == NULL) {
        fprintf (stderr, "Failed to call NearestNeighbors\n");
        PyErr_Print();
        goto cleanup;
    }

    fit = PyObject_GetAttrString (nn, "fit");
    if (fit == NULL) {
        fprintf (stderr, "No fit method\n");
        PyErr_Print();
        goto cleanup;
    }

    kneighbors = PyObject_GetAttrString (nn, "kneighbors");
    if (kneighbors == NULL) {
        fprintf (stderr, "No kneighbors method\n");
        PyErr_Print();
        goto cleanup;
    }

    args = PyTuple_New (1);
    PyTuple_SetItem (args, 0, set2_numpy);
    // Reference is stolen
    set2_numpy = NULL;
    result = PyObject_Call (fit, args, NULL);
    Py_XDECREF (args);
    if (result == NULL) {
        fprintf (stderr, "Unable to call fit method\n");
        PyErr_Print();
        goto cleanup;
    }
    Py_XDECREF (result);

    args = PyTuple_New (1);
    PyTuple_SetItem (args, 0, set1_numpy);
    // Reference is stolen
    set1_numpy = NULL;
    result = PyObject_Call (kneighbors, args, NULL);
    Py_XDECREF (args);
    if (result == NULL || !PyTuple_Check (result)) {
        fprintf (stderr, "Unable to make a query\n");
        PyErr_Print();
        goto cleanup;
    }

    // References are borrwed
    PyObject *dists   = PyTuple_GetItem (result, 0);
    PyObject *indices = PyTuple_GetItem (result, 1);

    // TODO: Check dtype
    if (!PyArray_Check (indices) || !PyArray_Check (dists)) {
        fprintf (stderr, "Something weird in the result\n");
        PyErr_Print();
        goto cleanup;
    }

    npy_intp len = PyArray_DIM ((PyArrayObject*)indices, 0);
    const uint64_t *indices_ptr = PyArray_GETPTR1 ((PyArrayObject*)indices, 0);
    const float    *dists_ptr   = PyArray_GETPTR1 ((PyArrayObject*)dists,   0);
    callback (dists_ptr, indices_ptr, len);

cleanup:
    if (set1_numpy != NULL) {
        Py_XDECREF (set1_numpy);
    }

    if (set2_numpy != NULL) {
        Py_XDECREF (set2_numpy);
    }

    if (result != NULL) {
        Py_XDECREF (result);
    }

    if (fit != NULL) {
        Py_XDECREF (fit);
    }

    if (kneighbors != NULL) {
        Py_XDECREF (kneighbors);
    }

    if (nn != NULL) {
        Py_XDECREF (nn);
    }
}
