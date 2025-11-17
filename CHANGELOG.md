# Changelog

## Version 0.5

* Improvement: Search for matches in the PCA space of the reference image which
  is of lower dimensionality than the descriptor space.
* Bug fix: Produce a comprehensible error if the number of descriptors is too
  small.
* Bug fix: Avoid an infinite loop if the number of matches is less then a
  minimal number of matches to find a transform matrix.

## Version 0.4

* Incompatible change: The default parameter of `match-descriptors` is changed
  to `1.2`.
* Improvement: `apply-transform` is parallelized.

## Version 0.3

* Improvement: Apply PCA analysis to descriptors in order to save space in the
  database.
* Incompatible change: Bruteforce matching of descriptors was removed.
* Incompatible change: Another database format. You must to manually remove
  older database file(s).

## Version 0.2

* Incompatible change: the output image is stored as an array of octets
  ('uint8') in the cli tool and `apply-transform` returns an array of type
  `(simple-array (unsigned-byte 8) 3)`.
* Incompatible change: `:min-inliers` default in `transform` is changed to `0.6`
  which improves the transformation finding algorithm in a number of cases.
* Improvement: Descriptors are stored in the database so they can be accessed
  later if they are needed. Currently, the database needs to be removed manually
  if it is not needed.
* Improvement: Add an option `-t` to the CLI tool which selects a number of
  working threads.
* Improvement: Add an option `-w` to the CLI tool which selects a working area.
* Improvement: The CLI tool now supports raw "format".

## Version 0.1

Initial version. Basic image alignment using 3D SIFT + pynndescent + RANSAC
