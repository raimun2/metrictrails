# metrictrails (development version)

* `summarise_stream()` gains `sinuosity_local`, the mean ratio between path length and chord over windows of `window` metres (new arguments `window` and `closed_tol`).
* `summarise_stream()`: `sinuosity` is now `NA` for closed paths (first and last readings closer than `closed_tol`); previously it was `Inf` for exact loops and very large for nearly closed ones.
* Documentation no longer refers to GPStream.

# metrictrails 1.0.0

* First CRAN release.
