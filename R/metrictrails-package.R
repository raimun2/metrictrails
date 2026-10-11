#' metrictrails: Processing of GNSS Streams from Sports Watches
#'
#' @description
#' Tools to read, clean and standardise GNSS streams recorded by sports
#' watches and similar devices, with a focus on trail and mountain running.
#'
#' A *stream* is a sequence of consecutive readings with `lon` and `lat` and,
#' optionally, `ele`, `timestamp`, `time` and other variables (heart rate,
#' cadence, ...). The recommended pipeline is:
#'
#' 1. [read_stream()]: read a GPX file.
#' 2. [uniform_stream()]: apply the naming convention.
#' 3. [sanitize_stream()]: smooth the coordinates and flag implausible
#'    readings.
#' 4. [resample_stream()]: resample to a uniform spatial resolution derived
#'    from the Nyquist criterion ([resolution_stream()]).
#' 5. [ele_correction()]: replace elevation with values from a Digital
#'    Elevation Model ([get_dem()], [resample_dem()]).
#'
#' Once the stream is tidy:
#'
#' * [differential_stream()], [summarise_stream()], [fractal_stream()] and
#'   [tobler_stream()] compute derived measures;
#' * [project_stream()] adds UTM coordinates and [locate_stream()] projects
#'   readings on a reference (linear referencing);
#' * [compare_stream()] and [detect_stream()] compare a stream with another
#'   stream or route;
#' * [match_stream()] aligns a stream along a reference trail it is known to
#'   follow;
#' * [write_stream()] writes the result to GPX.
#'
#' @keywords internal
"_PACKAGE"
