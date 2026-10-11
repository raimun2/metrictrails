#' GNSS streams of two activities from the Strava API
#'
#' Readings of two activities recorded in Santiago, Chile (a trail run on
#' Manquehue hill and a cross-country ride), as returned by the Strava API
#' streams endpoint, already following the metrictrails naming convention.
#'
#' @format A data frame with 4148 rows and 13 variables:
#' \describe{
#'   \item{lon}{longitude, WGS 84 degrees}
#'   \item{lat}{latitude, WGS 84 degrees}
#'   \item{ele}{elevation reported by Strava, metres}
#'   \item{time}{seconds since the start of the activity}
#'   \item{cadence}{cadence, steps or revolutions per minute}
#'   \item{distance}{cumulative distance computed by Strava, kilometres}
#'   \item{grade_smooth}{terrain grade smoothed by Strava, percent}
#'   \item{heartrate}{heart rate, beats per minute}
#'   \item{moving}{whether Strava considers the athlete moving}
#'   \item{temp}{temperature, degrees Celsius}
#'   \item{velocity_smooth}{speed smoothed by Strava, km/h}
#'   \item{id}{activity identifier, links to [strava_metadata]}
#'   \item{type}{activity type (`"Run"` or `"Ride"`)}
#' }
#' @source \url{https://developers.strava.com/docs/reference/}
"strava_streams"

#' Metadata of the activities in strava_streams
#'
#' Summary of the two activities in [strava_streams] as returned by the Strava
#' API activities endpoint, one row per activity.
#'
#' @format A data frame with 2 rows and 61 variables.
#'
#' @details Main variables: `id` (activity identifier, links to
#'   [strava_streams]), `name`, `type`, `start_date` and `start_date_local`
#'   (ISO 8601), `distance` (km), `elapsed_time` and `moving_time` (s),
#'   `total_elevation_gain`, `elev_high` and `elev_low` (m), `average_speed`
#'   and `max_speed`, and `map.summary_polyline` (encoded polyline). See the
#'   Strava API reference for the remaining variables.
#' @source \url{https://developers.strava.com/docs/reference/}
"strava_metadata"
