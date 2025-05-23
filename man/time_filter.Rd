% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_filter.R
\name{time_filter}
\alias{time_filter}
\title{Create a time snapshot from an OSM history file}
\usage{
time_filter(
  input_path,
  timestamp,
  output_path,
  overwrite = FALSE,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE,
  verbose = FALSE,
  progress = FALSE
)
}
\arguments{
\item{input_path}{A string. The path to the OSM history file to be filtered.
Please see \link{file_formats} for a list of supported file formats.}

\item{timestamp}{A string, Date, or POSIXct, or a vector of two such values.
The timestamp(s) in ISO 8601 format ('yyyy-mm-ddThh:mm:ssZ', e.g., "2015-01-01T00:00:00Z") or in ISO date
format ('yyyy-mm-dd', e.g., "2015-01-01") representing the point in time for the snapshot or the start and
end of a time interval.
If Date or POSIXct objects are provided, they will be converted automatically to the required string format.
\strong{Note:} If two timestamps are provided and the first is later than the second, they will be swapped (with a warning)
so that the interval is correct.}

\item{output_path}{A string. The path to the file where the output should be written to.
Please see \link{file_formats} for a list of supported file formats.}

\item{overwrite}{A logical. Whether existing files should be overwritten by the output. Defaults to \code{FALSE}.}

\item{echo_cmd}{A logical. Whether to print the Osmium command generated by the function call to the screen.
Defaults to \code{FALSE}.}

\item{echo}{A logical. Whether to print the standard output and error generated by the Osmium call to the screen.
Defaults to \code{TRUE}.}

\item{spinner}{A logical. Whether to show a reassuring spinner while the Osmium call is being executed.
Defaults to \code{TRUE}.}

\item{verbose}{A logical. Whether to display detailed information on the running command. Defaults to \code{FALSE}.}

\item{progress}{A logical. Whether to display a progress bar while running
the command. Defaults to \code{FALSE}.}
}
\value{
The normalized path to the output file.
}
\description{
Filters objects from an OSM history file based on a specified point or interval in time.
The output is a normal OSM data file containing the data as it would have looked at the specified time (UTC)
or over the given interval.
}
\examples{
\dontshow{if (identical(tolower(Sys.getenv("NOT_CRAN")), "true")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
history_path <- system.file("extdata/cur.osm.pbf", package = "rosmium")

# Single timestamp example (using ISO date format):
output_single <- time_filter(
  input_path = history_path,
  timestamp = "2015-01-01",
  output_path = tempfile(fileext = ".osm.pbf")
)

# Two timestamps example (mixing full ISO timestamp and ISO date format):
output_interval <- time_filter(
  input_path = history_path,
  timestamp = c("2015-01-01", "2015-06-01T00:00:00Z"),
  output_path = tempfile(fileext = ".osh.pbf")
)
\dontshow{\}) # examplesIf}
}
