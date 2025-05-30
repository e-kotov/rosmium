% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_refs.R
\name{osm_check_refs}
\alias{osm_check_refs}
\title{Check referential integrity of an OSM data file}
\usage{
osm_check_refs(
  input_path,
  show_ids = FALSE,
  check_relations = FALSE,
  input_format = NULL,
  echo_cmd = FALSE,
  echo = FALSE,
  spinner = TRUE,
  verbose = TRUE,
  progress = FALSE
)
}
\arguments{
\item{input_path}{String. Path to an existing OSM data file (not history or change file).}

\item{show_ids}{A logical. If \code{TRUE}, print all missing IDs to stdout. Defaults to \code{FALSE}.}

\item{check_relations}{A logical. If \code{TRUE}, also check relations references. Defaults to \code{FALSE}, which only checks if any \code{nodes} in `ways`` are missing.}

\item{input_format}{Optional string. Force input format (e.g. "osm", "pbf"). If \code{NULL}, autodetect.}

\item{echo_cmd}{A logical. Whether to print the generated Osmium command. Defaults to \code{FALSE}.}

\item{echo}{A logical. Whether to print Osmium's stdout/stderr. Defaults to \code{FALSE} to avoid duplication because the parsed output is printed to R console by default.}

\item{spinner}{A logical. Whether to show a spinner during execution. Defaults to \code{TRUE}.}

\item{verbose}{A logical. Whether to pass \code{--verbose} to Osmium for detailed logging. Defaults to \code{TRUE}.}

\item{progress}{A logical. Whether to pass \code{--progress} / \code{--no-progress}. Defaults to \code{FALSE}.}
}
\value{
A list of class \code{osm_check_refs_log}, containing:
\itemize{
\item \code{status_code}: \code{integer}. Exit status from Osmium.
\item \code{raw}: list of raw text captured from Osmium's stdout and stderr.
\item \code{metadata}: list of the original \code{verbose} and \code{check_relations} settings.
\item \code{summary}: data.frame of counts (present and missing references).
}

When printed, provides a formatted summary about the check and possible
warnings and errors.
}
\description{
This is a wrapper for \href{https://docs.osmcode.org/osmium/latest/osmium-check-refs.html}{osmium check-refs}.
Checks that all nodes referenced by ways (and optionally relations) are present.
}
