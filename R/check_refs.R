#' Check referential integrity of an OSM data file
#'
#' This is a wrapper for [osmium check-refs](https://docs.osmcode.org/osmium/latest/osmium-check-refs.html).
#' Checks that all nodes referenced by ways (and optionally relations) are present.
#'
#' @param input_path String. Path to an existing OSM data file (not history or change file).
#' @param show_ids A logical. If `TRUE`, print all missing IDs to stdout. Defaults to `FALSE`.
#' @param check_relations A logical. If `TRUE`, also check relations references. Defaults to `FALSE`.
#' @param input_format Optional string. Force input format (e.g. "osm", "pbf"). If `NULL`, autodetect.
#' @param echo_cmd A logical. Whether to print the generated Osmium command. Defaults to `FALSE`.
#' @param echo A logical. Whether to print Osmium's stdout/stderr. Defaults to `TRUE`.
#' @param spinner A logical. Whether to show a spinner during execution. Defaults to `TRUE`.
#' @param verbose A logical. Whether to pass `--verbose` to Osmium for detailed logging. Defaults to `TRUE`.
#' @param progress A logical. Whether to pass `--progress` / `--no-progress`. Defaults to `FALSE`.
#'
#' @return A character scalar: the plain-text output from `osmium check-refs`, invisibly returned if successful.
#' @export
osm_check_refs <- function(
  input_path,
  show_ids = FALSE,
  check_relations = FALSE,
  input_format = NULL,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE,
  verbose = TRUE,
  progress = FALSE
) {
  assert_osmium_is_installed()
  checkmate::assert_string(input_path)
  checkmate::assert_file_exists(input_path, access = "r")
  checkmate::assert_flag(show_ids)
  checkmate::assert_flag(check_relations)
  if (!is.null(input_format)) checkmate::assert_string(input_format)
  checkmate::assert_flag(echo_cmd)
  checkmate::assert_flag(echo)
  checkmate::assert_flag(spinner)
  checkmate::assert_flag(verbose)
  checkmate::assert_flag(progress)

  args <- c(
    "check-refs",
    if (show_ids) "--show-ids",
    if (check_relations) "--check-relations",
    if (!is.null(input_format)) paste0("--input-format=", input_format),
    if (verbose) "--verbose",
    if (progress) "--progress" else "--no-progress",
    input_path
  )

  logs <- processx::run(
    "osmium",
    args,
    echo = echo,
    spinner = spinner,
    echo_cmd = echo_cmd,
    error_on_status = FALSE
  )

  parsed <- parse_osm_check_refs_log(
    log_text = logs$stderr,
    status_code = logs$status,
    verbose = verbose,
    request_check_rels = check_relations,
    request_verbose = verbose,
    request_input_file = input_path
  )
  invisible(parsed)
}

#' Parse Osmium `check-refs` command output
#'
#' This function takes the raw stderr text from `osmium check-refs` and an
#' optional status code, and returns a structured object of class
#' `osm_check_refs_log` with:
#'   - status_code: integer exit status
#'   - raw: list of captured header/footer lines
#'   - metadata: version, option details, plus the original `verbose` and `check_relations` settings
#'   - summary: a data.frame of counts (present and missing references)
#'
#' @param log_text Character scalar: stderr output from `osmium check-refs`
#' @param status_code Integer or NULL: exit status from the command
#' @param verbose Logical: was `osmium` run with --verbose? Defaults to FALSE.
#' @param request_check_rels Logical: the original `check_relations` setting
#' @param request_verbose Logical: the original `verbose` setting
#' @return An object of class `osm_check_refs_log`
#' @keywords internal
#'
parse_osm_check_refs_log <- function(
  log_text,
  status_code = NULL,
  verbose = FALSE,
  request_check_rels = FALSE,
  request_verbose = FALSE,
  request_input_file = NULL
) {
  lines <- strsplit(log_text, "\n", fixed = TRUE)[[1]]
  lines <- sub("\r$", "", lines)
  lines <- lines[nzchar(lines)]
  lines <- grep("^\\[.*%.*\\]", lines, invert = TRUE, value = TRUE)

  clean <- function(x) trimws(sub("^\\[.*?\\]\\s*", "", x))
  cl <- vapply(lines, clean, "")

  raw <- list(
    version_lines = cl[grepl("^(osmium version|libosmium version)", cl)],
    summary_line = cl[grepl(
      "^There are [0-9]+ nodes, [0-9]+ ways, and [0-9]+ relations",
      cl
    )],
    missing_lines = cl[grepl("missing: *[0-9]+", cl)],
    memory_line = cl[grepl("^Memory used for indexes:", cl)]
  )

  # totals
  m_tot <- regexec(
    "([0-9]+) nodes, ([0-9]+) ways, and ([0-9]+) relations",
    raw$summary_line
  )
  nums <- as.integer(regmatches(raw$summary_line, m_tot)[[1]][2:4])
  names(nums) <- c("nodes", "ways", "relations")
  totals <- nums

  # missing
  patterns <- c(
    nodes_in_ways = "^Nodes.*in ways.*missing: *([0-9]+)",
    nodes_in_relations = "^Nodes.*in relations.*missing: *([0-9]+)",
    ways_in_relations = "^Ways.*in relations.*missing: *([0-9]+)",
    relations_in_relations = "^Relations.*in relations.*missing: *([0-9]+)"
  )
  missing <- vapply(
    patterns,
    function(p) {
      m <- grep(p, raw$missing_lines, value = TRUE)
      if (!length(m)) return(0L)
      as.integer(sub(p, "\\1", m[1]))
    },
    integer(1)
  )

  df <- data.frame(
    description = c(names(totals), names(missing)),
    count = c(as.integer(totals), missing),
    stringsAsFactors = FALSE
  )

  metadata <- list(
    request_check_rels = request_check_rels,
    request_verbose = request_verbose,
    request_input_file = request_input_file
  )

  res <- list(
    status_code = status_code,
    raw = raw,
    metadata = metadata,
    summary = df
  )
  class(res) <- "osm_check_refs_log"
  res
}

#' Print method for `osm_check_refs_log` objects
#'
#' Displays parsed results in a format resembling `osmium check-refs` native output.
#' @param x An `osm_check_refs_log` object
#' @param ... Additional args (ignored)
#' @export
#' @method print osm_check_refs_log
print.osm_check_refs_log <- function(x, ...) {
  md <- x$metadata
  df <- x$summary
  raw <- x$raw

  # file name
  if (!is.null(md$request_input_file)) {
    cat(sprintf("File checked: %s\n\n", md$request_input_file))
  }

  # version info
  if (length(raw$version_lines)) {
    cat(paste(raw$version_lines, collapse = "\n"), "\n")
  }

  # summary
  tot <- df$count[df$description %in% c("nodes", "ways", "relations")]
  cat(sprintf(
    "\nThere are %d nodes, %d ways, and %d relations in this file.\n",
    tot[1],
    tot[2],
    tot[3]
  ))

  # missing in ways
  nw <- df$count[df$description == "nodes_in_ways"]
  cat(sprintf("Nodes in ways missing: %d\n", nw))

  # missing in relations, if requested
  if (isTRUE(md$request_check_rels)) {
    nr <- df$count[df$description == "nodes_in_relations"]
    wr <- df$count[df$description == "ways_in_relations"]
    rr <- df$count[df$description == "relations_in_relations"]
    cat(sprintf("Nodes in relations missing: %d\n", nr))
    cat(sprintf("Ways in relations missing: %d\n", wr))
    cat(sprintf("Relations in relations missing: %d\n", rr))
  }

  # memory usage
  if (length(raw$memory_line)) {
    cat(raw$memory_line, "\n")
  }

  # note if relations were not checked
  if (!isTRUE(md$request_check_rels)) {
    cat(
      "\nNote: relations were not checked. Relation-level missing counts are omitted.\n You can run `osm_check_refs()` with `check_relations=TRUE` to check relations.\n"
    )
  }

  # Osmium exit status (with human-readable meaning)
  if (!is.null(x$status_code)) {
    code <- x$status_code
    meaning <- switch(
      as.character(code),
      "0" = "OK = All references satisfied.",
      "1" = "Error processing data or some references were not satisfied.\nThis is normal for Geofabrik extracts when you run with `check_relations=TRUE`.",
      "2" = "Problem with command line arguments.",
      "unexpected status code"
    )
    cat(sprintf("\nOsmium status code: %d\n", code))
    cat(sprintf("Osmium status meaning: %s\n", meaning))
  }

  invisible(x)
}
