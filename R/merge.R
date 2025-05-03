# File: R/merge.R

#' Merge several sorted OSM files
#'
#' This is a wrapper for [osmium merge](https://docs.osmcode.org/osmium/latest/osmium-merge.html).
#' Merges multiple sorted OSM files into a single output, removing duplicate objects.
#' Optionally checks referential integrity of each input before merging.
#'
#' @param input_paths A character vector of paths to one or more existing, sorted OSM files.
#' @param output_path A string. Path to the file where the merged result will be written.
#' @param check_integrity A string. One of "no", "simple", or "full". "no" (default) skips integrity checks;
#'   "simple" runs [`osm_check_refs()`] without relation checks;
#'   "full" runs [`osm_check_refs()`] with relation checks enabled.
#' @param input_format A string or `NULL`. Force input format (e.g. "osm", "pbf"). If `NULL`, autodetect.
#' @param output_format A string or `NULL`. Force output format (e.g. "osm", "pbf"). If `NULL`, autodetect.
#' @param fsync A logical. If `TRUE`, call `fsync` after writing to force flushing buffers. Defaults to `FALSE`.
#' @param generator A string or `NULL`. Name/version for the `generator` header. Defaults to Osmium’s own.
#' @param overwrite A logical. If `TRUE`, allow overwriting an existing `output_path`. Defaults to `FALSE`.
#' @param output_header A named character vector of additional header options. Each name is the header key,
#'   each value is the header value. A zero‐length value (`""`) writes `OPTION!`. Defaults to `NULL`.
#' @param echo_cmd A logical. Whether to print the generated Osmium command. Defaults to `FALSE`.
#' @param echo A logical. Whether to print Osmium’s stdout/stderr. Defaults to `TRUE`.
#' @param spinner A logical. Whether to show a spinner during execution. Defaults to `TRUE`.
#' @param verbose A logical. Whether to pass `--verbose` to Osmium for detailed logging. Defaults to `FALSE`.
#' @param progress A logical. Whether to pass `--progress` / `--no-progress`. Defaults to `FALSE`.
#'
#' @return Invisibly returns the normalized path to `output_path`.
#' @keywords internal
#' @export
osm_merge <- function(
  input_paths,
  output_path,
  check_integrity = "no",
  input_format = NULL,
  output_format = NULL,
  fsync = FALSE,
  generator = NULL,
  overwrite = FALSE,
  output_header = NULL,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE,
  verbose = FALSE,
  progress = FALSE
) {
  assert_osmium_is_installed()

  ## Validate inputs
  checkmate::assert_character(input_paths, any.missing = FALSE, min.len = 1)
  for (p in input_paths) {
    checkmate::assert_file_exists(p, access = "r")
  }
  checkmate::assert_string(output_path)

  ## check_integrity must be one of no/simple/full
  check_integrity <- match.arg(check_integrity, c("no", "simple", "full"))
  do_check <- check_integrity != "no"
  check_rel_flag <- check_integrity == "full"

  if (!is.null(input_format)) checkmate::assert_string(input_format)
  if (!is.null(output_format)) checkmate::assert_string(output_format)
  checkmate::assert_logical(fsync, len = 1)
  if (!is.null(generator)) checkmate::assert_string(generator)
  checkmate::assert_logical(overwrite, len = 1)
  if (!is.null(output_header)) {
    checkmate::assert_named(output_header, type = "unique")
    checkmate::assert_character(output_header)
  }
  checkmate::assert_logical(echo_cmd, len = 1)
  checkmate::assert_logical(echo, len = 1)
  checkmate::assert_logical(spinner, len = 1)
  checkmate::assert_logical(verbose, len = 1)
  checkmate::assert_logical(progress, len = 1)

  ## Prepare common flags
  verbose_flag <- if (verbose) "--verbose" else character()
  progress_flag <- if (progress) "--progress" else "--no-progress"
  input_fmt_flag <- if (!is.null(input_format))
    paste0("--input-format=", input_format) else character()
  output_fmt_flag <- if (!is.null(output_format))
    paste0("--output-format=", output_format) else character()

  ## Optionally check referential integrity
  if (do_check) {
    for (p in input_paths) {
      print(paste0("Checking integrity of ", p, "..."))
      osm_check_refs(
        input_path = p,
        show_ids = FALSE,
        check_relations = check_rel_flag,
        input_format = input_format,
        echo_cmd = echo_cmd,
        echo = echo,
        spinner = spinner,
        verbose = verbose,
        progress = progress
      )
    }
  }

  ## Build merge flags
  fsync_flag <- if (fsync) "--fsync" else character()
  gen_flag <- if (!is.null(generator)) paste0("--generator=", generator) else
    character()
  overwrite_flag <- if (overwrite) "--overwrite" else character()
  header_flags <- if (!is.null(output_header)) {
    unname(vapply(
      names(output_header),
      function(nm) {
        val <- output_header[[nm]]
        hdr <- if (nzchar(val)) paste0(nm, "=", val) else paste0(nm, "!")
        paste0("--output-header=", hdr)
      },
      character(1)
    ))
  } else character()
  output_flag <- paste0("--output=", output_path)

  ## run merge
  args <- c(
    "merge",
    input_paths,
    input_fmt_flag,
    output_fmt_flag,
    fsync_flag,
    gen_flag,
    output_flag,
    overwrite_flag,
    header_flags,
    verbose_flag,
    progress_flag
  )

  logs <- processx::run(
    "osmium",
    args,
    echo = echo,
    spinner = spinner,
    echo_cmd = echo_cmd
  )

  invisible(normalizePath(output_path))
}
