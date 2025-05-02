#' Check referential integrity of an OSM data file
#'
#' This is a wrapper for [osmium check-refs](https://docs.osmcode.org/osmium/latest/osmium-check-refs.html).
#' Checks that all nodes referenced by ways (and optionally relations) are present.
#'
#' @param input_path String. Path to an existing OSM data file (not history or change file).
#' @param show_ids Logical(1). If `TRUE`, print all missing IDs to stdout. Defaults to `FALSE`.
#' @param check_relations Logical(1). If `TRUE`, also check relations’ references. Defaults to `FALSE`.
#' @param input_format Optional string. Force input format (e.g. `"osm"`, `"pbf"`). If `NULL`, autodetect.
#' @param echo_cmd Logical(1). Whether to print the generated Osmium command. Defaults to `FALSE`.
#' @param echo Logical(1). Whether to print Osmium’s stdout/stderr. Defaults to `FALSE`.
#' @param spinner Logical(1). Whether to show a spinner during execution. Defaults to `TRUE`.
#' @param verbose Logical(1). Whether to pass `--verbose` for detailed logging. Defaults to `FALSE`.
#' @param progress Logical(1). Whether to show a progress bar (`--progress` / `--no-progress`). Defaults to `FALSE`.
#'
#' @return A character scalar: the plain-text output from `osmium check-refs`.
#'
#' @export
osm_check_refs <- function(
  input_path,
  show_ids = FALSE,
  check_relations = FALSE,
  input_format = NULL,
  echo_cmd = FALSE,
  echo = FALSE,
  spinner = TRUE,
  verbose = FALSE,
  progress = FALSE
) {
  assert_osmium_is_installed()

  ## Validate inputs
  checkmate::assert_string(input_path)
  checkmate::assert_file_exists(input_path, access = "r")
  checkmate::assert_logical(show_ids, len = 1)
  checkmate::assert_logical(check_relations, len = 1)
  if (!is.null(input_format)) checkmate::assert_string(input_format)
  checkmate::assert_logical(echo_cmd, len = 1)
  checkmate::assert_logical(echo, len = 1)
  checkmate::assert_logical(spinner, len = 1)
  checkmate::assert_logical(verbose, len = 1)
  checkmate::assert_logical(progress, len = 1)

  ## Build flags
  ids_flag <- if (show_ids) "--show-ids" else character()
  rels_flag <- if (check_relations) "--check-relations" else character()
  input_fmt <- if (!is.null(input_format))
    paste0("--input-format=", input_format) else character()
  verbose_flag <- if (verbose) "--verbose" else character()
  progress_flag <- if (progress) "--progress" else "--no-progress"

  ## Assemble and run
  args <- c(
    "check-refs",
    ids_flag,
    rels_flag,
    input_fmt,
    verbose_flag,
    progress_flag,
    input_path
  )

  logs <- processx::run(
    "osmium",
    args,
    echo = echo,
    spinner = spinner,
    echo_cmd = echo_cmd
  )

  ## Return the output text
  logs$stdout
}
