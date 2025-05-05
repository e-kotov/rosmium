#' Concatenate OSM files with optional integrity checks and sorting
#'
#' @description{
#' Concatenates all input files and writes the result to the output file. The data is not sorted in any way but strictly copied from input to output. This is a wrapper for **[`osmium cat`](https://docs.osmcode.org/osmium/latest/osmium-cat.html)** that can optionally (1) run referential-integrity pre-flight checks with [`osm_check_refs()`], (2) force-sort each input with [`osm_sort()`], and then (3) perform the actual concatenation.
#'
#' ## 1. Referential-integrity checks
#' Set `check_integrity` to
#' | value          | what is checked                           | implementation                               |
#' |----------------|-------------------------------------------|----------------------------------------------|
#' | "no" (def.)   | nothing                                   | -                                            |
#' | "simple"      | *nodes referenced by ways* are present    | `osm_check_refs(check_relations = FALSE)`    |
#' | "full"        | additionally checks *all refs in relations* | `osm_check_refs(check_relations = TRUE)`     |
#'
#' If any check returns exit-status `1` (missing refs) **and**
#' `stop_on_integrity_fail = TRUE`, the function **aborts** before concatenating.
#'
#' ## 2. Optional force-sort step
#' If `sort_before_cat = TRUE`, each input file is first piped through
#' [`osm_sort()`] (strategy = "simple") into a temporary file.  Temporary
#' files are cleaned up at the end of the R session.  If you need permanent,
#' sorted copies, call [`osm_sort()`] yourself and set the desired output path.
#'
#' ## 3. Memory usage during concatenation
#' `osmium cat` itself uses minimal buffers, but the optional sorting step
#' can dominate RAM usage (up to ~10x on-disk size).
#' }
#'
#' @param input_paths Character vector of one or more existing OSM files.
#' @param output_path String. Path to write the concatenated result.
#' @param object_type Character vector of types to include: one or more of
#'   "node", "way", "relation", "changeset".  Defaults to all types.
#' @param clean Character vector of attributes to clean (set to zero/empty):
#'   one or more of "version", "timestamp", "changeset",
#'   "uid", "user".  Defaults to none.
#' @param buffer_data Logical(1). If `TRUE`, read all inputs into memory
#'   before writing.  Defaults to `FALSE`.
#' @param check_integrity One of:
#'   - "no" (default): skip integrity checks,
#'   - "simple": run `osm_check_refs(check_relations = FALSE)`,
#'   - "full": run `osm_check_refs(check_relations = TRUE)`.
#' @param stop_on_integrity_fail Logical. If `TRUE`, abort on failed
#'   integrity check.  Defaults to `FALSE`.
#' @param sort_before_cat Logical. If `TRUE`, force-sort each input with
#'   [`osm_sort()`] before concatenation.  Defaults to `FALSE`.
#' @param input_format Optional string to force the input format
#'   (e.g. "osm", "pbf").  `NULL` to autodetect.
#' @param output_format Optional string to force the output format
#'   (e.g. "osm", "pbf").  `NULL` to autodetect.
#' @param generator Optional string for the **`--generator`** header tag.
#' @param overwrite Logical(1). If `TRUE`, allow overwriting an existing
#'   `output_path`.  Defaults to `FALSE`.
#' @param fsync Logical(1). If `TRUE`, call **`--fsync`** after writing.
#'   Defaults to `FALSE`.
#' @param output_header Named character vector of additional header options:
#'   `OPTION = "value"` or `OPTION = ""` to replicate via `OPTION!`.
#' @param echo_cmd Logical. Whether to print the generated Osmium command.
#'   Defaults to `FALSE`.
#' @param echo Logical. Whether to print stdout and stderr of the Osmium call.
#'   Defaults to `TRUE`.
#' @param spinner Logical. Whether to show a spinner while running.
#'   Defaults to `TRUE`.
#' @param verbose Logical. Whether to display detailed command info.
#'   Defaults to `FALSE`.
#' @param progress Logical. Whether to show a progress bar.
#'   Defaults to `FALSE`.
#'
#' @return Invisibly returns the normalized `output_path`.
#'
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' pbf <- system.file("extdata/cur.osm.pbf", package = "rosmium")
#'
#' # 1. get header bbox (an sf::st_bbox)
#' orig_bbox <- osm_get_bbox(pbf)
#'
#' # 2. split bbox in two halves by longitude
#' midx <- (orig_bbox["xmin"] + orig_bbox["xmax"]) / 2
#'
#' bbox1 <- sf::st_bbox(
#'   c(
#'     xmin = orig_bbox[["xmin"]],
#'     ymin = orig_bbox[["ymin"]],
#'     xmax = unname(midx),
#'     ymax = orig_bbox[["ymax"]]
#'   ),
#'   crs = sf::st_crs(orig_bbox)
#' )
#'
#' bbox2 <- sf::st_bbox(
#'   c(
#'     xmin = unname(midx),
#'     ymin = orig_bbox[["ymin"]],
#'     xmax = orig_bbox[["xmax"]],
#'     ymax = orig_bbox[["ymax"]]
#'   ),
#'   crs = sf::st_crs(orig_bbox)
#' )
#'
#' # 3. extract two halves
#' f1 <- extract(
#'   pbf,
#'   bbox1,
#'   tempfile(fileext = ".osm.pbf"),
#'   overwrite = TRUE,
#'   echo = FALSE,
#'   spinner = FALSE
#' )
#' f2 <- extract(
#'   pbf,
#'   bbox2,
#'   tempfile(fileext = ".osm.pbf"),
#'   overwrite = TRUE,
#'   echo = FALSE,
#'   spinner = FALSE
#' )
#'
#' # 4. concatenate
#' cat_out <- tempfile(fileext = ".osm.pbf")
#' osm_cat(c(f1, f2), cat_out, overwrite = TRUE, echo = FALSE, spinner = FALSE)
#'
#' # 5. sort, in case sorting was altered during concatenation
#' sorted_out <- tempfile(fileext = ".osm.pbf")
#' osm_sort(cat_out, sorted_out, overwrite = TRUE, echo = FALSE, spinner = FALSE)
#'
#' # 6. compute final bbox
#' final_bbox <- osm_get_bbox(sorted_out, calculate = TRUE)
#'
#' @export
osm_cat <- function(
  input_paths,
  output_path,
  object_type = c("node", "way", "relation", "changeset"),
  clean = character(),
  buffer_data = FALSE,
  check_integrity = c("no", "simple", "full"),
  stop_on_integrity_fail = FALSE,
  sort_before_cat = FALSE,
  input_format = NULL,
  output_format = NULL,
  generator = NULL,
  overwrite = FALSE,
  fsync = FALSE,
  output_header = NULL,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE,
  verbose = FALSE,
  progress = FALSE
) {
  assert_osmium_is_installed()

  ## ---- argument validation ----
  checkmate::assert_character(input_paths, any.missing = FALSE, min.len = 1)
  for (p in input_paths) {
    checkmate::assert_file_exists(p, access = "r")
  }
  checkmate::assert_string(output_path)
  checkmate::assert_character(object_type, any.missing = FALSE)
  checkmate::assert_subset(
    object_type,
    choices = c("node", "way", "relation", "changeset")
  )
  checkmate::assert_character(clean)
  checkmate::assert_subset(
    clean,
    choices = c("version", "timestamp", "changeset", "uid", "user"),
    empty.ok = TRUE
  )
  checkmate::assert_flag(buffer_data)
  check_integrity <- match.arg(check_integrity)
  do_check <- check_integrity != "no"
  check_rels <- identical(check_integrity, "full")
  checkmate::assert_flag(stop_on_integrity_fail)
  checkmate::assert_flag(sort_before_cat)
  if (!is.null(input_format)) checkmate::assert_string(input_format)
  if (!is.null(output_format)) checkmate::assert_string(output_format)
  if (!is.null(generator)) checkmate::assert_string(generator)
  checkmate::assert_flag(overwrite)
  checkmate::assert_flag(fsync)
  if (!is.null(output_header)) {
    checkmate::assert_named(output_header, type = "unique")
    checkmate::assert_character(output_header, any.missing = FALSE)
  }
  checkmate::assert_flag(echo_cmd)
  checkmate::assert_flag(echo)
  checkmate::assert_flag(spinner)
  checkmate::assert_flag(verbose)
  checkmate::assert_flag(progress)

  ## ---- 1 referential-integrity checks ----
  if (do_check) {
    for (p in input_paths) {
      if (echo) cat("Checking integrity of", p, "...\n")
      log <- osm_check_refs(
        input_path = p,
        show_ids = FALSE,
        check_relations = check_rels,
        input_format = input_format,
        echo_cmd = echo_cmd,
        echo = echo,
        spinner = spinner,
        verbose = verbose,
        progress = progress
      )
      if (
        isTRUE(stop_on_integrity_fail) &&
          !is.null(log$status_code) &&
          log$status_code == 1L
      ) {
        stop(
          sprintf("Referential integrity check failed for '%s'.", p),
          call. = FALSE
        )
      }
    }
  }

  ## ---- 2 optional force-sort ----
  if (sort_before_cat) {
    if (echo) cat("Sorting input files before cat (strategy = 'simple')...\n")
    sorted_paths <- character(length(input_paths))
    for (i in seq_along(input_paths)) {
      in_file <- input_paths[i]
      ext <- tools::file_ext(in_file)
      tmp_file <- tempfile(fileext = if (nzchar(ext)) paste0(".", ext) else "")
      osm_sort(
        input_paths = in_file,
        output_path = tmp_file,
        strategy = "simple",
        input_format = input_format,
        output_format = output_format,
        fsync = fsync,
        generator = generator,
        overwrite = TRUE,
        echo_cmd = echo_cmd,
        echo = echo,
        spinner = spinner,
        verbose = verbose,
        progress = progress
      )
      sorted_paths[i] <- tmp_file
    }
    old <- sorted_paths
    on.exit(unlink(old, recursive = FALSE, force = TRUE), add = TRUE)
    input_paths <- sorted_paths
  }

  ## ---- build command-line flags ----
  clean_flags <- if (length(clean) > 0) paste0("--clean=", clean) else
    character()
  type_flags <- paste0("--object-type=", object_type)
  buffer_flag <- if (buffer_data) "--buffer-data" else character()
  input_fmt_flag <- if (!is.null(input_format))
    paste0("--input-format=", input_format) else character()
  output_fmt_flag <- if (!is.null(output_format))
    paste0("--output-format=", output_format) else character()
  generator_flag <- if (!is.null(generator))
    paste0("--generator=", generator) else character()
  overwrite_flag <- if (overwrite) "--overwrite" else character()
  fsync_flag <- if (fsync) "--fsync" else character()
  header_flags <- if (!is.null(output_header)) {
    unname(vapply(
      names(output_header),
      function(nm) {
        val <- output_header[[nm]]
        tag <- if (nzchar(val)) paste0(nm, "=", val) else paste0(nm, "!")
        paste0("--output-header=", tag)
      },
      character(1)
    ))
  } else character()
  verbose_flag <- if (verbose) "--verbose" else character()
  progress_flag <- if (progress) "--progress" else "--no-progress"
  output_flag <- paste0("--output=", output_path)

  args <- c(
    "cat",
    input_paths,
    clean_flags,
    type_flags,
    buffer_flag,
    input_fmt_flag,
    output_fmt_flag,
    generator_flag,
    output_flag,
    overwrite_flag,
    fsync_flag,
    header_flags,
    verbose_flag,
    progress_flag
  )

  ## ---- run osmium cat ----
  processx::run(
    "osmium",
    args,
    echo = echo,
    spinner = spinner,
    echo_cmd = echo_cmd
  )

  invisible(normalizePath(output_path))
}
