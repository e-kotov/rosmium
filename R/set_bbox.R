#' Apply or Set Bounding‐Box Header on an OSM File
#'
#' Writes a new PBF file (or overwrites it) with its header bounding box set
#' to either a user‐supplied \code{sf::st_bbox} or—by default—the computed data
#' bounding box.  Computing may require reading the entire file and can be slow!
#'
#' @param input_path     String. Path to the existing OSM file.
#' @param output_path    String. Path where the new file will be written.
#' @param bbox           An \code{sf::st_bbox} to use.  If \code{NULL},
#'                       the bounding box is computed via \code{osm_get_bbox()}.
#' @param calculate      Logical(1). If \code{TRUE}, allow computing from data
#'                       when no header is present; otherwise error if missing.
#'                       Defaults to \code{TRUE}.
#' @param overwrite      Logical(1). If \code{TRUE}, allow overwriting
#'                       \code{output_path}.  Defaults to \code{FALSE}.
#' @param echo_cmd A logical. Whether to print the Osmium command generated by the function call to the screen.
#'   Defaults to `FALSE`.
#' @param echo A logical. Whether to print the standard output and error generated by the Osmium call to the screen.
#'   Defaults to `TRUE`.
#' @param spinner A logical. Whether to show a reassuring spinner while the Osmium call is being executed.
#'   Defaults to `TRUE`.
#' @param verbose A logical. Whether to display detailed information on the running command. Defaults to `FALSE`.
#' @param progress A logical. Whether to display a progress bar while running
#'   the command. Defaults to `FALSE`.
#'
#' @return Invisibly returns the normalized path to \code{output_path}.
#'
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' pbf <- system.file("extdata/cur.osm.pbf", package = "rosmium")
#'
#' # Compute true bbox and write to a new file
#' out <- tempfile(fileext = ".osm.pbf")
#' osm_set_bbox(pbf, out, calculate = TRUE, overwrite = TRUE)
#'
#' # Supply a custom bbox
#' library(sf)
#' custom <- st_bbox(
#'   c(xmin = -49.3, ymin = -25.45, xmax = -49.24, ymax = -25.41),
#'   crs = st_crs(4326)
#' )
#' out2 <- tempfile(fileext = ".osm.pbf")
#' osm_set_bbox(pbf, out2, bbox = custom, overwrite = TRUE)
#'
#' @export
osm_set_bbox <- function(
  input_path,
  output_path,
  bbox = NULL,
  calculate = TRUE,
  overwrite = FALSE,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE,
  verbose = FALSE,
  progress = FALSE
) {
  assert_osmium_is_installed()
  checkmate::assert_file_exists(input_path)
  checkmate::assert_string(output_path)
  checkmate::assert_logical(calculate, len = 1, any.missing = FALSE)
  checkmate::assert_logical(overwrite, len = 1, any.missing = FALSE)
  checkmate::assert_logical(echo_cmd, len = 1, any.missing = FALSE)
  checkmate::assert_logical(echo, len = 1, any.missing = FALSE)
  checkmate::assert_logical(spinner, len = 1, any.missing = FALSE)
  checkmate::assert_logical(verbose, len = 1, any.missing = FALSE)
  checkmate::assert_logical(progress, len = 1, any.missing = FALSE)

  # Ensure output directory exists
  out_dir <- dirname(output_path)
  if (!dir.exists(out_dir)) {
    stop("Directory does not exist: ", out_dir)
  }

  # Overwrite guard
  if (file.exists(output_path) && !overwrite) {
    stop(
      "Output file '",
      output_path,
      "' already exists;",
      " to overwrite, set overwrite = TRUE."
    )
  }

  # Determine which bbox to use
  if (!is.null(bbox)) {
    if (!inherits(bbox, "bbox")) {
      stop("`bbox` must be an sf::st_bbox object or NULL.")
    }
    b <- bbox
  } else {
    b <- osm_get_bbox(
      input_path,
      calculate = calculate,
      echo_cmd = echo_cmd,
      echo = echo,
      spinner = spinner,
      verbose = verbose,
      progress = progress
    )
  }

  # Format for osmium
  bbox_arg <- paste(
    c(b["xmin"], b["ymin"], b["xmax"], b["ymax"]),
    collapse = ","
  )

  # Build command
  args <- c(
    "extract",
    input_path,
    paste0("--bbox=", bbox_arg),
    "--set-bounds",
    if (verbose) "--verbose" else NULL,
    if (progress) "--progress" else "--no-progress",
    "--overwrite",
    "-o",
    output_path
  )

  # Run osmium
  processx::run(
    "osmium",
    args,
    echo = echo,
    spinner = spinner,
    echo_cmd = echo_cmd
  )

  invisible(normalizePath(output_path))
}
