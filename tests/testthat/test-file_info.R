testthat::skip_on_cran()

test_that("osm_file_info returns correct structure for cur.osm.pbf", {
  pbf <- system.file("extdata/cur.osm.pbf", package = "rosmium")
  expect_true(file.exists(pbf))

  # ---- output="list" ----
  info <- osm_file_info(pbf, output = "list")
  expect_type(info, "list")
  expect_named(info, c("file", "header"))

  # file sub-list
  f <- info$file
  expect_named(f, c("name", "format", "compression", "size"))
  expect_true(grepl("cur\\.osm\\.pbf$", f$name))
  expect_equal(f$format, "PBF")
  expect_equal(f$compression, "none")
  expect_true(is.numeric(f$size) && f$size > 0)

  # header sub-list
  h <- info$header
  expect_named(h, c("boxes", "with_history", "option"))
  # boxes must be a 1×4 matrix
  expect_true(is.matrix(h$boxes))
  expect_equal(dim(h$boxes), c(1, 4))
  expect_equal(
    as.numeric(h$boxes),
    c(-49.2889, -25.4413, -49.2509, -25.4197),
    tolerance = 1e-6
  )
  expect_false(h$with_history)

  opts <- h$option
  expect_named(opts, c("generator", "pbf_dense_nodes"))
  expect_true(nzchar(opts$generator))
  expect_true(opts$pbf_dense_nodes %in% c("true", "false"))

  # ---- output="json" ----
  j <- osm_file_info(pbf, output = "json")
  expect_true(RcppSimdJson::is_valid_json(j))

  # ---- output="text" ----
  t <- osm_file_info(pbf, output = "text")
  expect_type(t, "character")
  expect_true(grepl("Bounding boxes", t))
})

test_that("osm_file_info errors on missing file", {
  expect_error(
    osm_file_info("no_such_file.osm.pbf"),
    "File does not exist",
    ignore.case = TRUE
  )
})


test_that("osm_file_info rejects bad output arg", {
  pbf <- system.file("extdata/cur.osm.pbf", package = "rosmium")
  expect_error(
    osm_file_info(pbf, output = "xml"),
    "should be one of"
  )
})

test_that("osm_file_info get=boxes returns text with only generator", {
  pbf <- system.file("extdata/cur.osm.pbf", package = "rosmium")
  # get generator in text mode
  out <- osm_file_info(pbf, get = "boxes", output = "text")
  expect_true(grepl("boxes=", out))
  expect_false(grepl("generator", out))
})

test_that("osm_file_info crc=TRUE adds crc field", {
  pbf <- system.file("extdata/cur.osm.pbf", package = "rosmium")
  j <- osm_file_info(pbf, output = "list", crc = TRUE, extended = TRUE)
  expect_true("crc32" %in% names(j$data))
  expect_true(is.character(j$data$crc32))
})


test_that("osm_file_info extended=TRUE returns data element", {
  pbf <- system.file("extdata/cur.osm.pbf", package = "rosmium")
  info_ext <- osm_file_info(pbf, output = "list", extended = TRUE)
  expect_true(is.list(info_ext$data))
  expect_true(all(c("bbox", "timestamp", "count") %in% names(info_ext$data)))
})

test_that("osm_file_info warns if object_type used without extended", {
  pbf <- system.file("extdata/cur.osm.pbf", package = "rosmium")
  expect_warning(
    osm_file_info(pbf, output = "list", object_type = c("node", "way")),
    "`object_type` only applies when `extended = TRUE`"
  )
})

test_that("osm_file_info respects progress argument", {
  pbf <- system.file("extdata/cur.osm.pbf", package = "rosmium")
  # capture the command via echo_cmd so we can inspect it
  expect_output(
    osm_file_info(pbf, output = "text", echo_cmd = TRUE, progress = TRUE),
    "--progress"
  )
  expect_output(
    osm_file_info(pbf, output = "text", echo_cmd = TRUE, progress = FALSE),
    "--no-progress"
  )
})

test_that("osm_file_info_variables returns a list of variable names", {
  vars <- osm_file_info_variables()
  expect_type(vars, "character")
  expect_true(all(c("file.name", "file.size") %in% vars))
})
