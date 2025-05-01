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
