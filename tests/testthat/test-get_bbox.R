testthat::skip_on_cran()
test_that("osm_get_bbox reads header bbox correctly (with tolerance)", {
  pbf <- system.file("extdata/cur.osm.pbf", package = "rosmium")
  expect_true(file.exists(pbf))

  # default: read header bbox
  bbox <- osm_get_bbox(pbf)
  expect_s3_class(bbox, "bbox")

  # expected values (one decimal precision)
  expected_1dec <- c(
    -49.3, # xmin
    -25.4, # ymin
    -49.3, # xmax
    -25.4 # ymax
  )

  # compare with tolerance
  expect_equal(
    as.numeric(bbox),
    expected_1dec,
    tolerance = 0.1
  )

  # return_bbox_list = TRUE: a list of length 1
  lst <- osm_get_bbox(pbf, return_bbox_list = TRUE)
  expect_type(lst, "list")
  expect_length(lst, 1)
  expect_s3_class(lst[[1]], "bbox")
  expect_equal(
    as.numeric(lst[[1]]),
    expected_1dec,
    tolerance = 0.1
  )

  # calculate = TRUE on a header-rich file should still return header
  bbox2 <- osm_get_bbox(pbf, calculate = TRUE)
  expect_equal(
    as.numeric(bbox2),
    expected_1dec,
    tolerance = 0.1
  )
})


pbf <- system.file("extdata/cur.osm.pbf", package = "rosmium")

test_that("osm_get_bbox errors on missing file", {
  expect_error(
    osm_get_bbox("no_such_file.osm"),
    "File does not exist" # from assert_osmium_is_installed or file exists
  )
})

test_that("osm_get_bbox returns single bbox with correct values", {
  expect_true(file.exists(pbf))

  b1 <- osm_get_bbox(pbf)
  expect_s3_class(b1, "bbox")
  # Known header values from cur.osm.pbf
  expect_equal(
    as.numeric(b1),
    c(-49.28, -25.44, -49.25, -25.41),
    tolerance = 1e-3
  )
})

test_that("osm_get_bbox(return_bbox_list = TRUE) returns a list of bboxes", {
  L <- osm_get_bbox(pbf, return_bbox_list = TRUE)
  expect_type(L, "list")
  expect_length(L, 1)
  expect_s3_class(L[[1]], "bbox")
  expect_equal(unname(L[[1]]), unname(osm_get_bbox(pbf)), tolerance = 1e-6)
})

test_that("osm_get_bbox(calculate = TRUE) computes same bbox when header exists", {
  b_hdr <- osm_get_bbox(pbf)
  b_calc <- osm_get_bbox(pbf, calculate = TRUE)
  expect_equal(unname(b_calc), unname(b_hdr), tolerance = 1e-6)
})


# Parameter validation
test_that("osm_get_bbox rejects non-logical inputs", {
  expect_error(osm_get_bbox(pbf, calculate = "yes"), "Assertion on")
  expect_error(osm_get_bbox(pbf, return_bbox_list = NA), "Assertion on")
  expect_error(osm_get_bbox(pbf, echo_cmd = 1), "Assertion on")
  expect_error(osm_get_bbox(pbf, spinner = "no"), "Assertion on")
  expect_error(osm_get_bbox(pbf, verbose = NULL), "Assertion on")
  expect_error(osm_get_bbox(pbf, progress = "maybe"), "Assertion on")
})
