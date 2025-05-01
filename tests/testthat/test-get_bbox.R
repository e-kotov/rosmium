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
