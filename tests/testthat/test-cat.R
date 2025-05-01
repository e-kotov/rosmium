testthat::skip_on_cran()

test_that("osm_cat + osm_sort yield correct final bbox", {
  pbf <- system.file("extdata/cur.osm.pbf", package = "rosmium")

  # original header bbox
  orig_bbox <- osm_get_bbox(pbf)
  expect_s3_class(orig_bbox, "bbox")

  # split mid‐longitude
  midx <- (orig_bbox[["xmin"]] + orig_bbox[["xmax"]]) / 2

  bbox1 <- sf::st_bbox(
    c(
      xmin = orig_bbox[["xmin"]],
      ymin = orig_bbox[["ymin"]],
      xmax = midx,
      ymax = orig_bbox[["ymax"]]
    ),
    crs = sf::st_crs(orig_bbox)
  )
  bbox2 <- sf::st_bbox(
    c(
      xmin = midx,
      ymin = orig_bbox[["ymin"]],
      xmax = orig_bbox[["xmax"]],
      ymax = orig_bbox[["ymax"]]
    ),
    crs = sf::st_crs(orig_bbox)
  )

  # two extracts
  f1 <- extract(
    pbf,
    bbox1,
    tempfile(fileext = ".osm.pbf"),
    overwrite = TRUE,
    echo = FALSE,
    spinner = FALSE
  )
  f2 <- extract(
    pbf,
    bbox2,
    tempfile(fileext = ".osm.pbf"),
    overwrite = TRUE,
    echo = FALSE,
    spinner = FALSE
  )
  expect_true(file.exists(f1))
  expect_true(file.exists(f2))

  # concatenate
  cat_file <- tempfile(fileext = ".osm.pbf")
  osm_cat(c(f1, f2), cat_file, overwrite = TRUE, echo = FALSE, spinner = FALSE)
  expect_true(file.exists(cat_file))

  # sort
  sorted_file <- tempfile(fileext = ".osm.pbf")
  osm_sort(
    cat_file,
    sorted_file,
    overwrite = TRUE,
    echo = FALSE,
    spinner = FALSE
  )
  expect_true(file.exists(sorted_file))

  # recompute final bbox
  final_bbox <- osm_get_bbox(sorted_file, calculate = TRUE)
  expect_s3_class(final_bbox, "bbox")

  # 1) it should strictly expand the original envelope
  expect_lt(final_bbox[["xmin"]], orig_bbox[["xmin"]])
  expect_lt(final_bbox[["ymin"]], orig_bbox[["ymin"]])
  expect_gt(final_bbox[["xmax"]], orig_bbox[["xmax"]])
  expect_gt(final_bbox[["ymax"]], orig_bbox[["ymax"]])

  # 2) and match the exact values we observed
  expected <- c(
    -49.30187, # xmin
    -25.48331, # ymin
    -49.20784, # xmax
    -25.39819 # ymax
  )
  expect_equal(
    as.numeric(final_bbox),
    expected,
    tolerance = 1e-6
  )
})
