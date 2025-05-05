testthat::skip_on_cran()
test_that("osm_sort sorts a small PBF (simple strategy)", {
  pbf <- system.file("extdata/cur.osm.pbf", package = "rosmium")
  out <- tempfile(fileext = ".osm.pbf")

  expect_invisible(
    res <- osm_sort(
      input_paths = pbf,
      output_path = out,
      overwrite = TRUE,
      echo = FALSE,
      spinner = FALSE,
      progress = FALSE
    )
  )

  expect_true(file.exists(res))
  expect_gt(file.size(res), 0)
})

test_that("osm_sort works with multipass strategy", {
  pbf <- system.file("extdata/cur.osm.pbf", package = "rosmium")
  out <- tempfile(fileext = ".osm.pbf")

  expect_invisible(
    osm_sort(
      input_paths = pbf,
      output_path = out,
      strategy = "multipass",
      overwrite = TRUE,
      echo = FALSE,
      spinner = FALSE,
      progress = FALSE
    )
  )

  expect_true(file.exists(out))
})

test_that("osm_sort errors on non-existent input", {
  fake <- tempfile(fileext = ".pbf")
  out <- tempfile(fileext = ".osm.pbf")
  expect_error(osm_sort(fake, out), "File does not exist")
})
