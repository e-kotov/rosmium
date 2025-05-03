test_that("osm_check_refs works with check_relations = FALSE", {
  pbf_path <- system.file("extdata/cur.osm.pbf", package = "rosmium")
  x <- osm_check_refs(
    pbf_path,
    verbose = TRUE,
    echo = FALSE,
    check_relations = FALSE
  )

  ## structure
  expect_s3_class(x, "osm_check_refs_log")
  expect_equal(x$status_code, 0L)
  expect_false(x$metadata$request_check_rels)
  expect_true(x$metadata$request_verbose)
  expect_equal(x$metadata$request_input_file, pbf_path)

  ## summary counts
  df <- x$summary
  expect_equal(df$count[df$description == "nodes"], 40185L)
  expect_equal(df$count[df$description == "ways"], 6884L)
  expect_equal(df$count[df$description == "relations"], 430L)
  expect_equal(df$count[df$description == "nodes_in_ways"], 0L)
  expect_equal(df$count[df$description == "nodes_in_relations"], 0L)
  expect_equal(df$count[df$description == "ways_in_relations"], 0L)
  expect_equal(df$count[df$description == "relations_in_relations"], 0L)

  ## raw memory line
  expect_true(grepl("Memory used for indexes: 1344 MBytes", x$raw$memory_line))

  ## printed output
  out <- capture.output(print(x))
  # file name
  expect_true(grepl("^File checked: .*/cur\\.osm\\.pbf$", out[1]))
  # version block present
  expect_true(any(grepl("^osmium version", out)))
  # summary line
  expect_true(any(grepl(
    "^There are 40185 nodes, 6884 ways, and 430 relations in this file\\.$",
    out
  )))
  # always nodes_in_ways
  expect_true(any(grepl("^Nodes in ways missing: 0$", out)))
  # no relations lines
  expect_false(any(grepl("in relations missing:", out)))
  # memory usage printed
  expect_true(any(grepl("^Memory used for indexes: 1344 MBytes", out)))
  # note about relations not checked
  expect_true(any(grepl(
    "relations were not checked",
    out
  )))
  # status code line
  expect_true(any(grepl(
    "^Osmium status code: 0",
    out
  )))
})

test_that("osm_check_refs works with check_relations = TRUE", {
  pbf_path <- system.file("extdata/cur.osm.pbf", package = "rosmium")
  x <- osm_check_refs(
    pbf_path,
    verbose = TRUE,
    echo = FALSE,
    check_relations = TRUE
  )

  ## structure
  expect_s3_class(x, "osm_check_refs_log")
  expect_equal(x$status_code, 1L)
  expect_true(x$metadata$request_check_rels)

  ## summary counts
  df <- x$summary
  expect_equal(df$count[df$description == "nodes"], 40185L)
  expect_equal(df$count[df$description == "ways"], 6884L)
  expect_equal(df$count[df$description == "relations"], 430L)
  expect_equal(df$count[df$description == "nodes_in_ways"], 0L)
  expect_equal(df$count[df$description == "nodes_in_relations"], 433L)
  expect_equal(df$count[df$description == "ways_in_relations"], 4258L)
  expect_equal(df$count[df$description == "relations_in_relations"], 0L)

  ## printed output
  out <- capture.output(print(x))
  # nodes_in_relations line
  expect_true(any(grepl("^Nodes in relations missing: 433$", out)))
  expect_true(any(grepl("^Ways in relations missing: 4258$", out)))
  expect_true(any(grepl("^Relations in relations missing: 0$", out)))
  # status code 1 meaning
  expect_true(any(grepl(
    "^Osmium status code: 1",
    out
  )))
})
