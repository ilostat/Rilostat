test_that("get_ilostat returns expected data structure", {

  skip_on_cran()

  x <- get_ilostat(
    "UNE_2UNE_SEX_AGE_NB_A"
  )

  expect_s3_class(
    x,
    "data.frame"
  )

  expect_true(
    all(
      c("time", "obs_value") %in% names(x)
    )
  )

  expect_type(
    x$time,
    "character"
  )

  expect_type(
    x$obs_value,
    "double"
  )
})


test_that("cache directory is created and works", {

  skip_on_cran()

  t_dir <- tempfile("rilostat-")

  on.exit(
    unlink(
      t_dir,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  expect_false(
    dir.exists(t_dir)
  )

  x <- get_ilostat(
    "UNE_2UNE_SEX_AGE_NB_A",
    cache_dir = t_dir
  )

  expect_true(
    dir.exists(t_dir)
  )

  expect_s3_class(
    x,
    "data.frame"
  )
})


test_that("get_ilostat_toc search finds SDG datasets", {

  skip_on_cran()

  x <- get_ilostat_toc(
    search = "SDG"
  )

  expect_s3_class(
    x,
    "data.frame"
  )

  expect_gt(
    nrow(x),
    0L
  )

  expect_true(
    "database" %in% names(x)
  )

  expect_true(
    "ILOSDG" %in% x$database
  )
})


test_that("get_ilostat supports label filters", {

  skip_on_cran()

  x <- get_ilostat(
    "UNE_2UNE_SEX_AGE_NB_A",
    type = "label",
    filters = list(
      ref_area.label = "France"
    )
  )

  expect_s3_class(
    x,
    "data.frame"
  )

  expect_gt(
    nrow(x),
    0L
  )
})


test_that("get_ilostat_dic rejects an unknown dictionary", {

  skip_on_cran()

  expect_error(
    get_ilostat_dic("na_item")
  )
})