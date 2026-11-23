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



test_that("remote backend creates and reuses disk cache", {

  skip_on_cran()

  withr::local_envvar(
    ILOSTAT_BACKEND = "remote"
  )

  t_dir <- tempfile("rilostat-remote-")

  on.exit(
    unlink(t_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  expect_false(dir.exists(t_dir))

  x <- get_ilostat(
    "UNE_2UNE_SEX_AGE_NB_A",
    cache = TRUE,
    cache_dir = t_dir,
    quiet = TRUE
  )

  expect_true(dir.exists(t_dir))
  expect_s3_class(x, "data.frame")
  expect_gt(nrow(x), 0)

  cache_files <- list.files(
    t_dir,
    pattern = "\\.rds$",
    full.names = TRUE
  )

  expect_gt(length(cache_files), 0)

  # Verify reuse of the existing disk cache.
  y <- get_ilostat(
    "UNE_2UNE_SEX_AGE_NB_A",
    cache = TRUE,
    cache_update = FALSE,
    cache_dir = t_dir,
    quiet = TRUE
  )

  expect_equal(x, y)
})


test_that("local backend reads RDS without creating cache", {

  skip_on_cran()

  skip_if(
    !nzchar(Sys.getenv("ILOSTAT_DATA_ROOT")),
    "Local ILOSTAT data root is not configured"
  )

  withr::local_envvar(
    ILOSTAT_BACKEND = "local"
  )

  root <- Sys.getenv("ILOSTAT_DATA_ROOT")

  skip_if_not(
    dir.exists(root),
    "Local ILOSTAT data root is inaccessible"
  )

  id <- "UNE_TUNE_SEX_AGE_NB_A"

  skip_if_not(
    file.exists(file.path(root, "indicator", paste0(id, ".rds"))),
    "Local test dataset is unavailable"
  )

  t_dir <- tempfile("rilostat-local-")

  on.exit(
    unlink(t_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  expect_false(dir.exists(t_dir))

  x <- get_ilostat(
    id,
    cache = TRUE,
    cache_dir = t_dir,
    quiet = TRUE
  )

  expect_s3_class(x, "data.frame")
  expect_gt(nrow(x), 0)

  # Local backend must not create a second disk cache.
  expect_false(dir.exists(t_dir))
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