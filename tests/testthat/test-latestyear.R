
test_that("latestyear is available and defaults to FALSE", {

  expect_true(
    "latestyear" %in% names(formals(get_ilostat))
  )

  expect_true(
    "latestyear" %in% names(formals(get_ilostat_dat))
  )

  withr::local_options(ilostat_latestyear = FALSE)

  expect_identical(
    eval(formals(get_ilostat)$latestyear),
    FALSE
  )
})


test_that("latestyear preserves all observations when FALSE", {

  withr::local_envvar(ILOSTAT_BACKEND = "local")

  root <- Sys.getenv("ILOSTAT_DATA_ROOT", "")

  skip_if(
    !nzchar(root) || !dir.exists(root),
    "Local ILOSTAT storage unavailable"
  )

  id <- "UNE_TUNE_SEX_AGE_NB_A"

  all_data <- get_ilostat(
    id,
    latestyear = FALSE,
    filters = list(ref_area = "AUT"),
    quiet = TRUE
  )

  latest <- get_ilostat(
    id,
    latestyear = TRUE,
    filters = list(ref_area = "AUT"),
    quiet = TRUE
  )

  expect_gt(nrow(all_data), 0)
  expect_gt(nrow(latest), 0)
  expect_lte(nrow(latest), nrow(all_data))

  expected_time <- as.character(
    dplyr::first(all_data$time)
  )

  expect_true(
    all(as.character(latest$time) == expected_time)
  )

  expect_equal(
    latest,
    dplyr::filter(
      all_data,
      as.character(.data$time) == expected_time
    )
  )
})


test_that("latestyear is applied after time filtering", {

  withr::local_envvar(ILOSTAT_BACKEND = "local")

  root <- Sys.getenv("ILOSTAT_DATA_ROOT", "")

  skip_if(
    !nzchar(root) || !dir.exists(root),
    "Local ILOSTAT storage unavailable"
  )

  id <- "UNE_TUNE_SEX_AGE_NB_A"

  x <- get_ilostat(
    id,
    latestyear = TRUE,
    filters = list(
      ref_area = "AUT",
      timefrom = 2020,
      timeto = 2023
    ),
    quiet = TRUE
  )

  expect_gt(nrow(x), 0)

  expect_true(
    all(as.numeric(x$time) >= 2020)
  )

  expect_true(
    all(as.numeric(x$time) <= 2023)
  )

  expect_length(unique(x$time), 1)
})


test_that("latestyear preserves classification observations", {

  withr::local_envvar(ILOSTAT_BACKEND = "local")

  root <- Sys.getenv("ILOSTAT_DATA_ROOT", "")

  skip_if(
    !nzchar(root) || !dir.exists(root),
    "Local ILOSTAT storage unavailable"
  )

  id <- "UNE_TUNE_SEX_AGE_NB_A"

  x <- get_ilostat(
    id,
    latestyear = TRUE,
    filters = list(ref_area = "AUT"),
    quiet = TRUE
  )

  expect_gt(nrow(x), 0)
  expect_true("sex" %in% names(x))
  expect_true("classif1" %in% names(x))

  expect_length(unique(x$time), 1)
})


test_that("latestyear supports multilingual labels", {

  withr::local_envvar(ILOSTAT_BACKEND = "local")

  root <- Sys.getenv("ILOSTAT_DATA_ROOT", "")

  skip_if(
    !nzchar(root) || !dir.exists(root),
    "Local ILOSTAT storage unavailable"
  )

  for (lang in c("en", "fr", "es")) {

    x <- get_ilostat(
      "UNE_TUNE_SEX_AGE_NB_A",
      type = "both",
      lang = lang,
      latestyear = TRUE,
      filters = list(ref_area = "AUT"),
      quiet = TRUE
    )

    expect_gt(nrow(x), 0)
    expect_true("ref_area.label" %in% names(x))
    expect_length(unique(x$time), 1)
  }
})


test_that("latestyear local backend creates no cache", {

  withr::local_envvar(ILOSTAT_BACKEND = "local")

  root <- Sys.getenv("ILOSTAT_DATA_ROOT", "")

  skip_if(
    !nzchar(root) || !dir.exists(root),
    "Local ILOSTAT storage unavailable"
  )

  cache_dir <- tempfile("rilostat-latestyear-")

  x <- get_ilostat(
    "UNE_TUNE_SEX_AGE_NB_A",
    latestyear = TRUE,
    cache = TRUE,
    cache_dir = cache_dir,
    quiet = TRUE
  )

  expect_gt(nrow(x), 0)
  expect_false(dir.exists(cache_dir))
})
