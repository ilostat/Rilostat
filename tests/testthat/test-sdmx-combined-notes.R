# testthat regression tests for combined SDMX note labels.
testthat::test_that("combined notes resolve without changing ordinary codes", {
  dict <- data.frame(
    code = c("R1:2382", "T2:85", "T3:104"),
    label = c("Repository: OECD", "Age coverage - minimum age: 16 years old",
              "Age coverage - maximum age: 74 years old"),
    label_short = c("OECD", "Min 16", "Max 74"),
    stringsAsFactors = FALSE
  )
  x <- c("R1:2382_T2:85_T3:104", "R1:2382", NA_character_,
         "R1:2382_UNKNOWN:9")
  expect <- c(paste(dict$label, collapse = " | "), dict$label[[1]],
              NA_character_, "Repository: OECD | UNKNOWN:9")
  testthat::expect_identical(
    .ilostat_sdmx_lookup_values(x, dict, "label", "NOTE_SOURCE"), expect
  )
  testthat::expect_identical(
    .ilostat_sdmx_lookup_values(x[[1]], dict, "label_short", "NOTE_SOURCE"),
    "OECD | Min 16 | Max 74"
  )
  testthat::expect_identical(
    .ilostat_sdmx_lookup_values("AGE_YTHADULT_YGE15", dict, "label", "AGE"),
    "AGE_YTHADULT_YGE15"
  )
})

testthat::test_that("SDMX combined source notes agree with native labels", {
  testthat::skip_on_cran()
  id <- "UNE_TUNE_SEX_AGE_NB_A"
  note <- "R1:2382_T2:85_T3:104"
  native <- get_ilostat(id, type = "both", filters = list(note_source = note))
  sdmx <- get_ilostat_sdmx(id, type = "both", filters = list(NOTE_SOURCE = note))
  testthat::expect_gt(nrow(sdmx), 0L)
  testthat::expect_setequal(
    unique(sdmx$NOTE_SOURCE_LABEL), unique(native$note_source.label)
  )
  testthat::expect_true(all(sdmx$NOTE_SOURCE == note))
})
