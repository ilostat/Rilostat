test_that("SDMX metadata defines dynamic components", {

  meta <- .ilostat_sdmx_get_meta(
    "UNE_TUNE_SEX_AGE_NB_A"
  )

  components <- .ilostat_sdmx_components(meta)

  expect_true(
    all(c(
      "REF_AREA",
      "SOURCE",
      "FREQ",
      "MEASURE",
      "SEX",
      "AGE",
      "TIME_PERIOD",
      "OBS_VALUE",
      "OBS_STATUS",
      "UNIT_MEASURE",
      "UNIT_MULT",
      "NOTE_SOURCE",
      "NOTE_INDICATOR",
      "NOTE_CLASSIF",
      "BEST_SOURCE"
    ) %in% components$component_id)
  )

  age <- components[
    components$component_id == "AGE",
    ,
    drop = FALSE
  ]

  expect_equal(age$native_name, "classif1")
  expect_equal(age$codelist, "cl_age")
})


test_that("SDMX filters are translated to native ILOSTAT dimensions", {

  meta <- .ilostat_sdmx_get_meta(
    "UNE_TUNE_SEX_AGE_NB_A"
  )

  components <- .ilostat_sdmx_components(meta)

  filters <- .ilostat_sdmx_translate_filters(
    list(
      REF_AREA = "FRA",
      SEX = "SEX_F",
      AGE = "AGE_YTHADULT_YGE15"
    ),
    components
  )

  expect_named(
    filters,
    c("ref_area", "sex", "classif1")
  )

  expect_equal(filters$ref_area, "FRA")
  expect_equal(filters$sex, "SEX_F")
  expect_equal(
    filters$classif1,
    "AGE_YTHADULT_YGE15"
  )
})


test_that("structure returns the SDMX-oriented component model", {

  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "structure"
  )

  expect_s3_class(x, "data.frame")

  expect_named(
    x,
    c(
      "COMPONENT_ID",
      "ROLE",
      "POSITION",
      "REPRESENTATION",
      "COMPONENT_NAME",
      "CODELIST"
    )
  )

  expect_equal(nrow(x), 15L)

  expect_equal(
    x$COMPONENT_ID,
    c(
      "REF_AREA",
      "SOURCE",
      "FREQ",
      "MEASURE",
      "SEX",
      "AGE",
      "TIME_PERIOD",
      "OBS_VALUE",
      "OBS_STATUS",
      "UNIT_MEASURE",
      "UNIT_MULT",
      "NOTE_SOURCE",
      "NOTE_INDICATOR",
      "NOTE_CLASSIF",
      "BEST_SOURCE"
    )
  )

  age <- x[x$COMPONENT_ID == "AGE", ]

  expect_equal(age$CODELIST, "cl_age")
})


test_that("codelist normalizes dynamic AGE classification", {

  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "codelist",
    component = "AGE"
  )

  expect_named(
    x,
    c("AGE", "AGE_LABEL")
  )

  expect_true(
    "AGE_YTHADULT_YGE15" %in% x$AGE
  )

  i <- match(
    "AGE_YTHADULT_YGE15",
    x$AGE
  )

  expect_equal(
    x$AGE_LABEL[[i]],
    "Age (Youth, adults): 15+"
  )
})


test_that("codelist supports dataset SDMX constants", {

  freq <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "codelist",
    component = "FREQ"
  )

  expect_named(freq, c("FREQ", "FREQ_LABEL"))
  expect_equal(freq$FREQ, "A")
  expect_equal(freq$FREQ_LABEL, "Annual")

  measure <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "codelist",
    component = "MEASURE"
  )

  expect_equal(
    measure$MEASURE,
    "UNE_TUNE_NB"
  )

  unit <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "codelist",
    component = "UNIT_MEASURE"
  )

  expect_equal(unit$UNIT_MEASURE, "PS")

  mult <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "codelist",
    component = "UNIT_MULT"
  )

  expect_equal(mult$UNIT_MULT, "3")
})


test_that("codelist supports observation status", {

  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "codelist",
    component = "OBS_STATUS"
  )

  expect_named(
    x,
    c("OBS_STATUS", "OBS_STATUS_LABEL")
  )

  expect_true(all(c("B", "U") %in% x$OBS_STATUS))
})


test_that("data returns SDMX component names", {

  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "data",
    filters = list(
      REF_AREA = "FRA",
      SEX = "SEX_F",
      AGE = "AGE_YTHADULT_YGE15"
    )
  )

  expect_s3_class(x, "data.frame")

  expect_true(
    all(c(
      "REF_AREA",
      "SOURCE",
      "FREQ",
      "MEASURE",
      "SEX",
      "AGE",
      "TIME_PERIOD",
      "OBS_VALUE",
      "OBS_STATUS",
      "UNIT_MEASURE",
      "UNIT_MULT",
      "NOTE_SOURCE",
      "NOTE_INDICATOR",
      "NOTE_CLASSIF"
    ) %in% names(x))
  )

  # Native implementation detail must not leak.
  expect_false("classif1" %in% names(x))
  expect_false("indicator" %in% names(x))

  expect_true(all(x$REF_AREA == "FRA"))
  expect_true(all(x$SEX == "SEX_F"))
  expect_true(
    all(x$AGE == "AGE_YTHADULT_YGE15")
  )

  expect_true(all(x$FREQ == "A"))
  expect_true(all(x$MEASURE == "UNE_TUNE_NB"))
  expect_true(all(x$UNIT_MEASURE == "PS"))
  expect_true(all(x$UNIT_MULT == "3"))

  # best_source = "yes" filters to best sources and the native
  # BEST_SOURCE column is therefore not returned.
  expect_false("BEST_SOURCE" %in% names(x))
})


test_that("best_source no retains BEST_SOURCE", {

  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "data",
    filters = list(
      REF_AREA = "FRA",
      SEX = "SEX_F",
      AGE = "AGE_YTHADULT_YGE15"
    ),
    best_source = "no"
  )

  expect_true("BEST_SOURCE" %in% names(x))
  expect_false("indicator" %in% names(x))
})


test_that("label and label_short use semantic metadata", {

  filters <- list(
    REF_AREA = "FRA",
    SEX = "SEX_F",
    AGE = "AGE_YTHADULT_YGE15"
  )

  label <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    filters = filters,
    type = "label"
  )

  expect_true(
    all(label$AGE == "Age (Youth, adults): 15+")
  )

  short <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    filters = filters,
    type = "label_short"
  )

  expect_true(all(short$AGE == "15+"))
})


test_that("dataflow returns one requested flow", {

  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "dataflow"
  )

  expect_s3_class(x, "data.frame")

  expect_named(
    x,
    c(
      "DATAFLOW_ID",
      "DATAFLOW_NAME",
      "LAST_UPDATE"
    )
  )

  expect_equal(nrow(x), 1L)

  expect_equal(
    x$DATAFLOW_ID,
    "UNE_TUNE_SEX_AGE_NB_A"
  )

  expect_match(
    x$DATAFLOW_NAME,
    "Unemployment by sex and age"
  )
})


test_that("dataflow supports complete catalogue", {

  x <- get_ilostat_sdmx(
    resource = "dataflow"
  )

  expect_s3_class(x, "data.frame")

  expect_named(
    x,
    c(
      "DATAFLOW_ID",
      "DATAFLOW_NAME",
      "LAST_UPDATE"
    )
  )

  expect_gt(nrow(x), 1L)

  expect_true(
    "UNE_TUNE_SEX_AGE_NB_A" %in%
      x$DATAFLOW_ID
  )
})