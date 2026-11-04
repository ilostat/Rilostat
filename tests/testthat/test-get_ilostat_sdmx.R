SDMX_TEST_ID <- "UNE_TUNE_SEX_AGE_NB_A"

sdmx_test_meta <- .ilostat_sdmx_get_meta(
  SDMX_TEST_ID
)

test_that("SDMX metadata defines dynamic components", {

  skip_on_cran()
  meta <- sdmx_test_meta

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

  skip_on_cran()

  meta <- sdmx_test_meta

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

  skip_on_cran()

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

  skip_on_cran()

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

test_that("codelist returns canonical SDMX dictionaries", {

  skip_on_cran()

  freq <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "codelist",
    component = "FREQ"
  )

  expect_named(freq, c("FREQ", "FREQ_LABEL"))
  expect_true("A" %in% freq$FREQ)

  measure <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "codelist",
    component = "MEASURE"
  )

  expect_named(
    measure,
    c("MEASURE", "MEASURE_LABEL")
  )
  expect_true(
    "UNE_TUNE_NB" %in% measure$MEASURE
  )

  unit <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "codelist",
    component = "UNIT_MEASURE"
  )

  expect_named(
    unit,
    c("UNIT_MEASURE", "UNIT_MEASURE_LABEL")
  )
  expect_true(
    "PS" %in% unit$UNIT_MEASURE
  )

  mult <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "codelist",
    component = "UNIT_MULT"
  )

  expect_named(
    mult,
    c("UNIT_MULT", "UNIT_MULT_LABEL")
  )
  expect_true(
    "3" %in% mult$UNIT_MULT
  )
})

test_that("codelist supports observation status", {

  skip_on_cran()

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

  skip_on_cran()

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

  skip_on_cran()

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

  skip_on_cran()

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

skip_on_cran()

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

  skip_on_cran()

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

test_that("SDMX structure roles match pyilostat", {

skip_on_cran()

  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "structure"
  )

  roles <- setNames(
    x$ROLE,
    x$COMPONENT_ID
  )

  expect_equal(roles[["REF_AREA"]], "dimension")
  expect_equal(roles[["SOURCE"]], "dimension")
  expect_equal(roles[["TIME_PERIOD"]], "dimension")
  expect_equal(roles[["OBS_VALUE"]], "measure")
  expect_equal(roles[["OBS_STATUS"]], "attribute")
  expect_equal(roles[["BEST_SOURCE"]], "attribute")
})


test_that("SDMX structure representations match pyilostat", {

  skip_on_cran()

  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "structure"
  )

  representation <- setNames(
    x$REPRESENTATION,
    x$COMPONENT_ID
  )

  expect_equal(representation[["REF_AREA"]], "coded")
  expect_equal(representation[["AGE"]], "coded")
  expect_equal(representation[["TIME_PERIOD"]], "time")
  expect_equal(representation[["OBS_VALUE"]], "number")
  expect_equal(representation[["BEST_SOURCE"]], "coded")
})


test_that("SDMX structure positions match pyilostat", {

  skip_on_cran()

  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "structure"
  )

  position <- setNames(
    x$POSITION,
    x$COMPONENT_ID
  )

  expect_equal(position[["REF_AREA"]], 1L)
  expect_equal(position[["AGE"]], 6L)
  expect_equal(position[["TIME_PERIOD"]], 7L)

  expect_true(is.na(position[["OBS_VALUE"]]))
  expect_true(is.na(position[["OBS_STATUS"]]))
  expect_true(is.na(position[["BEST_SOURCE"]]))
})


test_that("SDMX-ML structure is well formed", {

  skip_on_cran()

  xml <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "structure",
    format = "sdmx-ml"
  )

  doc <- xml2::read_xml(xml)

  expect_s3_class(
    doc,
    "xml_document"
  )

  ns <- xml2::xml_ns(doc)

  expect_length(
    xml2::xml_find_all(
      doc,
      ".//str:TimeDimension",
      ns = ns
    ),
    1L
  )

  expect_length(
    xml2::xml_find_all(
      doc,
      ".//str:AttributeRelationship/str:Observation",
      ns = ns
    ),
    7L
  )
})


test_that("SDMX-ML TIME_PERIOD uses ObservationalTimePeriod", {

  skip_on_cran()

  xml <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "structure",
    format = "sdmx-ml"
  )

  doc <- xml2::read_xml(xml)
  ns <- xml2::xml_ns(doc)

  node <- xml2::xml_find_first(
    doc,
    paste0(
      ".//str:TimeDimension/",
      "str:LocalRepresentation/",
      "str:TextFormat"
    ),
    ns = ns
  )

  expect_equal(
    xml2::xml_attr(node, "textType"),
    "ObservationalTimePeriod"
  )
})


test_that("SDMX-ML contains ConceptScheme and concepts", {

  skip_on_cran()

  xml <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "structure",
    format = "sdmx-ml"
  )

  doc <- xml2::read_xml(xml)
  ns <- xml2::xml_ns(doc)

  scheme <- xml2::xml_find_first(
    doc,
    ".//str:ConceptScheme",
    ns = ns
  )

  expect_equal(
    xml2::xml_attr(scheme, "id"),
    "CS_UNE_TUNE_SEX_AGE_NB_A"
  )

  concepts <- xml2::xml_attr(
    xml2::xml_find_all(
      doc,
      ".//str:Concept",
      ns = ns
    ),
    "id"
  )

  expect_true("REF_AREA" %in% concepts)
  expect_true("OBS_VALUE" %in% concepts)
  expect_true("BEST_SOURCE" %in% concepts)
})

test_that("SDMX-ML AGE codelist uses canonical dictionary", {

  skip_on_cran()

  xml <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "codelist",
    component = "AGE",
    format = "sdmx-ml"
  )

  doc <- xml2::read_xml(xml)
  ns <- xml2::xml_ns(doc)

  codelist <- xml2::xml_find_first(
    doc,
    ".//str:Codelist",
    ns = ns
  )

  expect_equal(
    xml2::xml_attr(codelist, "id"),
    "CL_AGE"
  )

  expect_equal(
    xml2::xml_attr(codelist, "agencyID"),
    "ILO"
  )

  expect_equal(
    xml2::xml_attr(codelist, "version"),
    "1.0"
  )

  codes <- xml2::xml_find_all(
    doc,
    ".//str:Codelist/str:Code",
    ns = ns
  )

  expect_length(codes, 41L)
})

test_that("SDMX-ML dataflow references its DataStructure", {

  skip_on_cran()

  xml <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "dataflow",
    format = "sdmx-ml"
  )

  doc <- xml2::read_xml(xml)
  ns <- xml2::xml_ns(doc)

  flow <- xml2::xml_find_first(
    doc,
    ".//str:Dataflow",
    ns = ns
  )

  expect_equal(
    xml2::xml_attr(flow, "id"),
    "UNE_TUNE_SEX_AGE_NB_A"
  )

  expect_equal(
    xml2::xml_attr(flow, "agencyID"),
    "ILO"
  )

  expect_equal(
    xml2::xml_attr(flow, "version"),
    "1.0"
  )

  structure <- xml2::xml_find_first(
    flow,
    "./str:Structure",
    ns = ns
  )

  expect_equal(
    xml2::xml_text(structure),
    paste0(
      "urn:sdmx:org.sdmx.infomodel.",
      "datastructure.DataStructure=",
      "ILO:UNE_TUNE_SEX_AGE_NB_A(1.0)"
    )
  )
})


test_that("SDMX-ML dataflow supports complete catalogue", {

  skip_on_cran()

  data <- get_ilostat_sdmx(
    resource = "dataflow"
  )

  xml <- get_ilostat_sdmx(
    resource = "dataflow",
    format = "sdmx-ml"
  )

  doc <- xml2::read_xml(xml)
  ns <- xml2::xml_ns(doc)

  flows <- xml2::xml_find_all(
    doc,
    ".//str:Dataflow",
    ns = ns
  )

  expect_length(
    flows,
    nrow(data)
  )
})

test_that("SDMX-JSON structure follows SDMX 2.1 model", {

  skip_on_cran()

  text <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "structure",
    format = "sdmx-json"
  )

  x <- jsonlite::fromJSON(
    text,
    simplifyVector = FALSE
  )

  expect_equal(
    x$`$schema`,
    paste0(
      "https://json.sdmx.org/2.1/",
      "sdmx-json-structure-schema.json"
    )
  )

  dsd <- x$data$dataStructures[[1]]

  expect_equal(
    dsd$id,
    "UNE_TUNE_SEX_AGE_NB_A"
  )

  components <- dsd$dataStructureComponents

  dimensions <- vapply(
    components$dimensionList$dimensions,
    function(x) x$id,
    character(1)
  )

  expect_equal(
    dimensions,
    c(
      "REF_AREA",
      "SOURCE",
      "FREQ",
      "MEASURE",
      "SEX",
      "AGE"
    )
  )

  expect_equal(
    components$dimensionList$
      timeDimension$id,
    "TIME_PERIOD"
  )

  expect_equal(
    components$dimensionList$
      timeDimension$
      localRepresentation$
      format$
      dataType,
    "ObservationalTimePeriod"
  )

  expect_equal(
    components$measureList$
      measures[[1]]$id,
    "OBS_VALUE"
  )
})

test_that("SDMX-JSON AGE codelist follows SDMX 2.1 model", {

  skip_on_cran()

  text <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    resource = "codelist",
    component = "AGE",
    format = "sdmx-json"
  )

  x <- jsonlite::fromJSON(
    text,
    simplifyVector = FALSE
  )

  expect_equal(
    x$`$schema`,
    paste0(
      "https://json.sdmx.org/2.1/",
      "sdmx-json-structure-schema.json"
    )
  )

  expect_equal(
    x$meta$id,
    "CODELIST_AGE"
  )

  codelist <- x$data$codelists[[1]]

  expect_equal(
    codelist$id,
    "CL_AGE"
  )

  expect_equal(
    codelist$agencyID,
    "ILO"
  )

  expect_equal(
    codelist$version,
    "1.0"
  )

  expect_length(
    codelist$codes,
    41L
  )

  expect_true(
    all(
      c("id", "name", "names") %in%
        names(codelist$codes[[1]])
    )
  )
})

test_that("SDMX-JSON dataflow references its DataStructure", {

  skip_on_cran()

  flow_data <- data.frame(
    DATAFLOW_ID = SDMX_TEST_ID,
    DATAFLOW_NAME =
      "Unemployment by sex and age - Annual",
    LAST_UPDATE = "2026-01-01",
    stringsAsFactors = FALSE
  )

  text <- .ilostat_sdmx_to_json_dataflow(
    data = flow_data,
    lang = "en"
  )

  x <- jsonlite::fromJSON(
    text,
    simplifyVector = FALSE
  )

  expect_equal(
    x$`$schema`,
    paste0(
      "https://json.sdmx.org/2.1/",
      "sdmx-json-structure-schema.json"
    )
  )

  expect_equal(
    x$meta$id,
    "ILOSTAT_DATAFLOWS"
  )

  expect_length(
    x$data$dataflows,
    1L
  )

  flow <- x$data$dataflows[[1]]

  expect_equal(
    flow$id,
    SDMX_TEST_ID
  )

  expect_equal(
    flow$agencyID,
    "ILO"
  )

  expect_equal(
    flow$version,
    "1.0"
  )

  expect_equal(
    flow$structure,
    paste0(
      "urn:sdmx:org.sdmx.infomodel.",
      "datastructure.DataStructure=",
      "ILO:",
      SDMX_TEST_ID,
      "(1.0)"
    )
  )
})

test_that("public SDMX-JSON Dataflow references its DSD", {

  skip_on_cran()

  text <- get_ilostat_sdmx(
    SDMX_TEST_ID,
    resource = "dataflow",
    format = "sdmx-json"
  )

  x <- jsonlite::fromJSON(
    text,
    simplifyVector = FALSE
  )

  flow <- x$data$dataflows[[1]]

  expect_equal(
    flow$id,
    SDMX_TEST_ID
  )

  expect_equal(
    flow$structure,
    paste0(
      "urn:sdmx:org.sdmx.infomodel.",
      "datastructure.DataStructure=",
      "ILO:",
      SDMX_TEST_ID,
      "(1.0)"
    )
  )
})

test_that("SDMX annual time periods are unchanged", {

    skip_on_cran()

  x <- .ilostat_sdmx_format_time_period(
    c(
      "2023",
      "2024"
    )
  )

  expect_equal(
    x,
    c(
      "2023",
      "2024"
    )
  )
})


test_that("SDMX quarterly time periods are normalized", {

    skip_on_cran()

  x <- .ilostat_sdmx_format_time_period(
    c(
      "2024Q1",
      "2024Q4"
    )
  )

  expect_equal(
    x,
    c(
      "2024-Q1",
      "2024-Q4"
    )
  )
})


test_that("SDMX monthly time periods are normalized", {

    skip_on_cran()

  x <- .ilostat_sdmx_format_time_period(
    c(
      "2024M01",
      "2024M12"
    )
  )

  expect_equal(
    x,
    c(
      "2024-M01",
      "2024-M12"
    )
  )
})

test_that("non-matching FREQ returns empty SDMX schema", {

    skip_on_cran()

  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    filters = list(
      FREQ = "Q"
    )
  )

  expect_equal(
    dim(x),
    c(0L, 17L)
  )

  expect_named(
    x,
    c(
      "STRUCTURE",
      "STRUCTURE_ID",
      "ACTION",
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
    )
  )
})


test_that("non-matching MEASURE preserves both schema", {

    skip_on_cran()

  
  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    type = "both",
    filters = list(
      MEASURE = "XXX"
    )
  )

  expect_equal(
    dim(x),
    c(0L, 30L)
  )

  expect_true(
    all(
      c(
        "STRUCTURE_NAME",
        "REF_AREA_LABEL",
        "SOURCE_LABEL",
        "FREQ_LABEL",
        "MEASURE_LABEL",
        "AGE_LABEL",
        "NOTE_CLASSIF_LABEL"
      ) %in% names(x)
    )
  )

  expect_false(
    "BEST_SOURCE" %in% names(x)
  )
})

test_that("SDMX type code returns codes without STRUCTURE_NAME", {

     skip_on_cran()

  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    type = "code",
    filters = list(
      REF_AREA = "FRA",
      SEX = "SEX_F",
      AGE = "AGE_YTHADULT_YGE15"
    )
  )

  expect_false(
    "STRUCTURE_NAME" %in% names(x)
  )

  expect_equal(
    x$STRUCTURE[[1L]],
    "dataflow"
  )

  expect_equal(
    x$STRUCTURE_ID[[1L]],
    "ILO:DF_UNE_TUNE_SEX_AGE_NB_A(1.0)"
  )

  expect_equal(
    x$ACTION[[1L]],
    "I"
  )

  expect_equal(
    x$REF_AREA[[1L]],
    "FRA"
  )

  expect_equal(
    x$SEX[[1L]],
    "SEX_F"
  )

  expect_equal(
    x$AGE[[1L]],
    "AGE_YTHADULT_YGE15"
  )
})


test_that("SDMX type label returns full semantic labels", {

     skip_on_cran()

  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    type = "label",
    filters = list(
      REF_AREA = "FRA",
      SEX = "SEX_F",
      AGE = "AGE_YTHADULT_YGE15"
    )
  )

  expect_equal(
    x$STRUCTURE_NAME[[1L]],
    "Unemployment by sex and age (thousands) - Annual"
  )

  expect_equal(
    x$REF_AREA[[1L]],
    "France"
  )

  expect_equal(
    x$SOURCE[[1L]],
    "LFS - Employment Survey"
  )

  expect_equal(
    x$SEX[[1L]],
    "Female"
  )

  expect_equal(
    x$AGE[[1L]],
    "Age (Youth, adults): 15+"
  )

  expect_false(
    any(
      grepl(
        "_LABEL$",
        names(x)
      )
    )
  )
})


test_that("SDMX type label_short uses short semantic labels", {

     skip_on_cran()

  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    type = "label_short",
    filters = list(
      REF_AREA = "FRA",
      SEX = "SEX_F",
      AGE = "AGE_YTHADULT_YGE15"
    )
  )

  expect_equal(
    x$STRUCTURE_NAME[[1L]],
    "Unemployment by sex and age (thousands) - Annual"
  )

  expect_equal(
    x$REF_AREA[[1L]],
    "France"
  )

  expect_equal(
    x$SOURCE[[1L]],
    "LFS - Employment Survey"
  )

  expect_equal(
    x$SEX[[1L]],
    "Female"
  )

  expect_equal(
    x$AGE[[1L]],
    "15+"
  )
})


test_that("SDMX type both interleaves codes and labels", {

     skip_on_cran()

  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    type = "both",
    filters = list(
      REF_AREA = "FRA",
      SEX = "SEX_F",
      AGE = "AGE_YTHADULT_YGE15"
    )
  )

  expect_equal(
    names(x)[1:16],
    c(
      "STRUCTURE",
      "STRUCTURE_ID",
      "STRUCTURE_NAME",
      "ACTION",
      "REF_AREA",
      "REF_AREA_LABEL",
      "SOURCE",
      "SOURCE_LABEL",
      "FREQ",
      "FREQ_LABEL",
      "MEASURE",
      "MEASURE_LABEL",
      "SEX",
      "SEX_LABEL",
      "AGE",
      "AGE_LABEL"
    )
  )

  expect_equal(
    x$REF_AREA[[1L]],
    "FRA"
  )

  expect_equal(
    x$REF_AREA_LABEL[[1L]],
    "France"
  )

  expect_equal(
    x$SOURCE_LABEL[[1L]],
    "LFS - Employment Survey"
  )

  expect_equal(
    x$AGE[[1L]],
    "AGE_YTHADULT_YGE15"
  )

  expect_equal(
    x$AGE_LABEL[[1L]],
    "Age (Youth, adults): 15+"
  )
})

test_that("SDMX BEST_SOURCE follows pyilostat behavior", {
   skip_on_cran()


  filters <- list(
    REF_AREA = "FRA",
    SEX = "SEX_F",
    AGE = "AGE_YTHADULT_YGE15"
  )

  best <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    type = "both",
    filters = filters,
    best_source = "yes"
  )

  expect_false(
    "BEST_SOURCE" %in% names(best)
  )

  non_best <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    type = "both",
    filters = filters,
    best_source = "no"
  )

  expect_equal(
    tail(names(non_best), 1L),
    "BEST_SOURCE"
  )

  expect_false(
    "BEST_SOURCE_LABEL" %in%
      names(non_best)
  )

  all_sources <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    type = "both",
    filters = filters,
    best_source = "all"
  )

  expect_equal(
    tail(names(all_sources), 1L),
    "BEST_SOURCE"
  )

  expect_false(
    "BEST_SOURCE_LABEL" %in%
      names(all_sources)
  )
})

test_that("SDMX constant filters match dataset constants", {

     skip_on_cran()

  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    filters = list(
      REF_AREA = "FRA",
      FREQ = "A",
      MEASURE = "UNE_TUNE_NB"
    )
  )

  expect_true(
    all(x$FREQ == "A")
  )

  expect_true(
    all(
      x$MEASURE ==
        "UNE_TUNE_NB"
    )
  )
})


test_that("SDMX non-matching constants return empty schema", {

     skip_on_cran()

  
  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    filters = list(
      FREQ = "Q"
    )
  )

  expect_equal(
    dim(x),
    c(0L, 17L)
  )

  expect_equal(
    names(x)[1:3],
    c(
      "STRUCTURE",
      "STRUCTURE_ID",
      "ACTION"
    )
  )
})


test_that("SDMX TIME_PERIOD range uses native year filtering", {

     skip_on_cran()

  
  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    filters = list(
      REF_AREA = "FRA",
      TIME_PERIOD = list(
        ge = "2020",
        le = "2023"
      )
    )
  )

  expect_setequal(
    unique(x$TIME_PERIOD),
    c(
      "2020",
      "2021",
      "2022",
      "2023"
    )
  )
})


test_that("SDMX ordinary eq and ne operators work", {

     skip_on_cran()

  
  eq <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    filters = list(
      REF_AREA = list(
        eq = "FRA"
      )
    )
  )

  expect_equal(
    unique(eq$REF_AREA),
    "FRA"
  )

  ne <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    filters = list(
      REF_AREA = "FRA",
      SEX = list(
        ne = "SEX_T"
      )
    )
  )

  expect_setequal(
    unique(ne$SEX),
    c(
      "SEX_M",
      "SEX_F"
    )
  )
})

test_that("SDMX eq retains native Rilostat partial matching", {

     skip_on_cran()

  
  x <- get_ilostat_sdmx(
    "UNE_TUNE_SEX_AGE_NB_A",
    filters = list(
      REF_AREA = "FRA",
      SEX = list(
        eq = "T"
      )
    )
  )

  expect_equal(
    unique(x$SEX),
    "SEX_T"
  )
})

