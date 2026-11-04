# Internal helpers for the ILOSTAT SDMX interface.
#
# These functions are intentionally kept separate from the existing
# get_ilostat() implementation. The SDMX interface reuses the native
# Rilostat download/cache infrastructure and adds a semantic layer based
# on the dataset *_meta.json metadata.


# -------------------------------------------------------------------------
# Constants
# -------------------------------------------------------------------------

.ilostat_sdmx_languages <- c("en", "fr", "es")

.ilostat_sdmx_resources <- c(
  "data",
  "structure",
  "codelist",
  "dataflow"
)

.ilostat_sdmx_types <- c(
  "code",
  "label",
  "label_short",
  "both"
)

.ilostat_sdmx_formats <- c(
  "dataframe",
  "sdmx-ml",
  "sdmx-json"
)


# Canonical ILOSTAT codelists used by SDMX components.
#
# Classification dimensions are handled dynamically from dataset metadata.
.ilostat_sdmx_codelists <- c(
  REF_AREA       = "cl_ref_area",
  SOURCE         = "cl_source",
  FREQ           = "cl_freq",
  MEASURE        = "cl_measure",
  SEX            = "cl_sex",
  AGE            = "cl_age",
  OBS_STATUS     = "cl_obs_status",
  UNIT_MEASURE   = "cl_unit_measure",
  UNIT_MULT      = "cl_unit_mult",
  NOTE_SOURCE    = "cl_note_source",
  NOTE_INDICATOR = "cl_note_indicator",
  NOTE_CLASSIF   = "cl_note_classif",
  BEST_SOURCE    = "cl_best_source"
)


# -------------------------------------------------------------------------
# Validation
# -------------------------------------------------------------------------

.ilostat_sdmx_match_arg <- function(x, choices, arg) {

  if (length(x) != 1L || is.na(x)) {
    stop(
      sprintf("`%s` must be a single non-missing value.", arg),
      call. = FALSE
    )
  }

  match.arg(x, choices)
}


.ilostat_sdmx_validate_lang <- function(lang) {

  .ilostat_sdmx_match_arg(
    lang,
    .ilostat_sdmx_languages,
    "lang"
  )
}


# -------------------------------------------------------------------------
# Cache
# -------------------------------------------------------------------------

.ilostat_sdmx_cache_dir <- function(cache_dir = NULL) {

  if (is.null(cache_dir)) {
    cache_dir <- file.path(tempdir(), "ilostat")
  }

  if (!dir.exists(cache_dir)) {
    dir.create(
      cache_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  normalizePath(
    cache_dir,
    winslash = "/",
    mustWork = TRUE
  )
}


.ilostat_sdmx_meta_file <- function(id, cache_dir = NULL) {

  cache_dir <- .ilostat_sdmx_cache_dir(cache_dir)

  file.path(
    cache_dir,
    paste0(id, "_meta.json")
  )
}


# -------------------------------------------------------------------------
# Dataset metadata
# -------------------------------------------------------------------------

.ilostat_sdmx_meta_url <- function(id) {

  paste0(
    "https://rplumber.ilo.org/files/indicator/",
    id,
    "_meta.json"
  )
}


.ilostat_sdmx_get_meta <- function(
  id,
  cache_dir = NULL,
  refresh = FALSE
) {

  # -----------------------------------------------------------------------
  # Validate dataset ID
  # -----------------------------------------------------------------------

  if (
    is.null(id) ||
    length(id) != 1L ||
    is.na(id) ||
    !nzchar(id)
  ) {
    stop(
      "`id` is required for this SDMX resource.",
      call. = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # Cache location
  # -----------------------------------------------------------------------

  cache_file <- .ilostat_sdmx_meta_file(
    id = id,
    cache_dir = cache_dir
  )

  # -----------------------------------------------------------------------
  # Download metadata when required
  # -----------------------------------------------------------------------

  if (
    refresh ||
    !file.exists(cache_file)
  ) {

    url <- .ilostat_sdmx_meta_url(id)

    # Use the same User-Agent as the rest of Rilostat.
    ua <- build_user_agent()

    resp <- try(
      httr2::request(url) |>
        httr2::req_headers(
          `User-Agent` = ua
        ) |>
        httr2::req_timeout(30) |>
        httr2::req_perform(),
      silent = TRUE
    )

    if (
      inherits(resp, "try-error") ||
      httr2::resp_status(resp) != 200L
    ) {
      stop(
        sprintf(
          "Unable to download SDMX metadata for dataset `%s`.",
          id
        ),
        call. = FALSE
      )
    }

    # ---------------------------------------------------------------------
    # Write response to temporary file first
    # ---------------------------------------------------------------------

    tmp <- tempfile(
      pattern = paste0(
        id,
        "_meta_"
      ),
      fileext = ".json"
    )

    on.exit(
      unlink(tmp),
      add = TRUE
    )

    writeBin(
      httr2::resp_body_raw(resp),
      tmp
    )

    # ---------------------------------------------------------------------
    # Validate downloaded JSON before replacing the cache
    # ---------------------------------------------------------------------

    downloaded_meta <- try(
      jsonlite::fromJSON(
        tmp,
        simplifyVector = FALSE
      ),
      silent = TRUE
    )

    if (
      inherits(downloaded_meta, "try-error") ||
      is.null(downloaded_meta$dataset$id) ||
      !identical(
        downloaded_meta$dataset$id,
        id
      )
    ) {
      stop(
        sprintf(
          "Invalid SDMX metadata returned for dataset `%s`.",
          id
        ),
        call. = FALSE
      )
    }

    # ---------------------------------------------------------------------
    # Update disk cache
    # ---------------------------------------------------------------------

    if (
      !file.copy(
        tmp,
        cache_file,
        overwrite = TRUE
      )
    ) {
      stop(
        sprintf(
          "Unable to write SDMX metadata cache for dataset `%s`.",
          id
        ),
        call. = FALSE
      )
    }
  }

  # -----------------------------------------------------------------------
  # Read metadata from disk cache
  # -----------------------------------------------------------------------

  meta <- try(
    jsonlite::fromJSON(
      cache_file,
      simplifyVector = FALSE
    ),
    silent = TRUE
  )

  if (
    inherits(meta, "try-error") ||
    is.null(meta$dataset$id) ||
    !identical(
      meta$dataset$id,
      id
    )
  ) {
    stop(
      sprintf(
        "Invalid SDMX metadata returned for dataset `%s`.",
        id
      ),
      call. = FALSE
    )
  }

  meta
}


# -------------------------------------------------------------------------
# Metadata utilities
# -------------------------------------------------------------------------

.ilostat_sdmx_lang_value <- function(x, lang, default = NA_character_) {

  if (is.null(x)) {
    return(default)
  }

  value <- x[[lang]]

  if (is.null(value) || length(value) == 0L) {
    return(default)
  }

  as.character(value[[1L]])
}


.ilostat_sdmx_dimension_components <- function(meta, lang = "en") {

  lang <- .ilostat_sdmx_validate_lang(lang)

  dims <- meta$dimensions

  if (is.null(dims) || length(dims) == 0L) {
    return(data.frame(
      component_id = character(),
      native_name = character(),
      component_name = character(),
      stringsAsFactors = FALSE
    ))
  }

  out <- lapply(
    names(dims),
    function(name) {

      x <- dims[[name]]

      component_id <- x$type

      if (is.null(component_id) || !nzchar(component_id)) {
        component_id <- toupper(name)
      }

      native_name <- x$api_name

      if (is.null(native_name) || !nzchar(native_name)) {
        native_name <- name
      }

      component_name <- .ilostat_sdmx_lang_value(
        x$label,
        lang,
        default = component_id
      )

      data.frame(
        component_id = toupper(component_id),
        native_name = native_name,
        component_name = component_name,
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(rbind, out)
}


# -------------------------------------------------------------------------
# Complete semantic component map
# -------------------------------------------------------------------------

.ilostat_sdmx_components <- function(meta, lang = "en") {

  lang <- .ilostat_sdmx_validate_lang(lang)

  dimensions <- .ilostat_sdmx_dimension_components(
    meta,
    lang = lang
  )

  lookup_native <- function(component, default = NA_character_) {

    hit <- dimensions$native_name[
      dimensions$component_id == component
    ]

    if (length(hit)) {
      hit[[1L]]
    } else {
      default
    }
  }

  lookup_name <- function(component, default = component) {

    hit <- dimensions$component_name[
      dimensions$component_id == component
    ]

    if (length(hit)) {
      hit[[1L]]
    } else {
      default
    }
  }

  # -----------------------------------------------------------------------
  # Standard SDMX dimensions
  # -----------------------------------------------------------------------

  base <- data.frame(
    component_id = c(
      "REF_AREA",
      "SOURCE",
      "FREQ",
      "MEASURE"
    ),
    role = rep("dimension", 4L),
    native_name = c(
      lookup_native("REF_AREA", "ref_area"),
      lookup_native("SOURCE", "source"),
      NA_character_,
      NA_character_
    ),
    component_name = c(
      lookup_name("REF_AREA", "Reference area"),
      lookup_name("SOURCE", "Source"),
      "Frequency",
      "Measure"
    ),
    codelist = c(
      "cl_ref_area",
      "cl_source",
      "cl_freq",
      "cl_measure"
    ),
    stringsAsFactors = FALSE
  )

  # -----------------------------------------------------------------------
  # Dynamic semantic dimensions
  #
  # SEX, AGE, ECO, EDU, OCU, etc.
  # -----------------------------------------------------------------------

  reserved <- c(
    "REF_AREA",
    "SOURCE"
  )

  dynamic <- dimensions[
    !dimensions$component_id %in% reserved,
    ,
    drop = FALSE
  ]

  if (nrow(dynamic)) {

    dynamic <- data.frame(
      component_id = dynamic$component_id,
      role = "dimension",
      native_name = dynamic$native_name,
      component_name = dynamic$component_name,
      codelist = unname(
        .ilostat_sdmx_codelists[
          dynamic$component_id
        ]
      ),
      stringsAsFactors = FALSE
    )

  } else {

    dynamic <- base[FALSE, , drop = FALSE]
  }

  # -----------------------------------------------------------------------
  # Time dimension, measure and attributes
  # -----------------------------------------------------------------------

  tail <- data.frame(
    component_id = c(
      "TIME_PERIOD",
      "OBS_VALUE",
      "OBS_STATUS",
      "UNIT_MEASURE",
      "UNIT_MULT",
      "NOTE_SOURCE",
      "NOTE_INDICATOR",
      "NOTE_CLASSIF",
      "BEST_SOURCE"
    ),
    role = c(
      "dimension",
      "measure",
      rep("attribute", 7L)
    ),
    native_name = c(
      "time",
      "obs_value",
      "obs_status",
      NA_character_,
      NA_character_,
      "note_source",
      "note_indicator",
      "note_classif",
      "best_source"
    ),
    component_name = c(
      "Time period",
      "Observation value",
      "Observation status",
      "Unit of measure",
      "Multiplier",
      "Source note",
      "Indicator note",
      "Classification note",
      "Best source"
    ),
    codelist = c(
      NA_character_,
      NA_character_,
      "cl_obs_status",
      "cl_unit_measure",
      "cl_unit_mult",
      "cl_note_source",
      "cl_note_indicator",
      "cl_note_classif",
      "cl_best_source"
    ),
    stringsAsFactors = FALSE
  )

  out <- rbind(
    base,
    dynamic,
    tail
  )

  # -----------------------------------------------------------------------
  # Position
  #
  # Python assigns positions only to dimensions.
  # TIME_PERIOD has a logical position in the dataframe structure, but the
  # XML serializer does not write a position attribute on TimeDimension.
  #
  # Measures and attributes have no position.
  # -----------------------------------------------------------------------

  out$position <- NA_integer_

dimension_rows <- out$role == "dimension"

  out$position[dimension_rows] <- seq_len(
    sum(dimension_rows)
  )

  # -----------------------------------------------------------------------
  # Representation
  #
  # Match pyilostat:
  #   coded  -> dimensions/attributes using codelists
  #   time   -> TIME_PERIOD
  #   number -> OBS_VALUE
  # -----------------------------------------------------------------------

  out$representation <- "coded"

  out$representation[
    out$component_id == "TIME_PERIOD"
  ] <- "time"

  out$representation[
    out$component_id == "OBS_VALUE"
  ] <- "number"

  out
}

# -------------------------------------------------------------------------
# SDMX -> native Rilostat filters
# -------------------------------------------------------------------------

.ilostat_sdmx_translate_filters <- function(
  filters,
  components
) {

  if (is.null(filters)) {
    return(NULL)
  }

  if (!is.list(filters) || is.null(names(filters))) {
    stop(
      "`filters` must be a named list.",
      call. = FALSE
    )
  }

  if (any(!nzchar(names(filters)))) {
    stop(
      "All `filters` entries must be named.",
      call. = FALSE
    )
  }

  component_names <- toupper(names(filters))

  native <- components$native_name[
    match(
      component_names,
      components$component_id
    )
  ]

  unknown <- is.na(native) | !nzchar(native)

  if (any(unknown)) {
    stop(
      sprintf(
        "Unsupported SDMX filter component(s): %s.",
        paste(
          names(filters)[unknown],
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  names(filters) <- native

  filters
}


# -------------------------------------------------------------------------
# Value dictionaries from metadata
# -------------------------------------------------------------------------

.ilostat_sdmx_values_to_df <- function(values, lang = "en") {

  if (is.null(values) || !length(values)) {
    return(NULL)
  }

  out <- lapply(
    values,
    function(value) {

      label <- .ilostat_sdmx_lang_value(
        value$label,
        lang,
        default = value$code
      )

      display <- .ilostat_sdmx_lang_value(
        value$display,
        lang,
        default = label
      )

      data.frame(
        code = as.character(value$code),
        label = label,
        label_short = display,
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(rbind, out)
}


.ilostat_sdmx_metadata_codelist <- function(
  meta,
  component,
  lang = "en"
) {

  lang <- .ilostat_sdmx_validate_lang(lang)
  component <- toupper(component)

  # -----------------------------------------------------------------------
  # Dataset dimensions
  #
  # REF_AREA, SEX and dynamic classifications such as
  # AGE, ECO, OCU, EDU, etc.
  # -----------------------------------------------------------------------

  dims <- meta$dimensions

  if (!is.null(dims)) {

    for (name in names(dims)) {

      x <- dims[[name]]

      type <- x$type

      if (
        is.null(type) ||
        !length(type) ||
        is.na(type) ||
        !nzchar(type)
      ) {
        type <- toupper(name)
      }

      if (
        identical(
          toupper(type),
          component
        )
      ) {

        return(
          .ilostat_sdmx_values_to_df(
            x$values,
            lang = lang
          )
        )
      }
    }
  }

  # -----------------------------------------------------------------------
  # Source
  #
  # SOURCE is stored separately from meta$dimensions.
  # -----------------------------------------------------------------------

  if (component == "SOURCE") {

    values <- meta$source$values

    if (
      !is.null(values) &&
      length(values)
    ) {

      return(
        .ilostat_sdmx_values_to_df(
          values,
          lang = lang
        )
      )
    }

    return(NULL)
  }

  # -----------------------------------------------------------------------
  # Frequency
  #
  # FREQ is a dataset-level constant.
  # -----------------------------------------------------------------------

  if (component == "FREQ") {

    x <- meta$dataset$frequency

    if (
      !is.null(x) &&
      !is.null(x$code)
    ) {

      label <- .ilostat_sdmx_lang_value(
        x$label,
        lang,
        default = x$code
      )

      return(
        data.frame(
          code = as.character(x$code),
          label = label,
          label_short = label,
          stringsAsFactors = FALSE
        )
      )
    }

    return(NULL)
  }

  # -----------------------------------------------------------------------
  # Dataset-level SDMX constants
  #
  # MEASURE
  # UNIT_MEASURE
  # UNIT_MULT
  # -----------------------------------------------------------------------

  if (component == "MEASURE") {

    x <- meta$sdmx$measure

  } else if (component == "UNIT_MEASURE") {

    x <- meta$sdmx$unit_measure

  } else if (component == "UNIT_MULT") {

    x <- meta$sdmx$unit_mult

  } else {

    x <- NULL
  }

  if (
    !is.null(x) &&
    !is.null(x$code)
  ) {

    label <- .ilostat_sdmx_lang_value(
      x$label,
      lang,
      default = x$code
    )

    return(
      data.frame(
        code = as.character(x$code),
        label = label,
        label_short = label,
        stringsAsFactors = FALSE
      )
    )
  }

  # -----------------------------------------------------------------------
  # Observation status
  # -----------------------------------------------------------------------

  if (component == "OBS_STATUS") {

    return(
      .ilostat_sdmx_values_to_df(
        meta$observation_status,
        lang = lang
      )
    )
  }

  # -----------------------------------------------------------------------
  # Notes
  # -----------------------------------------------------------------------

  if (component == "NOTE_SOURCE") {

    values <- meta$notes$source

  } else if (component == "NOTE_INDICATOR") {

    values <- meta$notes$indicator

  } else if (component == "NOTE_CLASSIF") {

    values <- meta$notes$classif

  } else {

    values <- NULL
  }

  if (
    !is.null(values) &&
    length(values)
  ) {

    return(
      .ilostat_sdmx_values_to_df(
        values,
        lang = lang
      )
    )
  }

  NULL
}

.ilostat_sdmx_component_values <- function(
  meta,
  component,
  lang = "en"
) {

  lang <- .ilostat_sdmx_validate_lang(lang)

  .ilostat_sdmx_metadata_codelist(
    meta = meta,
    component = component,
    lang = lang
  )
}

# -------------------------------------------------------------------------
# Structure dataframe
# -------------------------------------------------------------------------

.ilostat_sdmx_structure_dataframe <- function(
  meta,
  lang = "en"
) {

  components <- .ilostat_sdmx_components(
    meta,
    lang = lang
  )

  data.frame(
    COMPONENT_ID = components$component_id,
    ROLE = components$role,
    POSITION = components$position,
    REPRESENTATION = components$representation,
    COMPONENT_NAME = components$component_name,
    CODELIST = components$codelist,
    stringsAsFactors = FALSE
  )
}

# -------------------------------------------------------------------------
# Codelist dataframe
# -------------------------------------------------------------------------

.ilostat_sdmx_codelist_dataframe <- function(
  id,
  meta,
  component,
  lang = "en",
  cache_dir = NULL,
  refresh = FALSE
) {

  lang <- .ilostat_sdmx_validate_lang(lang)

  if (
    is.null(component) ||
    length(component) != 1L ||
    is.na(component) ||
    !nzchar(component)
  ) {
    stop(
      "`component` is required for `resource = \"codelist\"`.",
      call. = FALSE
    )
  }

  component <- toupper(component)

  # Build the same semantic structure model used by structure/SDMX-ML.
  components <- .ilostat_sdmx_components(
    meta = meta,
    lang = lang
  )

  i <- match(
    component,
    components$component_id
  )

  if (is.na(i)) {
    stop(
      sprintf(
        "Unknown SDMX component `%s` for dataset `%s`.",
        component,
        id
      ),
      call. = FALSE
    )
  }

  codelist <- components$codelist[[i]]

  if (
    is.na(codelist) ||
    !nzchar(codelist)
  ) {
    stop(
      sprintf(
        "SDMX component `%s` does not use a codelist.",
        component
      ),
      call. = FALSE
    )
  }

  # Reuse the existing Rilostat dictionary/cache infrastructure.
  args <- list(
    dic = codelist,
    lang = lang
  )

  if (!is.null(cache_dir)) {
    args$cache_dir <- cache_dir
  }

  # Add the appropriate existing refresh argument here only if
  # get_ilostat_dic() exposes one in the current Rilostat API.
  out <- do.call(
    get_ilostat_dic,
    args
  )

  out <- as.data.frame(
    out,
    stringsAsFactors = FALSE
  )

  if (ncol(out) < 2L) {
    stop(
      sprintf(
        "ILOSTAT dictionary `%s` does not contain code and label columns.",
        codelist
      ),
      call. = FALSE
    )
  }

  # Match the public R SDMX convention we established:
  # AGE / AGE_LABEL, REF_AREA / REF_AREA_LABEL, etc.
  out <- out[, 1:2, drop = FALSE]

  names(out) <- c(
    component,
    paste0(component, "_LABEL")
  )

  out
}

.ilostat_sdmx_values_to_df <- function(values, lang = "en") {

  if (is.null(values) || !length(values)) {
    return(NULL)
  }

  out <- lapply(values, function(value) {

    label <- .ilostat_sdmx_lang_value(
      value$label,
      lang,
      default = value$code
    )

    display <- .ilostat_sdmx_lang_value(
      value$display,
      lang,
      default = label
    )

    data.frame(
      code = as.character(value$code),
      label = label,
      label_short = display,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}

# -------------------------------------------------------------------------
# Dataflow dataframe
# -------------------------------------------------------------------------

.ilostat_sdmx_dataflow_dataframe <- function(
  id = NULL,
  segment = "indicator",
  lang = "en",
  cache_dir = NULL,
  refresh = FALSE
) {

  lang <- .ilostat_sdmx_validate_lang(lang)

  args <- list(
    segment = segment,
    lang = lang
  )

  if (!is.null(cache_dir)) {
    args$cache_dir <- cache_dir
  }

  # get_ilostat_toc() uses cache_update for forced refresh.
  if (refresh) {
    args$cache_update <- TRUE
  }

  toc <- do.call(
    get_ilostat_toc,
    args
  )

  toc <- as.data.frame(
    toc,
    stringsAsFactors = FALSE
  )

  required <- c(
    "id",
    "indicator.label",
    "freq.label",
    "last.update"
  )

  missing <- setdiff(
    required,
    names(toc)
  )

  if (length(missing)) {
    stop(
      sprintf(
        "ILOSTAT table of contents is missing required column(s): %s.",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.null(id)) {

    if (
      length(id) != 1L ||
      is.na(id) ||
      !nzchar(id)
    ) {
      stop(
        "`id` must be a single non-empty dataset identifier.",
        call. = FALSE
      )
    }

    toc <- toc[
      toc$id == id,
      ,
      drop = FALSE
    ]

    if (!nrow(toc)) {
      stop(
        sprintf(
          "Dataflow `%s` was not found in the ILOSTAT table of contents.",
          id
        ),
        call. = FALSE
      )
    }
  }

  data.frame(
    DATAFLOW_ID = toc$id,
    DATAFLOW_NAME = paste(
      toc$indicator.label,
      toc$freq.label,
      sep = " - "
    ),
    LAST_UPDATE = toc$last.update,
    stringsAsFactors = FALSE
  )
}

# -------------------------------------------------------------------------
# SDMX time-period formatting
#
# Match pyilostat:
#   2024     -> 2024
#   2024Q1   -> 2024-Q1
#   2024M01  -> 2024-M01
# -------------------------------------------------------------------------

.ilostat_sdmx_format_time_period <- function(x) {

  x <- as.character(x)

  quarterly <- grepl(
    "^[0-9]{4}Q[1-4]$",
    x
  )

  monthly <- grepl(
    "^[0-9]{4}M(0[1-9]|1[0-2])$",
    x
  )

  x[quarterly] <- sub(
    "^([0-9]{4})Q([1-4])$",
    "\\1-Q\\2",
    x[quarterly]
  )

  x[monthly] <- sub(
    "^([0-9]{4})M(0[1-9]|1[0-2])$",
    "\\1-M\\2",
    x[monthly]
  )

  x
}

# -------------------------------------------------------------------------
# SDMX data message envelope
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# SDMX data envelope
#
# Match pyilostat:
#   STRUCTURE    = dataflow
#   STRUCTURE_ID = ILO:<dataflow_id>(1.0)
#   STRUCTURE_NAME is included for type = "both"
#   ACTION       = I
# -------------------------------------------------------------------------

.ilostat_sdmx_add_data_envelope <- function(
  x,
  meta,
  type = "code",
  lang = "en"
) {

  n <- nrow(x)

  dataflow_id <- meta$sdmx$dataflow$id

  if (
    is.null(dataflow_id) ||
    !length(dataflow_id) ||
    is.na(dataflow_id) ||
    !nzchar(dataflow_id)
  ) {
    stop(
      "SDMX metadata does not define a Dataflow ID.",
      call. = FALSE
    )
  }

  envelope <- data.frame(
    STRUCTURE = rep(
      "dataflow",
      n
    ),
    STRUCTURE_ID = rep(
      paste0(
        "ILO:",
        dataflow_id,
        "(1.0)"
      ),
      n
    ),
    stringsAsFactors = FALSE
  )

  # pyilostat includes STRUCTURE_NAME when both codes and labels
  # are requested.
  if (type %in% c(
        "label",
        "label_short",
        "both"
      )) {

    dataflow_name <- .ilostat_sdmx_lang_value(
      meta$sdmx$dataflow$name,
      lang,
      default = dataflow_id
    )

    envelope$STRUCTURE_NAME <- rep(
      dataflow_name,
      n
    )
  }

  envelope$ACTION <- rep(
    "I",
    n
  )

  cbind(
    envelope,
    x,
    stringsAsFactors = FALSE
  )
}

.ilostat_sdmx_extract_time_filter <- function(filters) {

  empty_result <- function(filters) {
    list(
      filters = filters,
      time = NULL,
      timefrom = NULL,
      timeto = NULL
    )
  }

  if (
    is.null(filters) ||
    !"TIME_PERIOD" %in% names(filters)
  ) {
    return(
      empty_result(filters)
    )
  }

  value <- filters$TIME_PERIOD
  filters$TIME_PERIOD <- NULL

  # -----------------------------------------------------------------------
  # Simple equality
  #
  # Preserve native Rilostat matching semantics:
  #
  # TIME_PERIOD = "2020" -> time = "2020"
  # TIME_PERIOD = "12"   -> time = "12"
  # -----------------------------------------------------------------------

  if (!is.list(value)) {

    return(
      list(
        filters = filters,
        time = value,
        timefrom = NULL,
        timeto = NULL
      )
    )
  }

  # -----------------------------------------------------------------------
  # Validate operators
  # -----------------------------------------------------------------------

  supported <- c(
    "eq",
    "gt",
    "ge",
    "lt",
    "le"
  )

  unsupported <- setdiff(
    names(value),
    supported
  )

  if (length(unsupported)) {

    stop(
      sprintf(
        "Unsupported TIME_PERIOD operator(s): %s.",
        paste(
          unsupported,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  if (!length(value)) {

    stop(
      "Empty `TIME_PERIOD` operator filter.",
      call. = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # Equality
  #
  # Keep the value unchanged and let native get_ilostat() perform its
  # existing time matching.
  # -----------------------------------------------------------------------

  if (!is.null(value$eq)) {

    if (length(value) != 1L) {

      stop(
        "`TIME_PERIOD` operator `eq` cannot be combined with other operators.",
        call. = FALSE
      )
    }

    return(
      list(
        filters = filters,
        time = value$eq,
        timefrom = NULL,
        timeto = NULL
      )
    )
  }

  # -----------------------------------------------------------------------
  # Inequality helpers
  #
  # Native Rilostat timefrom/timeto operate on years. Only the first four
  # characters are therefore relevant for quarterly/monthly periods too.
  # -----------------------------------------------------------------------

  get_year <- function(x) {

    if (
      is.null(x) ||
      !length(x)
    ) {
      return(NULL)
    }

    year <- suppressWarnings(
      as.integer(
        substr(
          as.character(x)[[1L]],
          1L,
          4L
        )
      )
    )

    if (is.na(year)) {

      stop(
        sprintf(
          "Invalid TIME_PERIOD value `%s`.",
          as.character(x)[[1L]]
        ),
        call. = FALSE
      )
    }

    year
  }

  # -----------------------------------------------------------------------
  # Lower bound
  #
  # ge 2020 -> timefrom 2020
  # gt 2020 -> timefrom 2021
  # -----------------------------------------------------------------------

  lower <- integer()

  if (!is.null(value$ge)) {
    lower <- c(
      lower,
      get_year(value$ge)
    )
  }

  if (!is.null(value$gt)) {
    lower <- c(
      lower,
      get_year(value$gt) + 1L
    )
  }

  timefrom <- if (length(lower)) {
    as.character(max(lower))
  } else {
    NULL
  }

  # -----------------------------------------------------------------------
  # Upper bound
  #
  # le 2023 -> timeto 2023
  # lt 2023 -> timeto 2022
  # -----------------------------------------------------------------------

  upper <- integer()

  if (!is.null(value$le)) {
    upper <- c(
      upper,
      get_year(value$le)
    )
  }

  if (!is.null(value$lt)) {
    upper <- c(
      upper,
      get_year(value$lt) - 1L
    )
  }

  timeto <- if (length(upper)) {
    as.character(min(upper))
  } else {
    NULL
  }

  list(
    filters = filters,
    time = NULL,
    timefrom = timefrom,
    timeto = timeto
  )
}

.ilostat_sdmx_extract_component_operators <- function(
  filters
) {

  if (is.null(filters) || !length(filters)) {
    return(
      list(
        filters = filters,
        exclude = list()
      )
    )
  }

  remaining <- filters
  exclude <- list()

  for (component in names(filters)) {

    value <- filters[[component]]

    # Ordinary scalar/vector filters remain unchanged.
    if (!is.list(value)) {
      next
    }

    operators <- names(value)

    if (
      is.null(operators) ||
      !length(operators)
    ) {
      stop(
        sprintf(
          "Empty operator filter for SDMX component `%s`.",
          component
        ),
        call. = FALSE
      )
    }

    unsupported <- setdiff(
      operators,
      c("eq", "ne")
    )

    if (length(unsupported)) {
      stop(
        sprintf(
          "Unsupported operator(s) for SDMX component `%s`: %s.",
          component,
          paste(
            unsupported,
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

    if (
      !is.null(value$eq) &&
      !is.null(value$ne)
    ) {
      stop(
        sprintf(
          "SDMX component `%s` cannot combine `eq` and `ne`.",
          component
        ),
        call. = FALSE
      )
    }

    # eq maps directly onto the existing positive Rilostat filter.
    if (!is.null(value$eq)) {
      remaining[[component]] <- value$eq
      next
    }

    # ne must be applied after retrieval.
    if (!is.null(value$ne)) {
      remaining[[component]] <- NULL
      exclude[[component]] <- value$ne
    }
  }

  list(
    filters = remaining,
    exclude = exclude
  )
}

.ilostat_sdmx_apply_exclusions <- function(
  x,
  exclude
) {

  if (
    is.null(exclude) ||
    !length(exclude) ||
    !nrow(x)
  ) {
    return(x)
  }

  for (component in names(exclude)) {

    if (!component %in% names(x)) {
      next
    }

    values <- as.character(
      exclude[[component]]
    )

    keep <- !x[[component]] %in% values

    # Preserve NA values unless NA itself was explicitly excluded.
    keep[is.na(keep)] <- TRUE

    x <- x[
      keep,
      ,
      drop = FALSE
    ]
  }

  rownames(x) <- NULL

  x
}

.ilostat_sdmx_extract_constant_filters <- function(
  filters,
  meta
) {

  if (is.null(filters) || !length(filters)) {
    return(
      list(
        filters = filters,
        matches = TRUE
      )
    )
  }

  constants <- c(
    FREQ = as.character(
      meta$dataset$frequency$code
    ),
    MEASURE = as.character(
      meta$sdmx$measure$code
    )
  )

  remaining <- filters
  matches <- TRUE

  for (component in names(constants)) {

    if (!component %in% names(remaining)) {
      next
    }

    requested <- as.character(
      remaining[[component]]
    )

    expected <- constants[[component]]

    if (
      !length(requested) ||
      !expected %in% requested
    ) {
      matches <- FALSE
    }

    remaining[[component]] <- NULL
  }

  list(
    filters = remaining,
    matches = matches
  )
}

# -------------------------------------------------------------------------
# Empty SDMX data result
#
# Used when a dataset-level constant filter cannot match the requested
# dataset, for example:
#
#   FREQ = "Q"
#
# against an annual dataset.
#
# The result contains zero observations but preserves the same SDMX schema
# and column ordering as a non-empty result.
# -------------------------------------------------------------------------

.ilostat_sdmx_empty_data <- function(
  meta,
  components,
  type = "code",
  lang = "en"
) {

  component_ids <- components$component_id

  # BEST_SOURCE is absent for the default best_source = "yes".
  component_ids <- component_ids[
    component_ids != "BEST_SOURCE"
  ]

  x <- as.data.frame(
    setNames(
      replicate(
        length(component_ids),
        character(),
        simplify = FALSE
      ),
      component_ids
    ),
    stringsAsFactors = FALSE
  )

  # Observation values are numeric.
  if ("OBS_VALUE" %in% names(x)) {
    x$OBS_VALUE <- numeric()
  }

  # Apply the requested representation.
  #
  # In particular, type = "both" creates the *_LABEL columns even though
  # there are no observations.
  x <- .ilostat_sdmx_apply_type(
    x,
    meta = meta,
    components = components,
    type = type,
    lang = lang
  )

  # Add STRUCTURE / STRUCTURE_ID / STRUCTURE_NAME / ACTION according to the
  # same rules as a non-empty SDMX result.
  x <- .ilostat_sdmx_add_data_envelope(
    x = x,
    meta = meta,
    type = type,
    lang = lang
  )

  .ilostat_sdmx_order_columns(
    x,
    components
  )
}