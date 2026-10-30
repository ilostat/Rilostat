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

  if (is.null(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    stop(
      "`id` is required for this SDMX resource.",
      call. = FALSE
    )
  }

  cache_file <- .ilostat_sdmx_meta_file(
    id = id,
    cache_dir = cache_dir
  )

  if (refresh || !file.exists(cache_file)) {

    url <- .ilostat_sdmx_meta_url(id)

    tmp <- tempfile(
      pattern = paste0(id, "_meta_"),
      fileext = ".json"
    )

    on.exit(
      unlink(tmp),
      add = TRUE
    )

    status <- try(
      utils::download.file(
        url,
        tmp,
        mode = "wb",
        quiet = TRUE
      ),
      silent = TRUE
    )

    if (
      inherits(status, "try-error") ||
      !file.exists(tmp) ||
      file.info(tmp)$size == 0
    ) {
      stop(
        sprintf(
          "Unable to download SDMX metadata for dataset `%s`.",
          id
        ),
        call. = FALSE
      )
    }

    if (!file.copy(tmp, cache_file, overwrite = TRUE)) {
      stop(
        sprintf(
          "Unable to write SDMX metadata cache for dataset `%s`.",
          id
        ),
        call. = FALSE
      )
    }
  }

  meta <- jsonlite::fromJSON(
    cache_file,
    simplifyVector = FALSE
  )

  if (
    is.null(meta$dataset$id) ||
    !identical(meta$dataset$id, id)
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
      "time_dimension",
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
      "Unit measure",
      "Unit multiplier",
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

  out$position <- seq_len(nrow(out))

  out$representation <- ifelse(
    is.na(out$codelist),
    "string",
    "enumerated"
  )

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

  component <- toupper(component)

  # Dataset dimensions:
  # REF_AREA, SOURCE, SEX and dynamic classifications such as AGE.
  dims <- meta$dimensions

  if (!is.null(dims)) {

    for (name in names(dims)) {

      x <- dims[[name]]

      type <- x$type

      if (is.null(type) || !nzchar(type)) {
        type <- toupper(name)
      }

      if (identical(toupper(type), component)) {
        return(
          .ilostat_sdmx_values_to_df(
            x$values,
            lang = lang
          )
        )
      }
    }
  }

  # Frequency.
  if (component == "FREQ") {

    x <- meta$dataset$frequency

    if (!is.null(x) && !is.null(x$code)) {

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
  }

  # Measure, unit measure and unit multiplier.
  if (component == "MEASURE") {
    x <- meta$sdmx$measure
  } else if (component == "UNIT_MEASURE") {
    x <- meta$sdmx$unit_measure
  } else if (component == "UNIT_MULT") {
    x <- meta$sdmx$unit_mult
  } else {
    x <- NULL
  }

  if (!is.null(x) && !is.null(x$code)) {

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

  # Observation status.
  if (component == "OBS_STATUS") {
    return(
      .ilostat_sdmx_values_to_df(
        meta$observation_status,
        lang = lang
      )
    )
  }

  # Notes.
  if (component == "NOTE_SOURCE") {
    values <- meta$notes$source
  } else if (component == "NOTE_INDICATOR") {
    values <- meta$notes$indicator
  } else if (component == "NOTE_CLASSIF") {
    values <- meta$notes$classif
  } else {
    values <- NULL
  }

  if (!is.null(values)) {
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
  meta,
  component,
  lang = "en"
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

  components <- .ilostat_sdmx_components(
    meta,
    lang = lang
  )

  if (!component %in% components$component_id) {
    stop(
      sprintf(
        "Component `%s` is not available in this dataset.",
        component
      ),
      call. = FALSE
    )
  }

  values <- .ilostat_sdmx_component_values(
    meta = meta,
    component = component,
    lang = lang
  )

  if (is.null(values)) {
    stop(
      sprintf(
        "No dataset codelist values are available for component `%s`.",
        component
      ),
      call. = FALSE
    )
  }

  # Keep the R-facing representation semantic:
  #
  # AGE
  # AGE_LABEL
  #
  # rather than exposing native names such as classif1/classif1.label.
  out <- data.frame(
    values$code,
    values$label,
    stringsAsFactors = FALSE
  )

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