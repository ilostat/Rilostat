#' Retrieve ILOSTAT data using an SDMX-oriented interface
#'
#' `get_ilostat_sdmx()` provides an SDMX-oriented interface to ILOSTAT
#' datasets while reusing the native Rilostat download, disk-cache and
#' filtering infrastructure.
#'
#' The existing [get_ilostat()] function remains the native ILOSTAT
#' interface. `get_ilostat_sdmx()` adds an SDMX semantic layer whose
#' components are derived dynamically from dataset metadata.
#'
#' @param id Character. ILOSTAT dataset identifier, for example
#'   `"UNE_TUNE_SEX_AGE_NB_A"`. Required for `"data"`, `"structure"` and
#'   `"codelist"`. For `resource = "dataflow"`, `id = NULL` returns the
#'   complete ILOSTAT dataflow catalogue.
#'
#' @param resource Character. SDMX resource to return. One of:
#'   `"data"`, `"structure"`, `"codelist"` or `"dataflow"`.
#'
#' @param component Character. SDMX component identifier used with
#'   `resource = "codelist"`, for example `"AGE"`, `"SEX"`,
#'   `"OBS_STATUS"` or `"UNIT_MEASURE"`.
#'
#' @param segment Character. Native ILOSTAT segment passed to
#'   [get_ilostat()] or [get_ilostat_toc()].
#'
#' @param type Character. Representation of coded values. One of:
#'   `"code"`, `"label"`, `"label_short"` or `"both"`.
#'
#'   `"code"` returns SDMX codes.
#'
#'   `"label"` replaces coded values with their labels.
#'
#'   `"label_short"` uses short/display labels when available.
#'
#'   `"both"` returns codes together with companion `*_LABEL` columns.
#'
#' @param format Character. Output format. One of:
#'   `"dataframe"`, `"sdmx-ml"` or `"sdmx-json"`.
#'
#'   `"sdmx-ml"` and `"sdmx-json"` are available for the structural
#'   resources `"structure"`, `"codelist"` and `"dataflow"`.
#'   Observation data are currently returned as a data frame.
#'
#' @param lang Character. Language used for labels. One of `"en"`,
#'   `"fr"` or `"es"`.
#'
#' @param filters Named list of filters using SDMX component identifiers,
#'   for example `REF_AREA`, `SEX`, `AGE`, `FREQ`, `MEASURE` and
#'   `TIME_PERIOD`.
#'
#'   Ordinary component filters reuse native Rilostat matching semantics.
#'   For example, `SEX = "SEX_F"` filters female observations.
#'
#'   `FREQ` and `MEASURE` are dataset-level SDMX constants. A filter that
#'   does not match the dataset returns an empty SDMX data frame.
#'
#'   `TIME_PERIOD` supports the existing Rilostat time filtering semantics.
#'   Equality can be expressed directly, for example
#'   `TIME_PERIOD = "2020"`, while comparison operators can be supplied as
#'   a named list, for example
#'   `TIME_PERIOD = list(ge = "2020", le = "2023")`.
#'
#'   Ordinary coded components also support `eq` and `ne`, for example
#'   `SEX = list(ne = "SEX_T")`.
#'
#' @param best_source Character. Source-selection behavior passed to
#'   [get_ilostat()]. `"yes"` (default) returns best-source observations
#'   and does not expose `BEST_SOURCE`; `"no"` and `"all"` retain the
#'   `BEST_SOURCE` component.
#'
#' @param fixed Logical. Native matching behavior passed to [get_ilostat()].
#'
#' @param cache_dir Character or `NULL`. Directory used for the SDMX
#'   metadata disk cache and native Rilostat cache. If `NULL`, the standard
#'   Rilostat cache location is used.
#'
#' @param refresh Logical. If `TRUE`, refresh cached dataset metadata and
#'   request a refresh of the native ILOSTAT data cache.
#'
#' @return
#' The return value depends on `resource` and `format`.
#'
#' With `format = "dataframe"`, a data frame is returned.
#'
#' With `format = "sdmx-ml"`, a character string containing SDMX-ML 3.1
#' is returned.
#'
#' With `format = "sdmx-json"`, a character string containing SDMX-JSON
#' 2.1 is returned.
#'
#' @details
#' For `resource = "data"`, native ILOSTAT column names are mapped to SDMX
#' component identifiers. Dynamic classifications are obtained from the
#' dataset metadata rather than hard-coded. For example, a native
#' `classif1` column can become `AGE` when the dataset metadata identifies
#' that dimension as age.
#'
#' The data resource includes the SDMX message envelope components
#' `STRUCTURE`, `STRUCTURE_ID` and `ACTION`. `STRUCTURE_NAME` is also
#' returned when labels are requested.
#'
#' Dataset-level components such as `FREQ`, `MEASURE`, `UNIT_MEASURE` and
#' `UNIT_MULT` are derived from the dataset SDMX metadata.
#'
#' `resource = "codelist"` returns the canonical ILOSTAT codelist associated
#' with the requested SDMX component.
#'
#' @examples
#' \dontrun{
#'
#' # Observation data using SDMX component names
#' x <- get_ilostat_sdmx(
#'   "UNE_TUNE_SEX_AGE_NB_A",
#'   filters = list(
#'     REF_AREA = "FRA",
#'     SEX = "SEX_F",
#'     AGE = "AGE_YTHADULT_YGE15"
#'   )
#' )
#'
#' # Codes and labels
#' x <- get_ilostat_sdmx(
#'   "UNE_TUNE_SEX_AGE_NB_A",
#'   type = "both",
#'   filters = list(
#'     REF_AREA = "FRA",
#'     SEX = "SEX_F"
#'   )
#' )
#'
#' # Time range
#' x <- get_ilostat_sdmx(
#'   "UNE_TUNE_SEX_AGE_NB_A",
#'   filters = list(
#'     REF_AREA = "FRA",
#'     TIME_PERIOD = list(
#'       ge = "2020",
#'       le = "2023"
#'     )
#'   )
#' )
#'
#' # Dataset structure
#' structure <- get_ilostat_sdmx(
#'   "UNE_TUNE_SEX_AGE_NB_A",
#'   resource = "structure"
#' )
#'
#' # Canonical AGE codelist
#' age <- get_ilostat_sdmx(
#'   "UNE_TUNE_SEX_AGE_NB_A",
#'   resource = "codelist",
#'   component = "AGE"
#' )
#'
#' # One Dataflow
#' flow <- get_ilostat_sdmx(
#'   "UNE_TUNE_SEX_AGE_NB_A",
#'   resource = "dataflow"
#' )
#'
#' # Complete ILOSTAT Dataflow catalogue
#' flows <- get_ilostat_sdmx(
#'   resource = "dataflow"
#' )
#'
#' # SDMX-ML 3.1 structure message
#' xml <- get_ilostat_sdmx(
#'   "UNE_TUNE_SEX_AGE_NB_A",
#'   resource = "structure",
#'   format = "sdmx-ml"
#' )
#'
#' # SDMX-JSON 2.1 structure message
#' json <- get_ilostat_sdmx(
#'   "UNE_TUNE_SEX_AGE_NB_A",
#'   resource = "structure",
#'   format = "sdmx-json"
#' )
#' }
#'
#' @export


get_ilostat_sdmx <- function(
  id = NULL,
  resource = "data",
  component = NULL,
  segment = "indicator",
  type = "code",
  format = "dataframe",
  lang = "en",
  filters = NULL,
  best_source = "yes",
  fixed = TRUE,
  cache_dir = NULL,
  refresh = FALSE
) {

  resource <- .ilostat_sdmx_match_arg(
    resource,
    .ilostat_sdmx_resources,
    "resource"
  )

  type <- .ilostat_sdmx_match_arg(
    type,
    .ilostat_sdmx_types,
    "type"
  )

  format <- .ilostat_sdmx_match_arg(
    format,
    .ilostat_sdmx_formats,
    "format"
  )

  lang <- .ilostat_sdmx_validate_lang(lang)

  if (resource == "dataflow") {

    dataflow_data <- .ilostat_sdmx_dataflow_dataframe(
      id = id,
      segment = segment,
      lang = lang,
      cache_dir = cache_dir,
      refresh = refresh
    )

    if (format == "dataframe") {
      return(dataflow_data)
    }

    if (format == "sdmx-ml") {
      return(
        .ilostat_sdmx_to_xml_dataflow(
          data = dataflow_data,
          lang = lang
        )
      )
    }

    if (format == "sdmx-json") {

      return(
        .ilostat_sdmx_to_json_dataflow(
          data = dataflow_data,
          lang = lang
        )
      )
    }

    stop(
      sprintf(
        "`format = \"%s\"` is not implemented yet for dataflow.",
        format
      ),
      call. = FALSE
    )
  }

  if (is.null(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    stop(
      "`id` is required for this SDMX resource.",
      call. = FALSE
    )
  }

  meta <- .ilostat_sdmx_get_meta(
    id = id,
    cache_dir = cache_dir,
    refresh = refresh
  )

  if (resource == "structure") {

    if (format == "dataframe") {
      return(
        .ilostat_sdmx_structure_dataframe(
          meta = meta,
          lang = lang
        )
      )
    }

    if (format == "sdmx-ml") {

      model <- .ilostat_sdmx_structure_model(
        id = id,
        meta = meta,
        lang = lang
      )

      return(
        .ilostat_sdmx_to_xml_structure(
          model = model,
          meta = meta,
          lang = lang
        )
      )
    }

    if (format == "sdmx-json") {

      model <- .ilostat_sdmx_structure_model(
        id = id,
        meta = meta,
        lang = lang
      )

      return(
        .ilostat_sdmx_to_json_structure(
          model = model,
          meta = meta,
          lang = lang
        )
      )
    }

    stop(
      sprintf(
        "`format = \"%s\"` is not implemented yet for structure.",
        format
      ),
      call. = FALSE
    )
  }

  if (resource == "codelist") {

    codelist_data <- .ilostat_sdmx_codelist_dataframe(
      id = id,
      meta = meta,
      component = component,
      lang = lang,
      cache_dir = cache_dir,
      refresh = refresh
    )

    if (format == "dataframe") {
      return(codelist_data)
    }

    if (format == "sdmx-ml") {

      component_id <- toupper(component)

      components <- .ilostat_sdmx_components(
        meta = meta,
        lang = lang
      )

      i <- match(
        component_id,
        components$component_id
      )

      if (is.na(i)) {
        stop(
          sprintf(
            "Unknown SDMX component `%s` for dataset `%s`.",
            component_id,
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
            component_id
          ),
          call. = FALSE
        )
      }

      return(
        .ilostat_sdmx_to_xml_codelist(
          data = codelist_data,
          component_id = component_id,
          codelist = codelist,
          lang = lang
        )
      )
    }

    if (format == "sdmx-json") {

      component_id <- toupper(component)

      components <- .ilostat_sdmx_components(
        meta = meta,
        lang = lang
      )

      i <- match(
        component_id,
        components$component_id
      )

      if (is.na(i)) {
        stop(
          sprintf(
            "Unknown SDMX component `%s` for dataset `%s`.",
            component_id,
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
            component_id
          ),
          call. = FALSE
        )
      }

      return(
        .ilostat_sdmx_to_json_codelist(
          data = codelist_data,
          component_id = component_id,
          codelist = codelist,
          lang = lang
        )
      )
    }

    stop(
      sprintf(
        "`format = \"%s\"` is not implemented yet for codelist.",
        format
      ),
      call. = FALSE
    )
  }

  .ilostat_sdmx_get_data(
    id = id,
    meta = meta,
    segment = segment,
    type = type,
    lang = lang,
    filters = filters,
    best_source = best_source,
    fixed = fixed,
    cache_dir = cache_dir,
    refresh = refresh
  )
}

# -------------------------------------------------------------------------
# Dataframe transformation
# -------------------------------------------------------------------------

.ilostat_sdmx_scalar <- function(x, default = NA_character_) {

  if (is.null(x) || length(x) == 0L) {
    return(default)
  }

  as.character(x[[1L]])
}


.ilostat_sdmx_add_constant_components <- function(
  x,
  meta
) {

  n <- nrow(x)

  if (!"FREQ" %in% names(x)) {
    x$FREQ <- rep(
      .ilostat_sdmx_scalar(meta$dataset$frequency$code),
      n
    )
  }

  if (!"MEASURE" %in% names(x)) {
    x$MEASURE <- rep(
      .ilostat_sdmx_scalar(meta$sdmx$measure$code),
      n
    )
  }

  if (!"UNIT_MEASURE" %in% names(x)) {
    x$UNIT_MEASURE <- rep(
      .ilostat_sdmx_scalar(meta$sdmx$unit_measure$code),
      n
    )
  }

  if (!"UNIT_MULT" %in% names(x)) {
    x$UNIT_MULT <- rep(
      .ilostat_sdmx_scalar(meta$sdmx$unit_mult$code),
      n
    )
  }

  x
}


.ilostat_sdmx_rename_native_columns <- function(
  x,
  components
) {

  for (i in seq_len(nrow(components))) {

    native <- components$native_name[[i]]
    sdmx <- components$component_id[[i]]

    if (
      !is.na(native) &&
      nzchar(native) &&
      native %in% names(x)
    ) {
      names(x)[names(x) == native] <- sdmx
    }
  }

  x
}


.ilostat_sdmx_lookup_values <- function(
  values,
  dictionary,
  field
) {

  if (is.null(dictionary)) {
    return(as.character(values))
  }

  pos <- match(
    as.character(values),
    dictionary$code
  )

  result <- dictionary[[field]][pos]

  missing <- is.na(result)

  result[missing] <- as.character(values)[missing]

  result
}


.ilostat_sdmx_apply_type <- function(
  x,
  meta,
  components,
  type,
  lang
) {

  if (type == "code") {
    return(x)
  }

  # At this stage labels are obtained from dataset metadata.
  # This is particularly important for dynamic classification dimensions.
  for (component in components$component_id) {

    if (!component %in% names(x)) {
      next
    }

    dictionary <- .ilostat_sdmx_component_values(
      meta = meta,
      component = component,
      lang = lang
    )

    if (is.null(dictionary)) {
      next
    }

    codes <- x[[component]]

    labels <- .ilostat_sdmx_lookup_values(
      codes,
      dictionary,
      "label"
    )

    labels_short <- .ilostat_sdmx_lookup_values(
      codes,
      dictionary,
      "label_short"
    )

    if (type == "label") {

      x[[component]] <- labels

    } else if (type == "label_short") {

      x[[component]] <- labels_short

    } else if (type == "both") {

      x[[paste0(component, "_LABEL")]] <- labels
    }
  }

  x
}


.ilostat_sdmx_order_columns <- function(
  x,
  components
) {

  # -----------------------------------------------------------------------
  # SDMX message envelope
  #
  # pyilostat order:
  #
  # code:
  #   STRUCTURE, STRUCTURE_ID, ACTION
  #
  # both:
  #   STRUCTURE, STRUCTURE_ID, STRUCTURE_NAME, ACTION
  # -----------------------------------------------------------------------

  envelope <- c(
    "STRUCTURE",
    "STRUCTURE_ID",
    "STRUCTURE_NAME",
    "ACTION"
  )

  envelope <- envelope[
    envelope %in% names(x)
  ]

  # -----------------------------------------------------------------------
  # SDMX components
  #
  # Keep component order from the structure model and place an associated
  # *_LABEL column immediately after its code column.
  #
  # Example:
  #   REF_AREA, REF_AREA_LABEL,
  #   SOURCE, SOURCE_LABEL,
  #   FREQ, FREQ_LABEL,
  #   ...
  # -----------------------------------------------------------------------

  component_columns <- character()

  for (component in components$component_id) {

    if (!component %in% names(x)) {
      next
    }

    component_columns <- c(
      component_columns,
      component
    )

    label <- paste0(
      component,
      "_LABEL"
    )

    if (label %in% names(x)) {
      component_columns <- c(
        component_columns,
        label
      )
    }
  }

  # -----------------------------------------------------------------------
  # Any columns not represented by the SDMX structure are retained at the
  # end rather than silently discarded.
  # -----------------------------------------------------------------------

  other <- setdiff(
    names(x),
    c(
      envelope,
      component_columns
    )
  )

  x[
    c(
      envelope,
      component_columns,
      other
    )
  ]
}


.ilostat_sdmx_get_data <- function(
  id,
  meta,
  segment,
  type,
  lang,
  filters,
  best_source,
  fixed,
  cache_dir,
  refresh
) {

  # -----------------------------------------------------------------------
  # SDMX component model
  # -----------------------------------------------------------------------

  components <- .ilostat_sdmx_components(
    meta,
    lang = lang
  )

  # -----------------------------------------------------------------------
  # Dataset-level constant filters
  #
  # FREQ and MEASURE are dataset-level constants rather than native
  # observation columns.
  #
  # Example for UNE_TUNE_SEX_AGE_NB_A:
  #   FREQ    = A
  #   MEASURE = UNE_TUNE_NB
  #
  # Matching constant filters are removed before translating the remaining
  # filters to native ILOSTAT dimensions.
  # -----------------------------------------------------------------------
  constant_filters <- .ilostat_sdmx_extract_constant_filters(
    filters = filters,
    meta = meta
  )

  filters <- constant_filters$filters

  if (!constant_filters$matches) {

    return(
      .ilostat_sdmx_empty_data(
        meta = meta,
        components = components,
        type = type,
        lang = lang
      )
    )
  }

  # Translate SDMX TIME_PERIOD semantics to the existing native
  # Rilostat time / timefrom / timeto filter mechanism.
  time_filter <- .ilostat_sdmx_extract_time_filter(
    filters
  )

  filters <- time_filter$filters

  operator_filters <- .ilostat_sdmx_extract_component_operators(
    filters
  )

  filters <- operator_filters$filters

  native_filters <- .ilostat_sdmx_translate_filters(
    filters = filters,
    components = components
  )

  if (!is.null(time_filter$time)) {
    native_filters$time <- time_filter$time
  }

  if (!is.null(time_filter$timefrom)) {
    native_filters$timefrom <- time_filter$timefrom
  }

  if (!is.null(time_filter$timeto)) {
    native_filters$timeto <- time_filter$timeto
  }
  
  # -----------------------------------------------------------------------
  # Native ILOSTAT request
  #
  # Always retrieve native codes here. The SDMX layer applies the requested
  # code / label / label_short / both representation afterwards.
  # -----------------------------------------------------------------------

  args <- list(
    id = id,
    segment = segment,
    type = "code",
    lang = lang,
    filters = native_filters,
    best_source = best_source,
    fixed = fixed
  )

  if (!is.null(cache_dir)) {
    args$cache_dir <- cache_dir
  }

  # Existing get_ilostat() uses cache_update for forced refresh.
  if (refresh) {
    args$cache_update <- TRUE
  }

  x <- do.call(
    get_ilostat,
    args
  )

  # -----------------------------------------------------------------------
  # Work on an ordinary data.frame
  #
  # Do not allow the SDMX layer to modify a data.table by reference.
  # -----------------------------------------------------------------------

  x <- as.data.frame(
    x,
    stringsAsFactors = FALSE
  )

  # -----------------------------------------------------------------------
  # Remove native physical indicator
  #
  # The dataset ID and SDMX MEASURE component already carry the indicator /
  # measure semantics. The native ILOSTAT `indicator` column must therefore
  # not leak into the SDMX representation.
  # -----------------------------------------------------------------------

  if ("indicator" %in% names(x)) {
    x$indicator <- NULL
  }

  # -----------------------------------------------------------------------
  # Native -> SDMX semantic component names
  #
  # Examples:
  #   ref_area  -> REF_AREA
  #   source    -> SOURCE
  #   sex       -> SEX
  #   classif1  -> AGE
  #   time      -> TIME_PERIOD
  #   obs_value -> OBS_VALUE
  # -----------------------------------------------------------------------

  x <- .ilostat_sdmx_rename_native_columns(
    x,
    components
  )

  x <- .ilostat_sdmx_apply_exclusions(
    x = x,
    exclude = operator_filters$exclude
  )

  # -----------------------------------------------------------------------
  # SDMX time representation
  #
  # Examples:
  #   2024     -> 2024
  #   2024Q1   -> 2024-Q1
  #   2024M01  -> 2024-M01
  # -----------------------------------------------------------------------

  if ("TIME_PERIOD" %in% names(x)) {

    x$TIME_PERIOD <- .ilostat_sdmx_format_time_period(
      x$TIME_PERIOD
    )
  }

  # -----------------------------------------------------------------------
  # Dataset-level constant SDMX components
  #
  # FREQ, MEASURE, UNIT_MEASURE and UNIT_MULT come from dataset metadata.
  # -----------------------------------------------------------------------

  x <- .ilostat_sdmx_add_constant_components(
    x,
    meta
  )

  # -----------------------------------------------------------------------
  # Apply requested representation
  #
  # code
  # label
  # label_short
  # both
  # -----------------------------------------------------------------------

  x <- .ilostat_sdmx_apply_type(
    x,
    meta = meta,
    components = components,
    type = type,
    lang = lang
  )

# -----------------------------------------------------------------------
# SDMX data envelope
#
# Match pyilostat:
#
# STRUCTURE_NAME is included whenever labels are requested:
#   label
#   label_short
#   both
#
# It is absent for:
#   code
#
# STRUCTURE_ID uses the formal Dataflow ID from metadata:
#   ILO:DF_<dataset_id>(1.0)
# -----------------------------------------------------------------------
  
  x <- .ilostat_sdmx_add_data_envelope(
    x = x,
    meta = meta,
    type = type,
    lang = lang
  )

  # -----------------------------------------------------------------------
  # Final SDMX column ordering
  # -----------------------------------------------------------------------

  .ilostat_sdmx_order_columns(
    x,
    components
  )
}