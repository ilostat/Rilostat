#' Retrieve ILOSTAT data using an SDMX-oriented interface
#'
#' `get_ilostat_sdmx()` provides an SDMX-oriented view of ILOSTAT datasets
#' while reusing the native Rilostat download, cache and filtering
#' infrastructure.
#'
#' The existing [get_ilostat()] function remains the native ILOSTAT
#' interface. This function adds semantic SDMX component names derived
#' dynamically from dataset metadata.
#'
#' @param id Character. ILOSTAT dataset identifier.
#' @param resource Character. Resource to return. Currently `"data"` and
#'   `"structure"` are implemented.
#' @param component Character. SDMX component identifier. Reserved for
#'   resources such as `"codelist"`.
#' @param segment Character. ILOSTAT segment passed to [get_ilostat()].
#' @param type Character. Representation of coded values: `"code"`,
#'   `"label"`, `"label_short"` or `"both"`.
#' @param format Character. Output format. Currently `"dataframe"` is
#'   implemented.
#' @param lang Character. Language: `"en"`, `"fr"` or `"es"`.
#' @param filters Named list of filters using SDMX component names such as
#'   `REF_AREA`, `SEX` and `AGE`.
#' @param best_source Character. Passed to [get_ilostat()].
#' @param fixed Logical. Passed to [get_ilostat()].
#' @param cache_dir Character. Cache directory. If `NULL`, uses the
#'   standard Rilostat temporary cache.
#' @param refresh Logical. If `TRUE`, refresh cached dataset metadata and
#'   native ILOSTAT data.
#'
#' @return A data frame.
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

  if (format != "dataframe") {
    stop(
      sprintf(
        "`format = \"%s\"` is not implemented yet.",
        format
      ),
      call. = FALSE
    )
  }

  if (resource == "dataflow") {

    return(
      .ilostat_sdmx_dataflow_dataframe(
        id = id,
        segment = segment,
        lang = lang,
        cache_dir = cache_dir,
        refresh = refresh
      )
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
    return(
      .ilostat_sdmx_structure_dataframe(
        meta = meta,
        lang = lang
      )
    )
  }

  if (resource == "codelist") {
    return(
      .ilostat_sdmx_codelist_dataframe(
        meta = meta,
        component = component,
        lang = lang
      )
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

  preferred <- components$component_id

  preferred <- preferred[
    preferred %in% names(x)
  ]

  label_columns <- paste0(
    preferred,
    "_LABEL"
  )

  label_columns <- label_columns[
    label_columns %in% names(x)
  ]

  other <- setdiff(
    names(x),
    c(preferred, label_columns)
  )

  x[
    c(
      preferred,
      label_columns,
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

  components <- .ilostat_sdmx_components(
    meta,
    lang = lang
  )

  native_filters <- .ilostat_sdmx_translate_filters(
    filters = filters,
    components = components
  )

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

  # Work on an ordinary data.frame so the SDMX layer does not modify a
  # data.table by reference.
  x <- as.data.frame(
    x,
    stringsAsFactors = FALSE
  )

# The dataset ID already determines the indicator/measure semantics.
# Do not expose the native ILOSTAT indicator column in the SDMX view.
if ("indicator" %in% names(x)) {
  x$indicator <- NULL
}

  x <- .ilostat_sdmx_rename_native_columns(
    x,
    components
  )

  x <- .ilostat_sdmx_add_constant_components(
    x,
    meta
  )

  x <- .ilostat_sdmx_apply_type(
    x,
    meta = meta,
    components = components,
    type = type,
    lang = lang
  )

  .ilostat_sdmx_order_columns(
    x,
    components
  )
}