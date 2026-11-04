# Internal SDMX-JSON 2.1 serialization helpers.


# -------------------------------------------------------------------------
# SDMX-JSON 2.1 structure
#
# R equivalent of pyilostat _to_sdmx_json_structure().
# -------------------------------------------------------------------------

.ilostat_sdmx_to_json_structure <- function(
  model,
  meta,
  lang = "en"
) {

  dimensions <- list()
  attributes <- list()
  measures <- list()
  time_dimension <- NULL

  for (component in model$components) {

    concept_urn <- paste0(
      "urn:sdmx:org.sdmx.infomodel.",
      "conceptscheme.Concept=",
      model$agency,
      ":",
      model$concept_scheme,
      "(",
      model$version,
      ").",
      component$id
    )

    item <- list(
      id = component$id,
      conceptIdentity = concept_urn
    )

    # ---------------------------------------------------------------------
    # Coded representation
    # ---------------------------------------------------------------------

    if (!is.null(component$codelist)) {

      item$localRepresentation <- list(
        enumeration = paste0(
          "urn:sdmx:org.sdmx.infomodel.",
          "codelist.Codelist=",
          model$agency,
          ":",
          toupper(component$codelist),
          "(",
          model$version,
          ")"
        )
      )
    }

    # ---------------------------------------------------------------------
    # Dimensions
    # ---------------------------------------------------------------------

    if (component$role == "dimension") {

      if (component$id == "TIME_PERIOD") {

        item$localRepresentation <- list(
          format = list(
            dataType = "ObservationalTimePeriod"
          )
        )

        time_dimension <- item

      } else {

        item$position <- component$position

        dimensions[[length(dimensions) + 1L]] <- item
      }

    # ---------------------------------------------------------------------
    # Measure
    # ---------------------------------------------------------------------

    } else if (component$role == "measure") {

      measures[[length(measures) + 1L]] <- item

    # ---------------------------------------------------------------------
    # Attributes
    # ---------------------------------------------------------------------

    } else if (component$role == "attribute") {

      item$usage <- "optional"

      item$attributeRelationship <- list(
        observation = list()
      )

      attributes[[length(attributes) + 1L]] <- item
    }
  }

  # -----------------------------------------------------------------------
  # Concepts
  # -----------------------------------------------------------------------

  concepts <- lapply(
    model$components,
    function(component) {

      out <- list(
        id = component$id,
        name = component$name
      )

      out$names <- setNames(
        list(component$name),
        lang
      )

      out
    }
  )

  # -----------------------------------------------------------------------
  # DataStructure
  # -----------------------------------------------------------------------

  dsd_name <- paste0(
    "Data structure for ",
    model$id
  )

  dsd_names <- setNames(
    list(dsd_name),
    lang
  )

  concept_scheme_name <- paste0(
    "Concept scheme for ",
    model$id
  )

  concept_scheme_names <- setNames(
    list(concept_scheme_name),
    lang
  )

  message <- list(
    `$schema` = paste0(
      "https://json.sdmx.org/2.1/",
      "sdmx-json-structure-schema.json"
    ),
    meta = list(
      id = paste0(
        "STRUCTURE_",
        model$id
      ),
      test = FALSE,
      contentLanguages = list(lang),
      sender = list(
        id = model$agency
      )
    ),
    data = list(
      dataStructures = list(
        list(
          id = model$id,
          version = model$version,
          agencyID = model$agency,
          name = dsd_name,
          names = dsd_names,
          dataStructureComponents = list(
            attributeList = list(
              id = "AttributeDescriptor",
              attributes = attributes
            ),
            dimensionList = list(
              id = "DimensionDescriptor",
              dimensions = dimensions,
              timeDimension = time_dimension
            ),
            measureList = list(
              id = "MeasureDescriptor",
              measures = measures
            )
          )
        )
      ),
      conceptSchemes = list(
        list(
          id = model$concept_scheme,
          version = model$version,
          agencyID = model$agency,
          name = concept_scheme_name,
          names = concept_scheme_names,
          concepts = concepts
        )
      )
    )
  )

  jsonlite::toJSON(
    message,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )
}

# -------------------------------------------------------------------------
# SDMX-JSON 2.1 codelist
# -------------------------------------------------------------------------

.ilostat_sdmx_to_json_codelist <- function(
  data,
  component_id,
  codelist,
  lang = "en"
) {

  component_id <- toupper(component_id)

  code_column <- component_id
  label_column <- paste0(
    component_id,
    "_LABEL"
  )

  if (
    !code_column %in% names(data) ||
    !label_column %in% names(data)
  ) {
    stop(
      sprintf(
        "Codelist data must contain `%s` and `%s`.",
        code_column,
        label_column
      ),
      call. = FALSE
    )
  }

  codes <- vector(
    "list",
    nrow(data)
  )

  for (i in seq_len(nrow(data))) {

    code <- as.character(
      data[[code_column]][[i]]
    )

    label <- as.character(
      data[[label_column]][[i]]
    )

    codes[[i]] <- list(
      id = code,
      name = label,
      names = setNames(
        list(label),
        lang
      )
    )
  }

  codelist_name <- paste0(
    component_id,
    " codelist"
  )

  message <- list(
    `$schema` = paste0(
      "https://json.sdmx.org/2.1/",
      "sdmx-json-structure-schema.json"
    ),
    meta = list(
      id = paste0(
        "CODELIST_",
        component_id
      ),
      test = FALSE,
      contentLanguages = list(lang),
      sender = list(
        id = "ILO"
      )
    ),
    data = list(
      codelists = list(
        list(
          id = toupper(codelist),
          version = "1.0",
          agencyID = "ILO",
          name = codelist_name,
          names = setNames(
            list(codelist_name),
            lang
          ),
          codes = codes
        )
      )
    )
  )

  jsonlite::toJSON(
    message,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )
}

# -------------------------------------------------------------------------
# SDMX-JSON 2.1 dataflow
#
# R equivalent of pyilostat Dataflow SDMX-JSON serialization.
# -------------------------------------------------------------------------

.ilostat_sdmx_to_json_dataflow <- function(
  data,
  lang = "en"
) {

  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data.frame.",
      call. = FALSE
    )
  }

  required <- c(
    "DATAFLOW_ID",
    "DATAFLOW_NAME"
  )

  missing <- setdiff(
    required,
    names(data)
  )

  if (length(missing)) {
    stop(
      sprintf(
        "Dataflow data is missing required column(s): %s.",
        paste(
          missing,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  dataflows <- vector(
    "list",
    nrow(data)
  )

  for (i in seq_len(nrow(data))) {

    dataflow_id <- as.character(
      data$DATAFLOW_ID[[i]]
    )

    dataflow_name <- as.character(
      data$DATAFLOW_NAME[[i]]
    )

    dataflows[[i]] <- list(
      id = dataflow_id,
      version = "1.0",
      agencyID = "ILO",
      name = dataflow_name,
      names = setNames(
        list(dataflow_name),
        lang
      ),
      structure = paste0(
        "urn:sdmx:org.sdmx.infomodel.",
        "datastructure.DataStructure=",
        "ILO:",
        dataflow_id,
        "(1.0)"
      )
    )
  }

  message <- list(
    `$schema` = paste0(
      "https://json.sdmx.org/2.1/",
      "sdmx-json-structure-schema.json"
    ),
    meta = list(
      id = "ILOSTAT_DATAFLOWS",
      test = FALSE,
      contentLanguages = list(lang),
      sender = list(
        id = "ILO"
      )
    ),
    data = list(
      dataflows = dataflows
    )
  )

  jsonlite::toJSON(
    message,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )
}
