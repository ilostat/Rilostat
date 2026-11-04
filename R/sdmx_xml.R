# Internal SDMX-ML 3.1 serialization helpers.


# -------------------------------------------------------------------------
# Namespaces
# -------------------------------------------------------------------------

.ilostat_sdmx_xml_ns <- c(
  mes = paste0(
    "http://www.sdmx.org/resources/",
    "sdmxml/schemas/v3_1/message"
  ),
  str = paste0(
    "http://www.sdmx.org/resources/",
    "sdmxml/schemas/v3_1/structure"
  ),
  com = paste0(
    "http://www.sdmx.org/resources/",
    "sdmxml/schemas/v3_1/common"
  )
)


# -------------------------------------------------------------------------
# Internal structure model
#
# Direct equivalent of pyilostat _build_sdmx_structure_model().
# -------------------------------------------------------------------------

.ilostat_sdmx_structure_model <- function(
  id,
  meta,
  lang = "en"
) {

  structure <- .ilostat_sdmx_structure_dataframe(
    meta = meta,
    lang = lang
  )

  components <- lapply(
    seq_len(nrow(structure)),
    function(i) {

      position <- structure$POSITION[[i]]

      if (is.na(position)) {
        position <- NULL
      } else {
        position <- as.integer(position)
      }

      codelist <- structure$CODELIST[[i]]

      if (is.na(codelist)) {
        codelist <- NULL
      }

      list(
        id = structure$COMPONENT_ID[[i]],
        name = structure$COMPONENT_NAME[[i]],
        role = structure$ROLE[[i]],
        position = position,
        representation = structure$REPRESENTATION[[i]],
        codelist = codelist,
        attachment = if (
          structure$ROLE[[i]] == "attribute"
        ) {
          "observation"
        } else {
          NULL
        }
      )
    }
  )

  list(
    id = id,
    agency = "ILO",
    version = "1.0",
    concept_scheme = paste0("CS_", id),
    components = components
  )
}


# -------------------------------------------------------------------------
# XML utilities
# -------------------------------------------------------------------------

.ilostat_sdmx_xml_prepared <- function() {

  format(
    as.POSIXct(Sys.time(), tz = "UTC"),
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
}


.ilostat_sdmx_xml_urn_concept <- function(
  model,
  component_id
) {

  paste0(
    "urn:sdmx:org.sdmx.infomodel.",
    "conceptscheme.Concept=",
    model$agency,
    ":",
    model$concept_scheme,
    "(",
    model$version,
    ").",
    component_id
  )
}


.ilostat_sdmx_xml_urn_codelist <- function(
  model,
  codelist
) {

  paste0(
    "urn:sdmx:org.sdmx.infomodel.",
    "codelist.Codelist=",
    model$agency,
    ":",
    toupper(codelist),
    "(",
    model$version,
    ")"
  )
}


# -------------------------------------------------------------------------
# SDMX-ML 3.1 structure
#
# Direct R equivalent of pyilostat _to_sdmx_ml_structure().
# -------------------------------------------------------------------------
.ilostat_sdmx_to_xml_structure <- function(
  model,
  meta,
  lang = "en"
) {

  ns <- .ilostat_sdmx_xml_ns

  # -----------------------------------------------------------------------
  # Root document and namespaces
  # -----------------------------------------------------------------------

  doc <- xml2::read_xml(
    paste0(
      '<mes:Structure',
      ' xmlns:mes="', ns[["mes"]], '"',
      ' xmlns:str="', ns[["str"]], '"',
      ' xmlns:com="', ns[["com"]], '"',
      '></mes:Structure>'
    )
  )

  root <- xml2::xml_root(doc)

  # -----------------------------------------------------------------------
  # Header
  # -----------------------------------------------------------------------

  header <- xml2::xml_add_child(
    root,
    "mes:Header"
  )

  xml2::xml_add_child(
    header,
    "mes:ID",
    paste0(
      "STRUCTURE_",
      model$id
    )
  )

  xml2::xml_add_child(
    header,
    "mes:Test",
    "false"
  )

  xml2::xml_add_child(
    header,
    "mes:Prepared",
    .ilostat_sdmx_xml_prepared()
  )

  sender <- xml2::xml_add_child(
    header,
    "mes:Sender",
    id = model$agency
  )

  xml2::xml_add_child(
    sender,
    "com:Name",
    "International Labour Organization",
    `xml:lang` = "en"
  )

  # -----------------------------------------------------------------------
  # Structures
  # -----------------------------------------------------------------------

  structures <- xml2::xml_add_child(
    root,
    "mes:Structures"
  )

  # -----------------------------------------------------------------------
  # ConceptScheme
  # -----------------------------------------------------------------------

  concept_schemes <- xml2::xml_add_child(
    structures,
    "str:ConceptSchemes"
  )

  concept_scheme <- xml2::xml_add_child(
    concept_schemes,
    "str:ConceptScheme",
    id = model$concept_scheme,
    agencyID = model$agency,
    version = model$version
  )

  xml2::xml_add_child(
    concept_scheme,
    "com:Name",
    paste0(
      "Concept scheme for ",
      model$id
    ),
    `xml:lang` = lang
  )

  # -----------------------------------------------------------------------
  # Concepts
  # -----------------------------------------------------------------------

  for (component in model$components) {

    concept <- xml2::xml_add_child(
      concept_scheme,
      "str:Concept",
      id = component$id
    )

    xml2::xml_add_child(
      concept,
      "com:Name",
      component$name,
      `xml:lang` = lang
    )
  }

  # -----------------------------------------------------------------------
  # DataStructure
  # -----------------------------------------------------------------------

  data_structures <- xml2::xml_add_child(
    structures,
    "str:DataStructures"
  )

  data_structure <- xml2::xml_add_child(
    data_structures,
    "str:DataStructure",
    id = model$id,
    agencyID = model$agency,
    version = model$version
  )

  xml2::xml_add_child(
    data_structure,
    "com:Name",
    paste0(
      "Data structure for ",
      model$id
    ),
    `xml:lang` = lang
  )

  # -----------------------------------------------------------------------
  # DataStructureComponents
  # -----------------------------------------------------------------------

  components_element <- xml2::xml_add_child(
    data_structure,
    "str:DataStructureComponents"
  )

  dimensions <- xml2::xml_add_child(
    components_element,
    "str:DimensionList"
  )

  attributes <- xml2::xml_add_child(
    components_element,
    "str:AttributeList"
  )

  measures <- xml2::xml_add_child(
    components_element,
    "str:MeasureList"
  )

  # -----------------------------------------------------------------------
  # Components
  # -----------------------------------------------------------------------

  for (component in model$components) {

    role <- component$role

    # ---------------------------------------------------------------------
    # Determine SDMX component element
    # ---------------------------------------------------------------------

    if (role == "dimension") {

      parent <- dimensions

      if (component$id == "TIME_PERIOD") {
        element_name <- "str:TimeDimension"
      } else {
        element_name <- "str:Dimension"
      }

    } else if (role == "measure") {

      parent <- measures
      element_name <- "str:Measure"

    } else if (role == "attribute") {

      parent <- attributes
      element_name <- "str:Attribute"

    } else {

      next
    }

    component_element <- xml2::xml_add_child(
      parent,
      element_name,
      id = component$id
    )

    # ---------------------------------------------------------------------
    # Position
    #
    # Ordinary dimensions have positions.
    # TIME_PERIOD, measures and attributes do not expose a position
    # attribute in the SDMX-ML representation.
    # ---------------------------------------------------------------------

    if (
      !is.null(component$position) &&
      component$id != "TIME_PERIOD"
    ) {

      xml2::xml_set_attr(
        component_element,
        "position",
        as.character(component$position)
      )
    }

    # ---------------------------------------------------------------------
    # ConceptIdentity
    # ---------------------------------------------------------------------

    xml2::xml_add_child(
      component_element,
      "str:ConceptIdentity",
      .ilostat_sdmx_xml_urn_concept(
        model = model,
        component_id = component$id
      )
    )

    # ---------------------------------------------------------------------
    # TIME_PERIOD representation
    # ---------------------------------------------------------------------

    if (component$id == "TIME_PERIOD") {

      local_representation <- xml2::xml_add_child(
        component_element,
        "str:LocalRepresentation"
      )

      xml2::xml_add_child(
        local_representation,
        "str:TextFormat",
        textType = "ObservationalTimePeriod"
      )
    }

    # ---------------------------------------------------------------------
    # Coded representation
    # ---------------------------------------------------------------------

    if (!is.null(component$codelist)) {

      local_representation <- xml2::xml_add_child(
        component_element,
        "str:LocalRepresentation"
      )

      xml2::xml_add_child(
        local_representation,
        "str:Enumeration",
        .ilostat_sdmx_xml_urn_codelist(
          model = model,
          codelist = component$codelist
        )
      )
    }

    # ---------------------------------------------------------------------
    # AttributeRelationship
    #
    # pyilostat attaches these dataset attributes to observations.
    # ---------------------------------------------------------------------

    if (
      role == "attribute" &&
      identical(
        component$attachment,
        "observation"
      )
    ) {

      relationship <- xml2::xml_add_child(
        component_element,
        "str:AttributeRelationship"
      )

      xml2::xml_add_child(
        relationship,
        "str:Observation"
      )
    }
  }

  # -----------------------------------------------------------------------
  # Serialize
  # -----------------------------------------------------------------------

  as.character(doc)
}

# -------------------------------------------------------------------------
# SDMX-ML 3.1 codelist
#
# R equivalent of pyilostat _to_sdmx_ml_codelist().
# -------------------------------------------------------------------------

.ilostat_sdmx_to_xml_codelist <- function(
  data,
  component_id,
  codelist,
  lang = "en"
) {

  ns <- .ilostat_sdmx_xml_ns

  component_id <- toupper(component_id)

  # -----------------------------------------------------------------------
  # Root
  # -----------------------------------------------------------------------

  doc <- xml2::read_xml(
    paste0(
      '<mes:Structure',
      ' xmlns:mes="', ns[["mes"]], '"',
      ' xmlns:str="', ns[["str"]], '"',
      ' xmlns:com="', ns[["com"]], '"',
      '></mes:Structure>'
    )
  )

  root <- xml2::xml_root(doc)

  # -----------------------------------------------------------------------
  # Header
  # -----------------------------------------------------------------------

  header <- xml2::xml_add_child(
    root,
    "mes:Header"
  )

  xml2::xml_add_child(
    header,
    "mes:ID",
    paste0(
      "CODELIST_",
      component_id
    )
  )

  xml2::xml_add_child(
    header,
    "mes:Test",
    "false"
  )

  xml2::xml_add_child(
    header,
    "mes:Prepared",
    .ilostat_sdmx_xml_prepared()
  )

  sender <- xml2::xml_add_child(
    header,
    "mes:Sender",
    id = "ILO"
  )

  xml2::xml_add_child(
    sender,
    "com:Name",
    "International Labour Organization",
    `xml:lang` = "en"
  )

  # -----------------------------------------------------------------------
  # Structures / Codelists
  # -----------------------------------------------------------------------

  structures <- xml2::xml_add_child(
    root,
    "mes:Structures"
  )

  codelists <- xml2::xml_add_child(
    structures,
    "str:Codelists"
  )

  codelist_element <- xml2::xml_add_child(
    codelists,
    "str:Codelist",
    id = toupper(codelist),
    agencyID = "ILO",
    version = "1.0"
  )

  xml2::xml_add_child(
    codelist_element,
    "com:Name",
    paste0(
      component_id,
      " codelist"
    ),
    `xml:lang` = lang
  )

  # -----------------------------------------------------------------------
  # Codes
  # -----------------------------------------------------------------------

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
        paste0(
          "SDMX codelist data for `%s` must contain ",
          "`%s` and `%s`."
        ),
        component_id,
        code_column,
        label_column
      ),
      call. = FALSE
    )
  }

  for (i in seq_len(nrow(data))) {

    code <- xml2::xml_add_child(
      codelist_element,
      "str:Code",
      id = as.character(
        data[[code_column]][[i]]
      )
    )

    xml2::xml_add_child(
      code,
      "com:Name",
      as.character(
        data[[label_column]][[i]]
      ),
      `xml:lang` = lang
    )
  }

  as.character(doc)
}

# -------------------------------------------------------------------------
# SDMX-ML 3.1 dataflow
#
# R equivalent of pyilostat _to_sdmx_ml_dataflow().
# -------------------------------------------------------------------------

.ilostat_sdmx_to_xml_dataflow <- function(
  data,
  lang = "en"
) {

  ns <- .ilostat_sdmx_xml_ns

  # -----------------------------------------------------------------------
  # Root
  # -----------------------------------------------------------------------

  doc <- xml2::read_xml(
    paste0(
      '<mes:Structure',
      ' xmlns:mes="', ns[["mes"]], '"',
      ' xmlns:str="', ns[["str"]], '"',
      ' xmlns:com="', ns[["com"]], '"',
      '></mes:Structure>'
    )
  )

  root <- xml2::xml_root(doc)

  # -----------------------------------------------------------------------
  # Header
  # -----------------------------------------------------------------------

  header <- xml2::xml_add_child(
    root,
    "mes:Header"
  )

  xml2::xml_add_child(
    header,
    "mes:ID",
    "ILOSTAT_DATAFLOWS"
  )

  xml2::xml_add_child(
    header,
    "mes:Test",
    "false"
  )

  xml2::xml_add_child(
    header,
    "mes:Prepared",
    .ilostat_sdmx_xml_prepared()
  )

  sender <- xml2::xml_add_child(
    header,
    "mes:Sender",
    id = "ILO"
  )

  xml2::xml_add_child(
    sender,
    "com:Name",
    "International Labour Organization",
    `xml:lang` = "en"
  )

  # -----------------------------------------------------------------------
  # Structures / Dataflows
  # -----------------------------------------------------------------------

  structures <- xml2::xml_add_child(
    root,
    "mes:Structures"
  )

  dataflows <- xml2::xml_add_child(
    structures,
    "str:Dataflows"
  )

  # -----------------------------------------------------------------------
  # Individual Dataflows
  # -----------------------------------------------------------------------

  for (i in seq_len(nrow(data))) {

    dataflow_id <- as.character(
      data$DATAFLOW_ID[[i]]
    )

    dataflow_name <- as.character(
      data$DATAFLOW_NAME[[i]]
    )

    dataflow <- xml2::xml_add_child(
      dataflows,
      "str:Dataflow",
      id = dataflow_id,
      agencyID = "ILO",
      version = "1.0"
    )

    xml2::xml_add_child(
      dataflow,
      "com:Name",
      dataflow_name,
      `xml:lang` = lang
    )

    structure <- xml2::xml_add_child(
      dataflow,
      "str:Structure"
    )

    xml2::xml_set_text(
      structure,
      paste0(
        "urn:sdmx:org.sdmx.infomodel.",
        "datastructure.DataStructure=",
        "ILO:",
        dataflow_id,
        "(1.0)"
      )
    )
  }

  as.character(doc)
}

