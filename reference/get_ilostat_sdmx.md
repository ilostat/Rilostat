# Retrieve ILOSTAT data using an SDMX-oriented interface

\`get_ilostat_sdmx()\` provides an SDMX-oriented interface to ILOSTAT
datasets while reusing the native Rilostat download, disk-cache and
filtering infrastructure.

## Usage

``` r
get_ilostat_sdmx(
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
)
```

## Arguments

- id:

  Character. ILOSTAT dataset identifier, for example
  \`"UNE_TUNE_SEX_AGE_NB_A"\`. Required for \`"data"\`, \`"structure"\`
  and \`"codelist"\`. For \`resource = "dataflow"\`, \`id = NULL\`
  returns the complete ILOSTAT dataflow catalogue.

- resource:

  Character. SDMX resource to return. One of: \`"data"\`,
  \`"structure"\`, \`"codelist"\` or \`"dataflow"\`.

- component:

  Character. SDMX component identifier used with \`resource =
  "codelist"\`, for example \`"AGE"\`, \`"SEX"\`, \`"OBS_STATUS"\` or
  \`"UNIT_MEASURE"\`.

- segment:

  Character. Native ILOSTAT segment passed to \[get_ilostat()\] or
  \[get_ilostat_toc()\].

- type:

  Character. Representation of coded values. One of: \`"code"\`,
  \`"label"\`, \`"label_short"\` or \`"both"\`.

  \`"code"\` returns SDMX codes.

  \`"label"\` replaces coded values with their labels.

  \`"label_short"\` uses short/display labels when available.

  \`"both"\` returns codes together with companion \`\*\_LABEL\`
  columns.

- format:

  Character. Output format. One of: \`"dataframe"\`, \`"sdmx-ml"\` or
  \`"sdmx-json"\`.

  \`"sdmx-ml"\` and \`"sdmx-json"\` are available for the structural
  resources \`"structure"\`, \`"codelist"\` and \`"dataflow"\`.
  Observation data are currently returned as a data frame.

- lang:

  Character. Language used for labels. One of \`"en"\`, \`"fr"\` or
  \`"es"\`.

- filters:

  Named list of filters using SDMX component identifiers, for example
  \`REF_AREA\`, \`SEX\`, \`AGE\`, \`FREQ\`, \`MEASURE\` and
  \`TIME_PERIOD\`.

  Ordinary component filters reuse native Rilostat matching semantics.
  For example, \`SEX = "SEX_F"\` filters female observations.

  \`FREQ\` and \`MEASURE\` are dataset-level SDMX constants. A filter
  that does not match the dataset returns an empty SDMX data frame.

  \`TIME_PERIOD\` supports the existing Rilostat time filtering
  semantics. Equality can be expressed directly, for example
  \`TIME_PERIOD = "2020"\`, while comparison operators can be supplied
  as a named list, for example \`TIME_PERIOD = list(ge = "2020", le =
  "2023")\`.

  Ordinary coded components also support \`eq\` and \`ne\`, for example
  \`SEX = list(ne = "SEX_T")\`.

- best_source:

  Character. Source-selection behavior passed to \[get_ilostat()\].
  \`"yes"\` (default) returns best-source observations and does not
  expose \`BEST_SOURCE\`; \`"no"\` and \`"all"\` retain the
  \`BEST_SOURCE\` component.

- fixed:

  Logical. Native matching behavior passed to \[get_ilostat()\].

- cache_dir:

  Character or \`NULL\`. Directory used for the SDMX metadata disk cache
  and native Rilostat cache. If \`NULL\`, the standard Rilostat cache
  location is used.

- refresh:

  Logical. If \`TRUE\`, refresh cached dataset metadata and request a
  refresh of the native ILOSTAT data cache.

## Value

The return value depends on \`resource\` and \`format\`.

With \`format = "dataframe"\`, a data frame is returned.

With \`format = "sdmx-ml"\`, a character string containing SDMX-ML 3.1
is returned.

With \`format = "sdmx-json"\`, a character string containing SDMX-JSON
2.1 is returned.

## Details

The existing \[get_ilostat()\] function remains the native ILOSTAT
interface. \`get_ilostat_sdmx()\` adds an SDMX semantic layer whose
components are derived dynamically from dataset metadata.

For \`resource = "data"\`, native ILOSTAT column names are mapped to
SDMX component identifiers. Dynamic classifications are obtained from
the dataset metadata rather than hard-coded. For example, a native
\`classif1\` column can become \`AGE\` when the dataset metadata
identifies that dimension as age.

The data resource includes the SDMX message envelope components
\`STRUCTURE\`, \`STRUCTURE_ID\` and \`ACTION\`. \`STRUCTURE_NAME\` is
also returned when labels are requested.

Dataset-level components such as \`FREQ\`, \`MEASURE\`, \`UNIT_MEASURE\`
and \`UNIT_MULT\` are derived from the dataset SDMX metadata.

\`resource = "codelist"\` returns the canonical ILOSTAT codelist
associated with the requested SDMX component.

## Examples

``` r
if (FALSE) { # \dontrun{

# Observation data using SDMX component names
x <- get_ilostat_sdmx(
  "UNE_TUNE_SEX_AGE_NB_A",
  filters = list(
    REF_AREA = "FRA",
    SEX = "SEX_F",
    AGE = "AGE_YTHADULT_YGE15"
  )
)

# Codes and labels
x <- get_ilostat_sdmx(
  "UNE_TUNE_SEX_AGE_NB_A",
  type = "both",
  filters = list(
    REF_AREA = "FRA",
    SEX = "SEX_F"
  )
)

# Time range
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

# Dataset structure
structure <- get_ilostat_sdmx(
  "UNE_TUNE_SEX_AGE_NB_A",
  resource = "structure"
)

# Canonical AGE codelist
age <- get_ilostat_sdmx(
  "UNE_TUNE_SEX_AGE_NB_A",
  resource = "codelist",
  component = "AGE"
)

# One Dataflow
flow <- get_ilostat_sdmx(
  "UNE_TUNE_SEX_AGE_NB_A",
  resource = "dataflow"
)

# Complete ILOSTAT Dataflow catalogue
flows <- get_ilostat_sdmx(
  resource = "dataflow"
)

# SDMX-ML 3.1 structure message
xml <- get_ilostat_sdmx(
  "UNE_TUNE_SEX_AGE_NB_A",
  resource = "structure",
  format = "sdmx-ml"
)

# SDMX-JSON 2.1 structure message
json <- get_ilostat_sdmx(
  "UNE_TUNE_SEX_AGE_NB_A",
  resource = "structure",
  format = "sdmx-json"
)
} # }
```
