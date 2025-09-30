#' ilostat_date_convert: Internal Date Conversion Table
#'
#' A static internal tibble used to convert ILOSTAT's time period codes
#' (e.g., 'Q1', 'M03') into partial date strings and numeric fractions
#' for date calculation purposes.
#'
#' @format A tibble with 17 rows and 3 columns:
#' \describe{
#'   \item{code}{ILOSTAT time code (e.g., "Q1", "M07").}
#'   \item{num}{Numeric fraction of the year for calculation (e.g., ".25").}
#'   \item{date}{Partial date string for year-month-day construction (e.g., "-04-01").}
#' }
#' @keywords internal
#' @noRd
ilostat_date_convert <- data.frame(
  code = c("", "Q1", "Q2", "Q3", "Q4", 
           "M01", "M02", "M03", "M04", "M05", "M06", 
           "M07", "M08", "M09", "M10", "M11", "M12"),
  num = c("", "", ".25", ".5", ".75", 
          "", ".083", ".167", ".25", ".333", ".417", 
          ".5", ".583", ".667", ".75", ".833", ".917"),
  date = c("-01-01", "-01-01", "-04-01", "-07-01", "-10-01", 
           "-01-01", "-02-01", "-03-01", "-04-01", "-05-01", "-06-01", 
           "-07-01", "-08-01", "-09-01", "-10-01", "-11-01", "-12-01")
) %>% tibble::as_tibble()

#' ilostat_cols_ref: Standard ILOSTAT Column Names
#'
#' A static internal character vector containing the expected default column names
#' returned by the ILOSTAT API. Used for internal data validation and ordering.
#'
#' @format A character vector of length 22.
#' @keywords internal
#' @noRd
ilostat_cols_ref <- c("ref_area", "ref_area.label", "source", "source.label", "indicator", 
                      "indicator.label", "sex", "sex.label", "classif1", "classif1.label", 
                      "classif2", "classif2.label", "time", "obs_value", "obs_status", 
                      "obs_status.label", "note_classif", "note_classif.label", 
                      "note_indicator", "note_indicator.label", "note_source", 
                      "note_source.label")