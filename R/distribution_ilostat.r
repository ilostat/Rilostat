#' @title Switch ilostat to distribution 
#' @description Get distribution for ilostat number of persons only.
#' @param x  dataset to transform into distribution. 
#' @param var  String variable name use for the distribution default \code{"no"}, 
#' 		could be \code{"sex"}, \code{"classif1"}, \code{"classif2"}.
#' @param .keep if true return only new column call distribution default \code{FALSE},
#' @param quiet a logical, if \code{TRUE} , don't return message from processing, \code{FALSE} (default).
#' 			Can be set also with options(ilostat_quiet = TRUE).
#' @details this function use the max of the corresponding grouping so it is 
#' 		important to not filter any subset of the corresponding variable selected for the distribution
#'		at this level, ie. if you remove SEX_T, the distribution by sex will only have SEX_F or SEX_M / max(SEX_M, SEX_F) * 100,
#'		which is no longer a distribution.
#'
#'		In addition, distribution is only applicable for indicators with Number of persons (usually in thousands), 
#'		So please do not distribute ratios, earnings, hours of works, CPI, GDP etc ... no warning will prevent for that
#'		if doubts use distribution from get_ilostat() instead of, warnings will help you.
#'
#'
#' @author David Bescond \email{bescond@ilo.org}
#' @return a data_frame. obs_status will no longer be a number of persons but a percentage.
#' @references
#' See citation("Rilostat")
#' ilostat bulk download facility user guidelines 
#' \url{https://ilostat.ilo.org/data/bulk/}
#' @examples
#' \dontrun{
#'  dat <- get_ilostat("EMP_TEMP_SEX_STE_GEO_NB_A", cache = FALSE)
#'  dat_dist <- distribution_ilostat(dat, "classif1")
#'  dat_plus_dist <- mutate(dat, dist = distribution_ilostat(dat,"classif1", .keep=TRUE))
#'  head(dat_dist)
#'  clean_ilostat_cache() 
#' }
#' @export

distribution_ilostat <- function(x, var, .keep = FALSE, quiet = getOption('ilostat_quiet', TRUE)) {

  # validation
  if (length(var) != 1 || !tolower(var) %in% c("sex", "classif1", "classif2")) {
    warning("var must be one of: 'sex', 'classif1', 'classif2'. Returning input unchanged.")
    return(x)
  }

  var <- tolower(var)

  if(!quiet) {message(
    paste0(
      "Processing of ",
      paste(unique(x[["indicator"]]), collapse = ", "),
      "... distribution by ", var
    )
  )}

  # reference columns present in data
  ref_col <- ilostat_cols_ref[ilostat_cols_ref %in% names(x)]
  ref_col <- setdiff(ref_col, var)[1]

  # computation
  out <- x %>%
    dplyr::group_by(across(all_of(ref_col))) %>%
	dplyr::mutate(
	  obs_value = dplyr::if_else(
		!is.na(.data$obs_value) & .data$obs_value != 0,
		.data$obs_value / max(.data$obs_value, na.rm = TRUE) * 100,
		.data$obs_value
	  )
	) %>%
    dplyr::ungroup()

  if (.keep) {
    return(out$obs_value)
  }

  out
}