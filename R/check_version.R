#' @title Check Rilostat version against CRAN
#' @description Compare installed version with CRAN and warn if outdated.
#' @param pkg Package name (default "Rilostat")
#' @param quiet Logical, suppress message if up-to-date
#' @return Invisible logical (TRUE = up-to-date, FALSE = outdated or unknown)
#' @importFrom utils packageVersion available.packages
#' @keywords internal
#' @export
check_ilostat_version <- function(pkg = "Rilostat", quiet = FALSE) {
  
  # installed version
  current_version <- tryCatch(
    utils::packageVersion(pkg),
    error = function(e) NULL
  )
  
  if (is.null(current_version)) {
    warning(sprintf("Package '%s' is not installed.", pkg), call. = FALSE)
    return(invisible(FALSE))
  }
  
  # CRAN version (light call)
	cran_version <- tryCatch(
	  as.character(utils::packageVersion(pkg, repos = "https://cloud.r-project.org")),
	  error = function(e) NA
	)
  
  if (is.na(cran_version)) {
    if (!quiet) {      message("Unable to check CRAN version (no internet or CRAN unavailable).")  }
    return(invisible(NA))
  }
  
  # compare
  if (current_version < cran_version) {
    warning(
      sprintf(
        "A newer version of '%s' is available on CRAN (%s > %s). Please run update.packages('%s').",
        pkg, cran_version, current_version, pkg
      ),
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  
  invisible(TRUE)
}


# Internal: run only once per session
.check_version_once <- local({
  checked <- FALSE
  function(...) {
    if (!checked) {
      checked <<- TRUE
      check_ilostat_version(...)
    }
  }
})