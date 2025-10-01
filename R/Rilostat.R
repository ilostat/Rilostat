#' @title R Tools for ilostat Open Data
#'
#' @description This package provides a user-friendly interface to access, download, and manage ILOSTAT open data.
#'
#' @details On a regular basis new tutorials and examples are built and available through this function.
#'
#' \tabular{ll}{
#' Package: \tab Rilostat\cr
#' Type: \tab Package\cr
#' Version: \tab See sessionInfo() or DESCRIPTION file\cr
#' Date: \tab 2020-2025\cr
#' License: \tab BSD_2_clause + LICENSE\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' @name Rilostat
#' @author David Bescond \email{bescond@ilo.org}
#' @references
#' See citation("Rilostat")
#'
#' ilostat bulk download facility user guidelines
#' \url{https://webapps.ilo.org/ilostat-files/Documents/ILOSTAT_BulkDownload_Guidelines.pdf}
#'
#' @keywords internal
#' @importFrom dplyr %>% select filter mutate summarise distinct group_by_at across contains bind_rows select_at mutate_if where ungroup group_by
#' @importFrom stringr str_replace str_c str_sub str_detect str_split fixed
#' @importFrom readr read_csv col_character col_double cols read_rds read_lines
#' @importFrom plyr llply ldply mapvalues
#' @importFrom haven read_dta read_sav read_sas write_dta write_sav write_sas
#' @importFrom tibble is_tibble as_tibble
#' @importFrom utils download.file packageVersion
#' @importFrom data.table fread fwrite
#' @importFrom httr2 request req_headers req_perform resp_body_raw resp_status
#' @examples
#' \dontrun{
#' # check which documentation have been recently added:
#' # help(Rilostat)
#'
#' # https://ilostat.github.io/Rilostat/
#' }
"_PACKAGE"


#' Internal utility to determine the base URL for data access.
#'
#' Checks if a local RDS directory exists and returns the local path
#' or the remote API URL.
#'
#' @return A character string representing the base URL or local path.
#' @keywords internal
#' @noRd
ilostat_url <- function() {

  if(dir.exists('/data/rds/')) {
    '/data/rds/'
  } else {
    'https://rplumber.ilo.org/files/'
    }

}

#' Internal utility function to translate and format filters for data processing.
#'
#' Takes user-provided filters (as a list) and converts them into a
#' string suitable for \code{dplyr::filter} evaluation.
#'
#' @param filters A list of filters provided by the user.
#' @param dat_name Column names in the target dataset.
#' @param fixed Logical, whether to use fixed string detection.
#' @return A character string of the filter expression, or \code{NULL}.
#' @keywords internal
#' @noRd
filters_ilostat <- function(
                        filters,
                        dat_name,
                        fixed){

  if(!is.list(filters)){

    if(unique(filters == 'none')){
      filters = list()
    } else{
      # Use base R functions explicitly
      stop("Incorrect filters should be a list or set as 'note'")

    }

  }

  newfilter <- list()

  if (is.list(filters) & !length(filters) == 0){

    names(filters) <- tolower(names(filters)) # not capital sensitive

    
    names(filters) <- plyr::mapvalues(names(filters),
                                      from = c('geo', 'geo.l', 'sou', 'sou.l', 'ind', 'ind.l', 'sex', 'sex.l', 'cl1', 'cl1.l', 'cl2', 'cl2.l', 't', 'val', 'flag', 'ncol', 'ncl', 'ncl.l', 'nind', 'nind.l', 'nsou', 'nsou.l', 'tf', 'tt', 'f', 'f.l'),
                                      to = c(ilostat_cols_ref, 'timefrom', 'timeto', 'freq', 'freq.label'), warn_missing = FALSE)

    for (i in 1:length(filters)){

      if(names(filters[i]) %in% 'timefrom'){

        newfilter[[i]] <- paste0("as.numeric(str_sub(time,1,4)) ", '>= ', as.numeric(filters[[i]]))

      } else  if (names(filters[i]) %in% 'timeto'){

        newfilter[[i]] <- paste0("as.numeric(str_sub(time,1,4)) ", '<= ', as.numeric(filters[[i]]))

      } else {

        if(names(filters[i]) %in% dat_name){
          if(fixed){

          newfilter[[i]] <- paste0(paste0("stringr::str_detect(",  names(filters[i]), ", stringr::fixed('", filters[[i]], "'))"), collapse = ' | ')

        } else {

          newfilter[[i]] <- paste0(paste0("stringr::str_detect(",  names(filters[i]), ", '", filters[[i]], "')"), collapse = ' | ')

        }

        } else {

          message("Variable name: '" ,names(filters[i]), "' , not present on dataset, filters not apply on this variable !")

        }

      }

    }
  }

  if(length(newfilter)>0){

    newfilter <- paste0("(", unlist(newfilter),")", collapse = " & ")

  } else {

    newfilter <- NULL

  }

  newfilter
}

#' Internal environment for session variables.
#'
#' Used to store non-exported, temporary variables needed during a session.
#'
#' @keywords internal
#' @noRd
.ilostatEnv <- new.env()


#' Internal function to run Shiny apps stored remotely.
#'
#' Downloads and executes the GLOBAL.R, UI.R, and SERVER.R files for a given app.
#'
#' @param name The name of the Shiny app directory.
#' @keywords internal
#' @noRd
.runapps <- function(name) {

  production_status <- FALSE

	myGlobal 	<- read_lines(paste0(ilostat_url(), 'apps/',name,'/GLOBAL.r'))
	myUi 		<- read_lines(paste0(ilostat_url(), 'apps/',name,'/UI.r'))
	myServer 	<- read_lines(paste0(ilostat_url(), 'apps/',name,'/SERVER.r'))
 
  # Execute the Shiny app
  shiny::runApp(
    appDir  = list(
      global =  eval(parse(text = myGlobal)),
      ui = function() {
        eval(parse(text = myUi)) 
      },
      server = function(input, output, session) {
         eval(parse(text = myServer)) 
      }
    ),
    port = 3838,
    launch.browser = FALSE,
    host = "127.0.0.1",
    workerId = "",
    quiet = FALSE,
    display.mode = "normal"
  )
}

#' Internal utility to construct a user agent string.
#'
#' Creates a string identifying the package name, version, R platform, and OS.
#'
#' @return A character string for the User-Agent header.
#' @keywords internal
#' @noRd
build_user_agent <- function() {
  sys <- Sys.info()
  os <- paste(sys[["sysname"]], sys[["release"]])
  platform <- R.version$platform

  paste0(
    "Rilostat/", utils::packageVersion("Rilostat"),
    " (", platform, "; ", os, ") "
  )
}

# Set encoding option (not requiring a roxygen block)
options(encoding="UTF-8")