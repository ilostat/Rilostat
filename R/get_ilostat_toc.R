#' @title Read ilostat Table of Contents
#' @description Download one table of contents from ilostat \url{https://ilostat.ilo.org} via bulk download facility 
#' \url{https://ilostat.ilo.org/data/bulk/}.
#' @param segment A character, way to get datasets by: \code{"indicator"} (default) or \code{"ref_area"}, 
#'        Can be set also with options(ilostat_segment = 'ref_area'),
#' @param lang a character, code for language. Available are \code{"en"} (default), 
#'        \code{"fr"} and \code{"es"}. Can be set also with options(ilostat_lang = 'fr'),
#' @param search a character vector, "none" (default), datasets with this pattern in
#'  	  the description will be returned,
#' 	      characters vector will be use as AND, Character with '|' as OR, see example,
#'        options(ilostat_time_format = 'date'),  
#' @param filters a list; \code{"none"} (default) to get a whole toc or a named list of
#'			filters to get just part of the table. Names of list objects are
#'			ilostat toc variable codes and values are vectors of observation codes.
#'			filters detect on variables.
#' @param fixed a logical, if \code{TRUE} (default), pattern is a string to be matched as is,
#'        Change to \code{FALSE} if more complex regex matching is needed.
#' @param quiet a logical, if \code{FALSE} , return message from processing, \code{TRUE} (default).
#' 			Can be set also with options(ilostat_quiet = TRUE), 
#' @return A tibble with ten columns depending of the segment: indicator or ref_area
#' 		\itemize{
#'      \item{\code{id}} : The codename of dataset of theme, will be used by the get_ilostat and get_ilostat_raw functions,
#'      \item{\code{indicator or ref_area}} : The indicator or ref_area code of dataset,
#'      \item{\code{indicator.label or ref_area.label}} : The indicator or ref_area name of dataset,
#'      \item{\code{freq}}  : The frequency code of dataset,
#'      \item{\code{freq.label}} : Is freq name of dataset,
#'      \item{\code{size}} : Size of the csv.gz files,
#'      \item{\code{data.start}} : First time period of the dataset, 
#'      \item{\code{data.end}} : Last time period of the dataset,
#'      \item{\code{last.update}} : Last update of the dataset,
#'      \item{\code{...}} : Others relevant information
#'      }
#' @seealso \code{\link{get_ilostat}}.
#' @details The toc in English by indicator is downloaded from ilostat API \url{https://rplumber.ilo.org/__docs__/#get-/metadata/toc/indicator/}. 
#' The values in column 'id' should be used to download a selected dataset.
#' @details The toc in English by ref_area is downloaded from ilostat API \url{https://rplumber.ilo.org/__docs__/#get-/metadata/toc/ref_area/}. 
#' The values in column 'id' should be used to download a selected dataset.
#' @references
#' See citation("Rilostat")
#' ilostat bulk download facility user guidelines \url{https://ilostat.ilo.org/data/bulk/} 
#' @author David Bescond \email{bescond@ilo.org}
#' @keywords utilities database
#' @examples
#' \dontrun{
#' ## default segment by indicator, default lang English
#'  toc <- get_ilostat_toc()
#'  head(toc)
#'  toc <- get_ilostat_toc(segment = 'ref_area', lang = 'fr')
#'  head(toc)
#' ##
#' ## search on toc
#'  toc <- get_ilostat_toc(search = 'education')
#'  head(toc)
#'  toc <- get_ilostat_toc(lang = 'fr', search = 'éducation')
#'  head(toc)
#'  toc <- get_ilostat_toc(segment = 'ref_area', lang = 'fr', search = 'Albanie')
#'  toc
#'  toc <- get_ilostat_toc(segment = 'ref_area', lang = 'es', search = 'Trimestral')
#'  head(toc)
#' ##
#' ## search multi on toc
#'  toc <- get_ilostat_toc(segment = 'ref_area', lang = 'fr', 
#'              search = 'Albanie|France', fixed = FALSE)
#'  head(toc)
#'  toc <- get_ilostat_toc(search = 'youth|adult', fixed = FALSE)
#'  head(toc)
#' ##
#' }
#' @export



get_ilostat_toc <- function(segment = getOption('ilostat_segment', 'indicator'), 
							lang = getOption('ilostat_lang', 'en'), 
							search = getOption('ilostat_search', 'none'), 
							filters = getOption('ilostat_filter', 'none'),
							fixed = getOption('ilostat_fixed', TRUE), 
							quiet = getOption('ilostat_quiet', FALSE)) {
  
  
  set_ilostat_toc(segment, lang, quiet)
  
  y <- get(paste0(".ilostatTOC", segment, lang), envir = .ilostatEnv) 
  
  
  if(!is_tibble(y)){
  
      stop("the toc file : ", ilostat_url(),segment, "/", "table_of_contents_",lang,".rds does not exist")
  
  }
  
  if(!unique(search %in% 'none')){  
	
	if(fixed){
	  
	  newsearch <- paste0("stringr::str_detect(titles, stringr::fixed('",search,"'))", collapse = ' & ')
	
	} else {
	  
	  newsearch <- paste0("stringr::str_detect(titles, '",search,"')", collapse = ' & ')
	
	}	
	
	y$titles <- paste0(y[[2]], y[[3]], y[[4]], y[[5]], y[[11]], y[[12]], y[[13]], y[[14]])
	
	y <- filter(y, eval(parse(text = newsearch)))[names(y) != 'titles'] 
   
  }
   
  # process filters
  
  if(!unique(filters == 'none')){  
  
    filters <- filters_ilostat(filters, names(y), fixed)
	  
    if(!is.null(filters)){
        
	  y <- filter(y, eval(parse(text = filters)))
	  
    }
  }	
   
   
  y
}


set_ilostat_toc <- function(segment, lang, quiet) {
  toc_name <- paste0(".ilostatTOC", segment, lang)

  if (!exists(toc_name, envir = .ilostatEnv)) {
    url <- paste0(ilostat_url(), segment, "/table_of_contents_", lang, ".rds")
    if(!quiet) message("Trying toc URL '", url, "'")

    # Build User-Agent
    ua <- build_user_agent()

    # Perform request
    resp <- request(url) |>
      req_headers(`User-Agent` = ua) |>
      req_perform()

    if (resp_status(resp) != 200) {
      stop("Table of contents for segment '", segment, "' and lang '", lang, "' not available")
    }

    # Save and read toc
    tmpfile <- tempfile(fileext = ".rds")
    writeBin(resp_body_raw(resp), tmpfile)

    .ilostatTOC <- read_rds(tmpfile)

    # Cache in environment
    assign(toc_name, .ilostatTOC, envir = .ilostatEnv)
  }

  invisible(0)
}
