#' @title Read Ilostat Table of Contents
#' @description Download one table of contents from ilostat \url{www.ilo.org/ilostat} via bulk download facility 
#' \url{http://www.ilo.org/ilostat-files/WEB_bulk_download/html/bulk_main.html}.
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
#' @return A tibble with ten columns depending of the segment: indicator or ref_area
#' 		\itemize{
#'      \item{\code{id}} : The codename of dataset of theme, will be used by the get_ilostat and get_ilostat_raw functions,
#'      \item{\code{indicator or ref_area}} : The indicator or ref_area code of dataset,
#'      \item{\code{indicator.label or ref_area.label}} : The indicator or ref_area name of dataset,
#'      \item{\code{freq}}  : The freqency code of dataset,
#'      \item{\code{freq.label}} : Is freq name of dataset,
#'      \item{\code{size}} : Size of the csv.gz files,
#'      \item{\code{data.start}} : First time period of the dataset, 
#'      \item{\code{data.end}} : Last time period of the dataset,
#'      \item{\code{last.update}} : Last update of the dataset,
#'      \item{\code{...}} : Others relevant information
#'      }
#' @seealso \code{\link{get_ilostat}}.
#' @details The TOC in English by indicator is downloaded from \url{http://www.ilo.org/ilostat-files/WEB_bulk_download/indicator/table_of_contents_en.csv}. 
#' The values in column 'id' should be used to download a selected dataset.
#' @details The TOC in English by ref_area is downloaded from \url{http://www.ilo.org/ilostat-files/WEB_bulk_download/ref_area/table_of_contents_en.csv}. 
#' The values in column 'id' should be used to download a selected dataset.
#' @references
#' See citation("Rilostat")
#' ilostat bulk download facility user guidelines \url{http://www.ilo.org/ilostat-files/WEB_bulk_download/ILOSTAT_BulkDownload_Guidelines.pdf} 
#' @author David Bescond \email{bescond@ilo.org}
#' @keywords utilities database
#' @examples
#' \dontrun{
#' ## default segment by indicator, default lang English
#' 		toc <- get_ilostat_toc()
#' 		head(toc)
#' 		toc <- get_ilostat_toc(segment = 'ref_area', lang = 'fr')
#' 		head(toc)
#' ##
#' ## search on toc
#'   	toc <- get_ilostat_toc(search = 'education')
#'   	head(toc)
#'   	toc <- get_ilostat_toc(lang = 'fr', search = 'éducation')
#'   	head(toc)
#'   	toc <- get_ilostat_toc(segment = 'ref_area', lang = 'fr', search = 'Albanie')
#'   	toc
#'   	toc <- get_ilostat_toc(segment = 'ref_area', lang = 'es', search = 'Trimestral')
#'   	head(toc)
#' ##
#' ## search multi on toc
#'      toc <- get_ilostat_toc(segment = 'ref_area', lang = 'fr', 
#'              search = 'Albanie|France', fixed = FALSE)
#'		head(toc)
#' 		toc <- get_ilostat_toc(search = 'youth|adult', fixed = FALSE)
#'		head(toc)
#' 		toc <- get_ilostat_toc(search = c('youth','adult'), fixed = FALSE)
#'		head(toc)
#' ##
#' }
#' @export

get_ilostat_toc <- function(segment = getOption('ilostat_segment', 'indicator'), 
							lang = getOption('ilostat_lang', 'en'), 
							search = getOption('ilostat_search', 'none'), 
							filters = getOption('ilostat_filter', 'none'),
							fixed = getOption('ilostat_fixed', TRUE)) {
  
  set_ilostat_toc(segment, lang)
  
  y <- get(paste0(".ilostatTOC", segment, lang), envir = .ilostatEnv) 
  
  if(!is_tibble(y)){
  
  	  stop("the toc file : ", ilostat_url(),segment, "/", "table_of_contents_",lang,".csv does not exist")
  
  }
  
  if(!unique(search %in% 'none')){  
	
	if(fixed){
	  
	  newsearch <- paste0("stringr::str_detect(titles, stringr::fixed('",search,"'))", collapse = ' & ')
	
	} else {
	  
	  newsearch <- paste0("stringr::str_detect(titles, '",search,"')", collapse = ' & ')
	
	}	
	
	y$titles <- paste0(y[[2]], y[[3]], y[[4]], y[[5]], y[[11]], y[[12]], y[[13]], y[[14]])
	
	y <- filter_(y, newsearch)[names(y) != 'titles'] 
   
  }
   
  # process filters
  
  if(!unique(filters == 'none')){  
  
    filters <- filters_ilostat(filters, names(y), fixed)
	  
    if(!is.null(filters)){
        
	  y <- filter_(y, filters)
	  
    }
  }	
   
  invisible(gc(reset = TRUE))
   
  y
}


set_ilostat_toc <- function(segment, lang) {
  
  if (!exists(paste0(".ilostatTOC", segment, lang), envir = .ilostatEnv)) {
    
	base <- ilostat_url()
    
	url <- paste0(base, segment, "/", "table_of_contents_",lang,".csv")
    
	.ilostatTOC <- read_csv(
	
						url(url), 
	
						col_types = cols(.default = col_character(), n.records = col_double()))
    
	assign(paste0(".ilostatTOC", segment, lang), .ilostatTOC, envir = .ilostatEnv)
  
  }
  
  invisible(0)

  }


