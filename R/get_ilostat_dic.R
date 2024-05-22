#' @title Read Ilostat Dictionary
#' @description Downloads one ilostat dictionary from ilostat \url{https://ilostat.ilo.org} via bulk download facility 
#' \url{https://ilostat.ilo.org/data/bulk/}.
#' @details For a given coded variable from ilostat \url{https://ilostat.ilo.org/}.
#'    The dictionaries link codes with human-readable labels.
#'    To translate codes to labels, use \code{\link{label_ilostat}}.
#' @param dic A character, dictionary for the variable to be downloaded,
#' @param lang a character, code for language. Available are \code{"en"} (default), 
#'        \code{"fr"} and \code{"es"}. Can be set also with options(ilostat_lang = 'fr'),
#' @return tibble with two columns: code names and full names.
#' @seealso \code{\link{label_ilostat}}, \code{\link{get_ilostat}}.
#' @keywords utilities database
#' @author David Bescond \email{bescond@ilo.org}
#' @references
#' See citation("Rilostat")
#' ilostat bulk download facility user guidelines 
#' \url{https://webapps.ilo.org/ilostat-files/WEB_bulk_download/ILOSTAT_BulkDownload_Guidelines.pdf}
#' ilostat bulk download facility main page 
#' \url{https://ilostat.ilo.org/data/bulk/}
#' @examples
#' \dontrun{
#'  tmp <- get_ilostat_dic("indicator")
#'  head(tmp)
#'  tmp <- get_ilostat_dic("classif1", lang = "fr")
#'  head(tmp)
#' }
#' @export

get_ilostat_dic <- function(dic,
							lang = getOption('ilostat_lang', 'en') 
							) {

  dictlang <- paste0(tolower(dic), "_", tolower(lang))
    
  if (!exists(dictlang, envir = .ilostatEnv)) {
      
	# url <- ilostat_url()		   
      
	tname <- paste0(ilostat_url(), "metadata/dic/?var=", tolower(dic), "&lang=",tolower(lang),  "&format=rds")
	  
	get_dic <- read_rds(tname) # %>% as_tibble %>% mutate_if(is.factor, as.character)
      
	assign(dictlang, get_dic, envir = .ilostatEnv)
	  
	invisible(gc(reset = TRUE))
   
    if(!is_tibble(get_dic)){
  
  	  stop("the dictionnary : ", ilostat_url(),dic, "_",lang,".rds does not exist")
  
    }
	
  }
  

  get(dictlang, envir = .ilostatEnv)

}

