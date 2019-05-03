#' @title Read Ilostat Dictionary
#' @description Downloads one ilostat dictionary from ilostat \url{www.ilo.org/ilostat} via bulk download facility 
#' \url{http://www.ilo.org/ilostat-files/WEB_bulk_download/html/bulk_main.html}.
#' @details For a given coded variable from ilostat \url{www.ilo.org/ilostat}.
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
#' See citation("ilostat")
#' ilostat bulk download facility user guidelines 
#' \url{http://www.ilo.org/ilostat-files/WEB_bulk_download/ILOSTAT_BulkDownload_Guidelines.pdf}
#' ilostat bulk download facility main page 
#' \url{http://www.ilo.org/ilostat-files/WEB_bulk_download/html/bulk_main.html}
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
      
	url <- ilostat_url()		   
      
	tname <- paste0(url, "dic/", dictlang,  ".csv")
	  
	get_dic <- read_csv(url(tname), col_types = cols(.default = col_character()), progress = FALSE)
      
	assign(dictlang, get_dic, envir = .ilostatEnv)
	  
	invisible(gc(reset = TRUE))
   
    if(!is_tibble(get_dic)){
  
  	  stop("the dictionnary : ", ilostat_url(),dic, "_",lang,".csv does not exist")
  
    }
	
  }
  

  get(dictlang, envir = .ilostatEnv)

}

