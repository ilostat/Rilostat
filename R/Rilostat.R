#' @title Get more Support
#'
#' @description On regular basis new tutorial and examples are building and available through this function.
#'
#' @details brief description of the package
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
#' R Tools for ilostat Open Data
#'
#' @name Rilostat
#' @author David Bescond \email{bescond@ilo.org}
#' @references
#' See citation("Rilostat")
#'
#' ilostat bulk download facility user guidelines 
#' \url{https://webapps.ilo.org/ilostat-files/Documents/ILOSTAT_BulkDownload_Guidelines.pdf}
#'
#' @keywords package
#' @importFrom readr read_csv
#' @importFrom readr col_character
#' @importFrom readr col_double
#' @importFrom readr cols
#' @importFrom readr read_rds
#' @importFrom plyr llply
#' @importFrom plyr ldply
#' @importFrom plyr mapvalues
#' @importFrom dplyr %>%
#' @importFrom dplyr select_at
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by_at
#' @importFrom dplyr mutate_if
#' @importFrom haven read_dta
#' @importFrom haven read_sav
#' @importFrom haven read_sas
#' @importFrom haven write_dta
#' @importFrom haven write_sav
#' @importFrom haven write_sas
#' @importFrom stringr str_replace
#' @importFrom stringr str_c
#' @importFrom stringr str_sub
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom stringr fixed
#' @importFrom tibble is_tibble
#' @importFrom tibble as_tibble
#' @importFrom utils download.file
#' @importFrom data.table fwrite
#' @importFrom data.table fread

#' @examples
#' \dontrun{
#'
#' # check which documentation have been recently added:
#'
#'  # help(Rilostat) 
#' 
#'  # https://ilostat.github.io/Rilostat/ 
#' 
#' }

NULL

ilostat_url <- function() {

	if(dir.exists('/data/rds/')) {
		'/data/rds/'
	} else {
		'https://rplumber.ilo.org/'
		}
  
  }

filters_ilostat <- function(
							filters, 
							dat_name, 
							fixed){

  if(!is.list(filters)){
  
    if(unique(filters == 'none')){
	  filters = list()
	} else{  
	
	  stop("Incorrect filters should be a list or set as 'note'")
  
    }
  
  }
  
  newfilter <- list()	
  
  if (is.list(filters) & !length(filters) == 0){  
	
    names(filters) <- tolower(names(filters)) # not capital sensitive
	
	names(filters) <- mapvalues(names(filters), 
									from = c('geo', 'geo.l', 'sou', 'sou.l', 'ind', 'ind.l', 'sex', 'sex.l', 'cl1', 'cl1.l', 'cl2', 'cl2.l', 't', 'val', 'flag', 'ncol', 'ncl', 'ncl.l', 'nind', 'nind.l', 'nsou', 'nsou.l', 'tf', 'tt', 'f', 'f.l'), 
									to  = c(ilostat_cols_ref, 'timefrom', 'timeto', 'freq', 'freq.label'), warn_missing = FALSE)
     
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
        
	newfilter <-  paste0("(", unlist(newfilter),")", collapse = " & ")
  
  } else {
	
	newfilter <- NULL
	  
  }
		
  newfilter
}



.ilostatEnv <- new.env()
