#' @title Usefull Function
#' @name ilostat
NULL

ilostat_url <- function() {

  "http://www.ilo.org/ilostat-files/WEB_bulk_download/"

  }

.onAttach <- function(...){
  
  # Avoid running if in batch job / user not present
  if (!interactive()) return()
  
  # Obtain the installed package information
  local_version = utils::packageDescription('ilostat')
  
  # Grab the package information from CRAN
  cran_version = pkgVersionCRAN("ilostat")
  
  # Verify we have package information
  if(!is.null(cran_version) && length(cran_version) != 0L){
    
	latest_version = utils::compareVersion(cran_version, local_version$Version)
    
    d = if(latest_version == 0){
			'CURRENT'
		} else if(latest_version == 1){
			'OUT OF DATE'
		} else{
			'DEVELOPMENT'
		}
  
  } else{ # Gracefully fail.
    d = "Error in obtaining remote version info !"
    latest_version = 0
  }
  
  if(latest_version == 1){
    
	packageStartupMessage('\n!!! NEW VERSION ', cran_version , ' !!!')
    
	packageStartupMessage('Download the latest version: ',cran_version,' from CRAN via `install.packages("ilostat")`\n')
  
  }

}

pkgVersionCRAN = function(pkg, cran_url="http://cran.r-project.org/web/packages/"){
  
  # Create URL
  cran_pkg_loc = paste0(cran_url,pkg)
  
  # Try to establish a connection
  suppressWarnings( conn <- try( url(cran_pkg_loc) , silent=TRUE ) )
  
  # If connection, try to parse values, otherwise return NULL
  if ( all( class(conn) != "try-error") ) {
    
	suppressWarnings( cran_pkg_page <- try( readLines(conn) , silent=TRUE ) )
    
	close(conn)
  
  } else {
    
	return(NULL)
  
  }
  
  # Extract version info
  version_line = cran_pkg_page[grep("Version:",cran_pkg_page)+1]
  
  gsub("<(td|\\/td)>","",version_line)

}


check_pkg_for_ilostat_apps <- function(pkgs){

test_packages_for_ilostat <- NULL
if (length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
  new_package <- setdiff(pkgs, rownames(installed.packages()))
  for (i in 1:length(new_package)){
	answer <- menu(c("Yes", "No"), title= paste0("package : ",new_package[i], " is not available, do you want to install it ? "))
	if(answer == 1) { 
	  install.packages(new_package[i],  repos='http://cran.us.r-project.org') 
	} 
	
	if(answer == 2) {
	  test_packages_for_ilostat <- 'NORUN'
	  message('application could not be run as all requests packages have not been installed !!')
	return(NULL)
	}
  }
}
sapply(pkgs, require, character.only = TRUE)
	return('OK')
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
									from = c('col', 'col.l', 'geo', 'geo.l', 'sou', 'sou.l', 'ind', 'ind.l', 'sex', 'sex.l', 'cl1', 'cl1.l', 'cl2', 'cl2.l', 't', 'val', 'flag', 'ncol', 'ncl', 'ncl.l', 'nind', 'nind.l', 'nsou', 'nsou.l', 'tf', 'tt', 'f', 'f.l'), 
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