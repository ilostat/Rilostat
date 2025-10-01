#' @title Clean ilostat Cache
#' @description Deletes all cache files from the your ilostat cache directory.
#'              See \code{\link{get_ilostat}} for more on cache.
#' @param cache_dir A character, path to a cache directory. The directory has to exist.
#'        	The \code{NULL} (default) uses and creates
#'        	'ilostat' directory in the temporary directory from
#'        	\code{\link{tempdir}}. The directory can also be set with
#'        	\code{option} ilostat_cache_dir,
#' @param cache_update a logical whether to delete only out of date cache files. Useful when \code{cache_dir} 
#'          is set as keep only update datasets. Can be set also with
#'        	options(ilostat_update = TRUE). Default is \code{FALSE}.
#' @param quiet a logical, if \code{TRUE} , don't return message from processing, \code{FALSE} (default).
#' 			Can be set also with options(ilostat_quiet = TRUE), 
#' @author David Bescond \email{bescond@ilo.org}
#' @references
#' See citation("Rilostat")
#' ilostat bulk download facility user guidelines 
#' \url{https://webapps.ilo.org/ilostat-files/Documents/ILOSTAT_BulkDownload_Guidelines.pdf}
#' @examples 
#' \dontrun{
#' clean_ilostat_cache() 
#' }
#' @export

clean_ilostat_cache <- function(cache_dir = getOption("ilostat_cache_dir", file.path(tempdir(), "ilostat")), 
								cache_update = getOption("ilostat_cache_update", FALSE), 
								quiet = getOption('ilostat_quiet', FALSE)){

  if (!file.exists(cache_dir)){ 
    stop("The cache folder ", cache_dir, " does not exist")
  }
  
  cache_files <- list.files(cache_dir, pattern = "rds|dta|csv|csv.gz|sav|sas7bdat", full.names = TRUE)
  
  
  if (length(cache_files) == 0) {
    
	if(!quiet) message("The cache folder ", cache_dir, " is empty.")
  
  } else {
    
	if(cache_update){

      cache_files <-  cache_files %>% as_tibble
  
      cache_files$segment <- stringr::str_split(cache_files$value, '-', n = 2, simplify = TRUE)[,1] 
  
      cache_files$segment <- ifelse(stringr::str_sub(cache_files$segment,-9, -1) %in% 'indicator', 'indicator', 'ref_area')		
   
      cache_files$id <- stringr::str_split(cache_files$value, '-', n = 3, simplify = TRUE)[,2]
  
      cache_files$last.update <- stringr::str_split(cache_files$value, '-', n = 5, simplify = TRUE)[,5]
  
      cache_files$last.update <- stringr::str_split(cache_files$last.update, stringr::fixed('.'), n = 2, simplify = TRUE)[,1]
	
      ref_toc <- NULL

      test = "last.update = ifelse(substr(last.update, 6,8) %in% '/20', last.update %>% strptime('%d/%m/%Y  %H:%M:%S') %>% format('%Y%m%dT%H%M%S'), last.update)"
	
      if(unique(cache_files$segment) %in% 'indicator'){
            
		ref_toc <- bind_rows(ref_toc, 
						get_ilostat_toc('indicator') %>%
						select_at(.vars = c("id", "last.update")) %>%
						mutate(eval(parse(text = test)))
						)
						
		ref_toc <- filter(ref_toc, eval(parse(text = "id %in% cache_files$id")))	
	  
	  }
	  
	  if(unique(cache_files$segment) %in% 'ref_area'){
	  
	    
		ref_toc <- bind_rows(ref_toc, 
						get_ilostat_toc('ref_area') %>%
						select_at(.vars = c("id", "last.update")) %>%
						mutate(eval(parse(text = test))) 
						)
		ref_toc <- filter(ref_toc, eval(parse(text = "id %in% cache_files$id")))				
	  
	  }
	  
	  cache_files <- filter(cache_files, eval(parse(text = "!paste0(id, last.update) %in% paste0(ref_toc$id, ref_toc$last.update)")))
	  
	  if(nrow(cache_files) > 0) {
	  
		cache_files <- cache_files$value
	  
	    unlink(cache_files)
    
	    if(!quiet) message("Deleted outdated ilostat files from ", cache_dir)    
        
	  
	  } else {
	  
		if(!quiet)  message("Cache folder ", cache_dir, " is up to date.")
  
	  
	  }
  
	} else {
  
	
		unlink(cache_files)
    
		if(!quiet) message("Deleted ilostat files from ", cache_dir)    
    
	}
  
  }
  
  invisible(TRUE)

}
