#' @title Switch Ilostat codes and labels
#' @description Gets definitions/labels for ilostat codes from ilostat dictionaries.
#' @param x A character or a factor vector or a data_frame to labelled. 
#' @param dic  A string (vector) naming ilostat dictionary or dictionaries.
#'   If \code{NULL} (default) dictionary names are taken from column names of 
#'   the data_frame. A character or a factor vector or a data_frame to labelled, 
#' @param lang a character, code for language. Available are \code{"en"} (default), 
#'        \code{"fr"} and \code{"es"}. Can be set also with options(ilostat_lang = 'fr'),
#' @param code a vector of names of the column for which code columns
#'   should be retained. Set to \code{"all"}, keep all the code.  
#' @details A character or a factor vector of codes returns a corresponding vector of definitions. 
#'   \code{label_ilostat} labels also data_frames from \code{\link{get_ilostat}}. For vectors a dictionary 
#    name have to be supplied. For data_frames dictonary names are taken from column names with suffix ".label". 
#'   "time" and "values" columns are returned as they were, so you can supply data_frame from \code{\link{get_ilostat}} 
#'   and get data_frame with definitions instead of codes.
#' @author David Bescond \email{bescond@ilo.org}
#' @return a vector or a data_frame. The suffix ".label" is added to code column names.
#' @references
#' See citation("Rilostat")
#' ilostat bulk download facility user guidelines 
#' \url{https://webapps.ilo.org/ilostat-files/Documents/ILOSTAT_BulkDownload_Guidelines.pdf}
#' @examples
#' \dontrun{
#'  dat <- get_ilostat("UNE_2UNE_SEX_AGE_NB_A", cache = FALSE)
#'  dat_lab <- label_ilostat(dat)
#'  head(dat_lab)
#'  clean_ilostat_cache() 
#' }
#' @export

label_ilostat <- function(	x, 
							dic = NULL, 
							code = NULL, 
							lang = getOption('ilostat_lang','en')){
  
  if (is_tibble(x)){
    
	y <- x
	
	if(!is.null(code)){
	  
	  if(code[1] %in% 'all'){
	    
		code <- ilostat_cols_ref[ilostat_cols_ref %in% names(y) & !ilostat_cols_ref %in% c('time','obs_value')] 
	  
	  }
	  
	  code <- code[!stringr::str_detect(code, '.label')]
	
	}

    mynams <- names(y)[!(names(y) %in% c("time", "obs_value")) & names(y) %in% ilostat_cols_ref]
    
	for (i in mynams){
      
	  l <- label_ilostat(y[[i]], dic = i, lang = lang)
      
	  y[[i]] <- as.vector(unlist(l))
	  
	  names(y)[names(y) == i] <- paste0(i, '.label')	
    
	}

    # Codes added if asked
    if (!is.null(code)){ 
	  
	  code_in <- code %in% str_replace(names(y), '.label', '')
	  
	  if (!all(code_in)) {
	    
		stop("code column name(s) ", shQuote(code[!code_in])," not found on x")
	  
	  }
	          
      y_code <- x[, code, drop = FALSE]            
	  
	  y <- as_tibble(cbind(y_code, y))
	  
	  ref_cols <- ilostat_cols_ref[ilostat_cols_ref %in% colnames(y)]
	  
	  order_cols <- c(ref_cols, colnames(y)[!colnames(y) %in% ref_cols])
	  
	  y <- select_at(y, .vars = order_cols)
	
    }
	
  } else {
  
    if (is.null(dic)) stop("Dictionary information is missing")

    dic_df <- get_ilostat_dic(dic, lang = lang)

	if(stringr::str_detect(dic, 'note_')){
	
	y <-  as.character(
			
			mapvalues(
			  
			  as.factor(x),	
			  
			  from = levels(as.factor(x)), 
							
			  to =  unlist(
					  
					  llply(
					    
						str_split(levels(as.factor(x)), pattern = "_"),
						
						function(z) {
																			
						  str_c(dic_df[[2]][match(z, dic_df[[1]])], collapse = " | ")
																		}
					  )
					
					)
			
			, warn_missing = FALSE
			
			)
		  
		  )
	
	} else {
	
      # mapvalues
     y <- dic_df[[2]][match(x, dic_df[[1]])]  

	  
    }

  }

  # Ensure the output is tibble

  if (is.data.frame(y) && length(y) > 1) {
    
	y <- as_tibble(y)
  
  }  
  
  invisible(gc(reset = TRUE))
  
  y

}
 