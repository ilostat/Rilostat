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
#' Date: \tab 2015-2017\cr
#' License: \tab BSD_2_clause + LICENSE\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' R Tools for ilostat Open Data
#'
#' @name ilostat
#'
#' @param id a character, file that to use if \code{NULL} return the list of all available file. 
#'
#' @author David Bescond \email{bescond@ilo.org}
#' @references
#' See citation("Rilostat")
#'
#' ilostat bulk download facility user guidelines 
#' \url{http://www.ilo.org/ilostat-files/WEB_bulk_download/ILOSTAT_BulkDownload_Guidelines.pdf}
#'
#' @keywords package
#' @importFrom readr read_csv
#' @importFrom readr col_character
#' @importFrom readr col_double
#' @importFrom readr cols
#' @importFrom plyr llply
#' @importFrom plyr ldply
#' @importFrom plyr mapvalues
#' @importFrom dplyr %>%
#' @importFrom dplyr select_
#' @importFrom dplyr select
#' @importFrom dplyr filter_
#' @importFrom dplyr contains
#' @importFrom dplyr distinct
#' @importFrom dplyr distinct_
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_
#' @importFrom dplyr summarise_
#' @importFrom dplyr group_by_
#' @importFrom dplyr ungroup
#' @importFrom haven read_dta
#' @importFrom haven read_sav
#' @importFrom haven read_sas
#' @importFrom haven write_dta
#' @importFrom haven write_sav
#' @importFrom haven write_sas
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_c
#' @importFrom stringr str_sub
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom stringr fixed
#' @importFrom tibble data_frame
#' @importFrom tibble is_tibble
#' @importFrom tibble as_data_frame
#' @importFrom utils download.file
#' @importFrom utils install.packages
#' @importFrom utils installed.packages
#' @importFrom utils menu
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attrs
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_ns
#' @importFrom RCurl ftpUpload
#' @importFrom data.table fwrite
#' @importFrom DT datatable
#' @examples
#' \dontrun{
#'
#' # check which documentation have been recently added:
#'
#' ilostat() 
#'
#' # get the basic first vignette:
#' 
#' ilostat('GettingStarted') 
#' 
#' }
#' @export

ilostat <- function(id = NULL)
{

  readMe <- paste0(ilostat_web, '_ReadMe.csv')
  
  y <- read_csv(readMe, col_types = cols(.default = col_character()), progress = FALSE)

  if(is.null(id) || id == 'all'){

    for (i in 1:nrow(y)){
	  # href links
	  if(y$directory[i] %in% 'tutorials'){
       
    	y$also[i] <- str_replace(y$also[i], 'html', paste0("<a href='",paste0(ilostat_web, "tutorials/", y$id[i], ".html"),"'>html</a>"))
 	 
	    y$also[i] <- str_replace(y$also[i], 'pdf', paste0("<a href='",paste0(ilostat_web, "tutorials/", y$id[i], ".pdf"),"'>pdf</a>"))
 	 
	    y$also[i] <- str_replace(y$also[i], 'docx', paste0("<a href='",paste0(ilostat_web, "tutorials/", y$id[i], ".docx"),"'>docx</a>"))
       
	    y$also[i] <- str_replace(y$also[i], 'source', paste0("<a href='",paste0(ilostat_web, "tutorials/", y$id[i], ".Rmd"),"'>source</a>"))
       
	    y$id[i] <- paste0("<a href='",paste0(ilostat_web, "tutorials/",y$id[i], ".html"),"'>",y$id[i],"</a>")
       
	  } 
	  if(y$directory[i] %in% 'gadgets'){
      	  
	    y$also[i] <- str_replace(y$also[i], 'source', paste0("<a href='",paste0(ilostat_web, "gadgets/", y$id[i], ".r"),"'>source</a>"))
			
	  }
    }

	if(is.null(id)){
   
      y <- y[y$status %in% 'public', ]
   
    }
   
    datatable(	y, 
				filter = 'top', 
				options = 	list(
								pageLength = 20
							), 
				escape = FALSE
    )
	
  } else {
  
    id <- tolower(id)
  
    y <- y %>% filter_(.dots = paste0("tolower(id) %in% '", id, "'"))
  
    if(nrow(y) == 0 || nrow(y) > 1){

      stop("id: ", id, " not found, plse check id validity with ilostat_plus()")
    
	} else {
      if(y$directory %in% 'tutorials'){
	  
	    utils::browseURL(paste0(ilostat_web, '/tutorials/',y$id,".html"))
	  
	  }
	  if(y$directory %in% 'gadgets'){
	  	  
	    test <- check_pkg_for_ilostat_apps(str_split(y$add.packages, ';') %>% unlist)
		source(paste0(ilostat_web,'/gadgets/',y$id,".r") , local = TRUE) # globalenv()
		# on.exit({
		#		exit_ilostat_apps()
		#		rm(exit_ilostat_apps, envir = globalenv())
		#	})
		#run_ilostat_apps()

	  }

    }  
	

  }
  
}



.ilostatEnv <- new.env()

