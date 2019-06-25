#' @title Read Ilostat Data, Metadata via ILO SDMX api
#' @description query codelist, data and metadata via ilo sdmx api
#' @param dsd A datastructure definition, see \code{examples} section,  
#' @param sdmx_resource : a character, type of info to be returned from the sdmx api: \code{'codelist'} (default), 
#' 		 	\code{'data'}, \code{'dataflow'}, \code{'conceptref'},
#' @param sdmx_format : for data only, a character, format of info to be returned from the sdmx api: \code{'structurespecificdata'} (default) also available \code{'csv'}.
#' @param lang a character for language. Available are \code{"en"} (default), 
#'        \code{"fr"} and \code{"es"}. Can be set also with options(ilostat_lang = 'fr'),
#' @param count a logical, count data records only if \code{resource = 'data'}, \code{FALSE} (default), 
#' @param quiet a logical, if \code{TRUE} , don't return message from processing, \code{FALSE} (default).
#' 			Can be set also with options(ilostat_quiet = TRUE).
#' @author David Bescond \email{bescond@ilo.org}
#' @references
#'
#' See citation("Rilostat")
#' 
#' ilostat sdmx user guidelines:
#'  "https://www.ilo.org/ilostat-files/Documents/SDMX_User_Guide.pdf"

#' @examples 
#' \dontrun{
#' ########## get codelist
#' # fetch indicator define on ILOSTAT
#'  dic <- sdmx_ilostat(dsd = "CL_INDICATOR", lang ="en")
#'  head(dic)
#'
#' # fetch country available on ILOSTAT
#'  dic <- sdmx_ilostat(dsd = "CL_AREA", lang ="es")
#'  head(dic)
#'
#' # fetch classif ECO available on ILOSTAT
#'  dic <- sdmx_ilostat(dsd = "CL_ECO", lang ="en")
#'  head(dic)
#'
#' # fetch classif ECO verion available on ILOSTAT
#'  dic <- sdmx_ilostat(dsd = "CL_CLASSIF_ECO", lang ="en")
#'  head(dic)
#'
#' # fetch note type available on ILOSTAT
#'  dic <- sdmx_ilostat(dsd = "CL_NOTE_TYPE", lang ="en")
#'  head(dic)
#'
#' # fetch note "Repository" available on ILOSTAT
#'  dic <- sdmx_ilostat(dsd = "CL_NOTE_R1", lang ="en")
#'  head(dic)
#'
#' ########## get data
#'
#' ### with attribute
#'  dat <- sdmx_ilostat(dsd = 'STI_ALB_EMP_TEMP_SEX_AGE_NB', 
#'                     sdmx_resource = 'data')
#'  head(dat)
#'
#' # without attribute
#'  dat <- sdmx_ilostat(dsd = "STI_DEU_EMP_TEMP_SEX_AGE_NB?detail=dataonly", 
#'                     sdmx_resource = 'data')
#'
#' # of last N data
#'  dat <- sdmx_ilostat(dsd = "STI_ITA_EMP_TEMP_SEX_AGE_NB?lastNObservations=1",
#'                     sdmx_resource = 'data')
#'  head(dat)
#'
#' # of first N data
#'  dat <- sdmx_ilostat(dsd = "STI_ARG_EMP_TEMP_SEX_AGE_NB?firstNObservations=2", 
#'                     sdmx_resource = 'data')
#'  head(dat)
#'
#' ######## with multi country and advanced filters
#'
#' # to get the order of the filter first get the conceptref of the DSD
#'
#'  filter_position <- sdmx_ilostat(dsd = 'STI_ALL_EMP_TEMP_SEX_AGE_NB', 
#'  				   sdmx_resource = 'conceptref')
#'  filter_position
#'
#' # COUNTRY and FREQ are in second and third position of the filters
#'
#'  dat <- sdmx_ilostat(dsd = "STI_ALL_EMP_TEMP_SEX_AGE_NB/.FRA+DEU.M....", 
#'                     sdmx_resource = 'data')
#'  head(dat)
#'
#'
#' # as from 2009
#'  dat <- sdmx_ilostat("STI_FRA_UNE_TUNE_SEX_AGE_NB/STI.FRA.M.5893..", 
#'                     sdmx_resource = 'data')
#'  head(dat)
#'
#' ########## dataflow available
#'
#'  flow <- sdmx_ilostat("STI_TTO_MULTI", sdmx_resource = 'dataflow')
#'
#'  flow <- sdmx_ilostat("KI_ALL_EMP_MULTI", sdmx_resource = 'dataflow')
#'
#'  flow <- sdmx_ilostat("YI_FRA_UNE_MULTI", sdmx_resource = 'dataflow')
#'
#' ########## count data available
#'
#' # with multi country
#'  sdmx_ilostat("STI_FRA_UNE_TUNE_SEX_AGE_NB/STI.FRA.M.5893...", 
#'                      sdmx_resource = 'data', count = TRUE)
#'
#' }
#' @export


sdmx_ilostat <- function(	dsd, 
							sdmx_resource = getOption('ilostat_sdmx_resource', 'codelist'),
							sdmx_format = getOption('ilostat_sdmx_format', 'structurespecificdata'),
							lang  = getOption('ilostat_lang', 'en'), 
							count = getOption('ilostat_sdmx_count', FALSE),
							quiet = getOption('ilostat_quiet', FALSE)){
  
  if(tolower(sdmx_resource) == 'data'){	

	if(!count){
	  
	  y <- sdmx_ilostat_data(dsd, sdmx_format, quiet)	
	
	} else {
	   
	   y <- sdmx_ilostat_count(dsd, sdmx_format, quiet)
	
	}		
  
  } else if(tolower(sdmx_resource) == 'codelist'){

	y <- sdmx_ilostat_codelist(dsd, lang)
   
  } else if(tolower(sdmx_resource) == 'dataflow'){
	
	y <- sdmx_ilostat_dataflow(dsd,  quiet)
  
  } else if(tolower(sdmx_resource) == 'conceptref'){
	
	y <- sdmx_ilostat_conceptRef(dsd, quiet)
  
  }

  y
  
}

sdmx_ilostat_data <- function (	dsd,
								sdmx_format,
								quiet){

	# add attribute format compact
	dsd <- 	ifelse(	
				stringr::str_detect(dsd,"[?]"), 
				paste0(dsd, "&format=",	sdmx_format), 
				paste0(dsd, "?format=", sdmx_format)
			)
	
	mypath <- paste0("https://www.ilo.org/sdmx/rest/data/ILO,DF_",dsd)
	

	if(sdmx_format %in% 'csv') {
			
			X <- 	try(read_csv(mypath, col_types = cols(.default = col_character()), progress = FALSE), silent = TRUE)
	}
	
	if (sdmx_format %in% 'structurespecificdata'){
	
			X <- 	try(
	
				read_xml(mypath), 
				
				silent = TRUE
			)
	
	}
	
	# test error message
	if(substr(X[1], 1, 5)%in%"Error"){ 
		
		if(!quiet){
		
		  if(stringr::str_detect(X[1], 'HTTP error 413')){
		  
		    message("'Error message dsd exceeding threshold 300000, please apply filters to generate a smaller dataset.")
			
		  }
		  
		  if(stringr::str_detect(X[1], 'HTTP error 400| HTTP error 404')){
		  
		   message("'Error message dsd is invalid.")
		   
		  }
		  
		message('check : ', mypath 	)
		
		}
		
		return(NULL)
	
	}
	
	invisible(gc(reset = TRUE))
	
	if(!quiet){
		
		message('data from : ', mypath )	
	}
	
	if (sdmx_format %in% 'structurespecificdata'){
	
		# extract namespace of the xml doc
		ns <- xml_ns(X)		
	
	# test dataset exist
		len_x <- length(xml_find_all(X, ".//message:DataSet", ns))
	
		if(len_x ==0){ 
	
			if(!quiet){
		
			message("Query with dsd = '", dsd, "': Dataset does not exist.")
		
			}
		
			return(NULL)
	
		}
	

	
		# return SeriesKey only
		if(str_detect(dsd,"detail=serieskeysonly")){ 
	  
			y <- xml_attrs(xml_find_all(X, ".//Series", ns)) %>% lapply(as.list)  %>% bind_rows  
	
		} else {
	
		# return all 
	  y <-  llply(
			  
			  xml_find_all(X, ".//Series", ns), 
			  
			  function(y){ 
				
				left_join(
				
				xml_attrs(y) %>% as.list %>% as_tibble %>% mutate("tmpvars" = 1),
				
				xml_attrs(xml_find_all(y, ".//Obs", ns)) %>% lapply(as.list) %>% bind_rows %>% mutate("tmpvars" = 1)  , 
				
				by = "tmpvars"
				
				) %>% 
				
				select_at(vars(-contains("tmpvars"))) 
				
			  }
			
			) %>% 
			
			bind_rows 
		
			rm(X)
		
		}
	
	}
	
	invisible(gc(reset = TRUE))
	

	
	
	if(sdmx_format %in% 'csv') {
	return(X) } else {return(y)}
	
	
	
	
}

sdmx_ilostat_count <- function( dsd, 
								sdmx_format,
								quiet){


  Detail  <- grep("\\?", dsd)%in%1 
  
  if(length(Detail)%in%0){
	
	Detail 	<- FALSE
  
  }
  
  dsd <- ifelse(	
		   
		   stringr::str_detect(dsd,"[?]"), 
		   
		   paste0(dsd, "&format=", 'structurespecificdata'), 
		   
		   paste0(dsd, "?format=", 'structurespecificdata'))
  
  # set if SeriesKeysOnly is requested (NO Obs, No Attrs)
  SeriesKeysOnly 	<- grep("DETAIL=SERIESKEYSONLY", toupper(dsd))%in%1 	
  
  if(length(SeriesKeysOnly)%in%0){
  
    SeriesKeysOnly 	<- FALSE
  }
  
  # set if DataOnly are requested (No Attrs)
  DataOnly 	<- grep("DETAIL=DATAONLY", toupper(dsd))%in%1	
  
  if(length(DataOnly)%in%0){
  
    DataOnly 	<- FALSE
  
  }


  if(Detail & !SeriesKeysOnly & !DataOnly){
    
	dsd <- paste0(dsd,"&detail=dataonly")
    
	DataOnly = TRUE 
  
  }

  if(!Detail){
	
	dsd <- paste0(dsd,"&detail=dataonly") 
	
	DataOnly = TRUE
  
  }
  
  mypath <- paste0("https://www.ilo.org/sdmx/rest/data/ILO,DF_",dsd)
  
  X <- 	try(
             
			 read_xml(mypath), 
		   silent = TRUE
		)

  
  # test error message
  if(substr(X[1], 1, 5)%in%"Error"){ 
    
	if(!quiet){
	  
	  if(stringr::str_detect(X[1], 'HTTP error 413')){
		  
		    message("'Error message dsd exceeding threshold 300000, please apply filters to generate a smaller dataset.")
			
		  }
		  
		  if(stringr::str_detect(X[1], 'HTTP error 400')){
		  
		   message("'Error message dsd is invalid.")
		   
		  }
		  
		message('check : ', mypath 	)
    
	}
	
	return(NULL)
  
  } 
	
  # extract namespace of the xml doc
  ns <- xml_ns(X)		

  # test dataset exist
  if(length(xml_find_all(X, ".//message:DataSet", ns))==0){ 
	
	if(!quiet){
		
	  message("Query with dsd = '", dsd, "' Dataset does not exist.")
	
	}
	
	return(NULL)
  
  }
	
  if(!quiet){
    
	message('data count from : ', mypath )
  
  }
  
  if(DataOnly){
	
	length(xml_find_all(X, ".//Obs", ns)) 
  
  } else {
	
	length(xml_find_all(X, ".//Series", ns)) 
  }

}

sdmx_ilostat_codelist	<- function(dsd, 
									lang,
									quiet){

  mypath <- paste0(
	          
			  "https://www.ilo.org/sdmx/rest/codelist/ILO/",
			  
			  dsd
			)
			
  X <- try(	
		
		read_xml(mypath, 
		
		encoding = "UTF-8"), 
		
		silent = TRUE
	  
	  )

  if(substr(X[1], 1, 5)%in%"Error"){ # error test
	
	message("Query with dsd = '", dsd, "' Error message return.")
	
	return(NULL)
  
  }
  
  ns <- xml_ns(X)	# extract namespace of the xml doc

  if(length(xml_find_all(X, ".//structure:Codelists", ns))==0){ # empty test 
		
	message("Query with dsd = '", dsd, "' Dataset does not exist.")
	
    return(NULL)
	
  } 
 
  message('codelist from : ', mypath )
  
  xml_find_all(X,".//structure:Code", ns) %>%
	
	ldply( 
	  
	  function(y){
		
		MyCode <- c(code = xml_attr(y,"id"))
		MyLabel <- c(label = xml_text(xml_find_all(y,".//common:Name",ns)[xml_attr(xml_find_all(y,".//common:Name",ns),"lang")%in%lang]))
		
		Annotation <- NULL
		if(length(xml_find_all(y,".//common:Annotations",ns))>0){
		  for (i in 1:length(xml_text(xml_find_all(y,".//common:AnnotationType",ns)))){
		
			if(is.na(xml_text(xml_find_all(xml_find_all(y,".//common:Annotation",ns)[i] ,".//common:AnnotationText",ns))[xml_attr(xml_find_all(y,".//common:AnnotationText",ns),"lang")%in%lang][1]) ){
				
				test <- xml_text(xml_find_all(xml_find_all(y,".//common:Annotation",ns)[i] ,".//common:AnnotationText",ns))
				if(length(test) !=0 ){
					Annotation[[ xml_text(xml_find_all(xml_find_all(y,".//common:Annotation",ns)[i] ,".//common:AnnotationType",ns)) ]] <- xml_text(xml_find_all(xml_find_all(y,".//common:Annotation",ns)[i] ,".//common:AnnotationText",ns))
				} else {
					Annotation[[ xml_text(xml_find_all(xml_find_all(y,".//common:Annotation",ns)[i] ,".//common:AnnotationType",ns)) ]] <- NA
				
				}
			} else {
			
			  Annotation[[ xml_text(xml_find_all(xml_find_all(y,".//common:Annotation",ns)[i] ,".//common:AnnotationType",ns)) ]] <- xml_text(xml_find_all(xml_find_all(y,".//common:Annotation",ns)[i] ,".//common:AnnotationText",ns))[xml_attr(xml_find_all(y,".//common:AnnotationText",ns),"lang")%in%lang][1]
			
		    }
	
		  }
		  
		}
			
		Description <- c(description = xml_text(xml_find_all(y,".//common:Description",ns)[xml_attr(xml_find_all(y,".//common:Description",ns),"lang")%in%lang]))

		c(MyCode, MyLabel, Annotation, Description) %>% t %>% as_tibble
								
	  }
	) %>% 
	
	as_tibble %>% filter(eval(parse(text = "!code %in% '_Z'")))
}

sdmx_ilostat_dataflow <- function(  dsd,
									quiet){
  mypath <- paste0(
	          
			  "https://www.ilo.org/sdmx/rest/dataflow/ILO/DF_",
			  
			  dsd
			)
  
  X <- 	try(
  
          read_xml(mypath), 
		  
		  silent = TRUE
		  
		)
  
  # test error message
  if(substr(X[1], 1, 5)%in%"Error"){ 
    
	if(!quiet){
		
	  message("Query with dsd = '", dsd, "' Error message return.")
    
	}
	
	return(NULL)
  
  }  
	
  # extract namespace of the xml doc
  ns <- xml_ns(X)		

  # test dataset exist
  if(length(xml_find_all(X, ".//structure:Dataflow", ns))==0){ 

	if(!quiet){
		  
	  message("Query with dsd = '", dsd, "' Dataset does not exist.")
	
	}
	
	return(NULL)
  
  }

  if(!quiet){

    message('dataflow from : ', mypath )

  }  
  
  xml_attr(xml_find_all(X, ".//structure:Dataflow", ns), 'id')

}


sdmx_ilostat_conceptRef <- function(dsd,
									quiet){		

  mypath <- paste0(
	          
			  "https://www.ilo.org/sdmx/rest/datastructure/ILO/",
			  
			  dsd
			)
  
  X <- try(	
		
	  read_xml(mypath, 
		
	  encoding = "UTF-8"), 
		
	  silent = TRUE
	  
	)

	if(substr(X[1], 1, 5)%in%"Error"){ 
		
		if(!quiet){

		   message("'Error message dsd is invalid.")

		message('check : ', mypath 	)
		
		}
		
		return(NULL)
	
	}
  
    # extract namespace of the xml doc
  ns <- xml_ns(X)	

  # test dataset exist
  if(length(xml_find_all(X, ".//structure:DimensionList", ns))==0){ 
  
	message("Query with dsd = '", dsd, "' Dataset does not exist.")

	y <- NULL
  
  } else {
  
    y <-  xml_attr(xml_find_all(xml_find_all(X, ".//structure:DimensionList", ns), ".//structure:Dimension", ns), 'id') 
	
  }

  message('dimension from : ', mypath )

  y								

}

