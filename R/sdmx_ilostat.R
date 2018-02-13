#' @title Read Ilostat Data, Metadata via ILO DSMX api
#' @description query codelist, data and metadata via ilo sdmx api
#' @param dsd A datastructure definition, see \code{examples} section,  
#' @param sdmx_resource : a character, type of info to be returned from the sdmx api: \code{'codelist'} (default), 
#' 		 	\code{'data'}, \code{'dataflow'}, \code{'conceptref'},
#' @param sdmx_format : a character, format of info to be returned from the sdmx api: \code{'compact_2_1'} (default), 
#' 		 	coming soon: \code{'generic_2_1'}, \code{'json'},
#' @param lang a character, code for language. Available are \code{"en"} (default), 
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
#'  "http://www.ilo.org/ilostat/content/conn/ILOSTATContentServer/path/Contribution Folders/statistics/web_pages/static_pages/technical_page/ilostat_appl/SDMX_User_Guide.pdf"

#' @examples 
#' \dontrun{
#' ########## get codelist
#' # fetch collection define on ILOSTAT
#' dic <- sdmx_ilostat(dsd = "CL_COLLECTION", lang ="en")
#' head(dic)
#'
#' # fetch country available on ILOSTAT
#' dic <- sdmx_ilostat(dsd = "CL_COUNTRY", lang ="es")
#' head(dic)
#'
#' # fetch indicator define in collection STI
#' sdmx_ilostat(dsd = "CL_INDICATOR_STI", lang ="fr")
#' head(dic)
#'
#' ########## get data
#'
#' ### with attribute
#' dat <- sdmx_ilostat(dsd = 'STI_ALB_EMP_TEMP_SEX_AGE_NB', 
#'                     sdmx_resource = 'data')
#' head(dat)
#'
#' # without attribute
#' dat <- sdmx_ilostat(dsd = "STI_DEU_EMP_TEMP_SEX_AGE_NB?detail=dataonly", 
#'                     sdmx_resource = 'data')
#'
#' # of last N data
#' dat <- sdmx_ilostat(dsd = "STI_ITA_EMP_TEMP_SEX_AGE_NB?lastNObservations=1",
#'                     sdmx_resource = 'data')
#' head(dat)
#'
#' # of first N data
#' dat <- sdmx_ilostat(dsd = "STI_ARG_EMP_TEMP_SEX_AGE_NB?firstNObservations=2", 
#'                     sdmx_resource = 'data')
#' head(dat)
#'
#' ######## with multi country and advanced filters
#'
#' # to get the order of the filter first get the conceptref of the DSD
#'
#' filter_position <- sdmx_ilostat(dsd = 'STI_ALL_EMP_TEMP_SEX_AGE_NB', 
#' 					   sdmx_resource = 'conceptref')
#' filter_position
#'
#' # COUNTRY and FREQ are in second and third position of the filters
#'
#' dat <- sdmx_ilostat(dsd = "STI_ALL_EMP_TEMP_SEX_AGE_NB/.FRA+DEU.M....", 
#'                     sdmx_resource = 'data')
#' head(dat)
#'
#' # check availability of time series	
#' dat <- sdmx_ilostat(dsd = "STI_CHL_EMP_TEMP_SEX_AGE_NB/......?detail=serieskeysonly", 
#'                     sdmx_resource = 'data')
#' head(dat)
#'
#' # as from 2009
#' sdmx_ilostat("STI_ZAF_EMP_TEMP_SEX_AGE_NB/......?startPeriod=2009-01-01&detail=serieskeysonly",
#'                     sdmx_resource = 'data')
#'
#' # as from 2009
#' dat <- sdmx_ilostat("STI_FRA_UNE_TUNE_SEX_AGE_NB/STI.FRA.M.48..", 
#'                     sdmx_resource = 'data')
#' head(dat)
#'
#' ########## dataflow available
#'
#' flow <- sdmx_ilostat("STI_TTO_MULTI", sdmx_resource = 'dataflow')
#'
#' flow <- sdmx_ilostat("KI_ALL_EMP_MULTI", sdmx_resource = 'dataflow')
#'
#' flow <- sdmx_ilostat("YI_FRA_UNE_MULTI", sdmx_resource = 'dataflow')
#'
#' ########## count data available
#'
#' # with multi country
#' sdmx_ilostat("STI_FRA_UNE_TUNE_SEX_AGE_NB/STI.FRA.M.48..", 
#'                      sdmx_resource = 'data', count = TRUE)
#'
#' }
#' @export


sdmx_ilostat <- function(	dsd, 
							sdmx_resource = getOption('ilostat_sdmx_resource', 'codelist'),
							sdmx_format = getOption('ilostat_sdmx_format', 'compact_2_1'),
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
	
	y <- sdmx_ilostat_dataflow(dsd, sdmx_format, quiet)
  
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
				paste0(dsd, "&format=",								sdmx_format), 
				paste0(dsd, "?format=", sdmx_format)
			)
	
	mypath <- paste0("http://www.ilo.org/ilostat/sdmx/ws/rest/data/ILO,DF_",dsd)
	
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
	len_x <- length(xml_find_all(X, ".//message:DataSet", ns))
	
	if(len_x ==0){ 
	
		if(!quiet){
		
		  message("Query with dsd = '", dsd, "': Dataset does not exist.")
		
		}
		
		return(NULL)
	
	}
	
	if(!quiet){
		
	  message('data from : ', mypath )	
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
				
				xml_attrs(y) %>% as.list %>% as_data_frame %>% mutate("tmpvars" = 1),
				
				xml_attrs(xml_find_all(y, ".//Obs", ns)) %>% lapply(as.list) %>% bind_rows %>% mutate("tmpvars" = 1)  , 
				
				by = "tmpvars"
				
				) %>% 
				
				select(-contains("tmpvars")) 
			  
			  }
			
			) %>% 
			
			bind_rows 
	}
	
	invisible(gc(reset = TRUE))
	
	y
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
		   
		   paste0(dsd, "&format=", sdmx_format), 
		   
		   paste0(dsd, "?format=", sdmx_format))
  
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
  
  mypath <- paste0("http://www.ilo.org/ilostat/sdmx/ws/rest/data/ILO,DF_",dsd)
  
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
									# sdmx_format,
									lang,
									quiet){

  mypath <- paste0(
	          
			  "http://www.ilo.org/ilostat/sdmx/ws/rest/codelist/ILO/",
			  
			  dsd, 
			  
			  '?format=', 'compact_2_1'
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

  if(length(xml_find_all(X, ".//str:Codelists", ns))==0){ # empty test 
		
	message("Query with dsd = '", dsd, "' Dataset does not exist.")
	
    return(NULL)
	
  } 
 
  message('codelist from : ', mypath )
  
  xml_find_all(X,".//str:Code", ns) %>%
	
	ldply( 
	  
	  function(y){
		
		Code <- c(code = xml_attr(y,"id"))
		
		Label <- c(label = xml_text(xml_find_all(y,".//com:Name",ns)[xml_attr(xml_find_all(y,".//com:Name",ns),"lang")%in%lang]))
		
		Annotation <- NULL
		if(length(xml_find_all(y,".//com:Annotation",ns))>1){
		  for (i in 1:length(xml_text(xml_find_all(y,".//com:AnnotationTitle",ns)))){
		
			if(is.na(xml_text(xml_find_all(xml_find_all(y,".//com:Annotation",ns)[i] ,".//com:AnnotationText",ns))[xml_attr(xml_find_all(y,".//com:AnnotationText",ns),"lang")%in%lang][1]) ){
			
			  Annotation[[ xml_text(xml_find_all(xml_find_all(y,".//com:Annotation",ns)[i] ,".//com:AnnotationTitle",ns)) ]] <- xml_text(xml_find_all(xml_find_all(y,".//com:Annotation",ns)[i] ,".//com:AnnotationText",ns))
						
			} else {
			
			  Annotation[[ xml_text(xml_find_all(xml_find_all(y,".//com:Annotation",ns)[i] ,".//com:AnnotationTitle",ns)) ]] <- xml_text(xml_find_all(xml_find_all(y,".//com:Annotation",ns)[i] ,".//com:AnnotationText",ns))[xml_attr(xml_find_all(y,".//com:AnnotationText",ns),"lang")%in%lang][1]
			
		    }
	
		  }
		  
		}
			
		Description <- c(description = xml_text(xml_find_all(y,".//com:Description",ns)[xml_attr(xml_find_all(y,".//com:Description",ns),"lang")%in%lang]))

		c(Code, Label, Annotation, Description) %>% t %>% as_data_frame
								
	  }
	) %>% 
	
	as_data_frame 
}

sdmx_ilostat_dataflow <- function(  dsd,
									sdmx_format,
									quiet){
  mypath <- paste0(
	          
			  "http://www.ilo.org/ilostat/sdmx/ws/rest/dataflow/ILO/DF_",
			  
			  dsd, 
			  
			  '?format=', sdmx_format
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
  if(length(xml_find_all(X, ".//str:Dataflow", ns))==0){ 

	if(!quiet){
		  
	  message("Query with dsd = '", dsd, "' Dataset does not exist.")
	
	}
	
	return(NULL)
  
  }

  if(!quiet){

    message('dataflow from : ', mypath )

  }  
  
  xml_attr(xml_find_all(X, ".//str:Dataflow", ns), 'id')

}


sdmx_ilostat_conceptRef <- function(dsd,
									quiet){		

  mypath <- paste0(
	          
			  "http://www.ilo.org/ilostat/sdmx/ws/rest/datastructure/ILO/",
			  
			  dsd, 
			  
			  '?format=', 'generic_2_1'
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
  if(length(xml_find_all(X, ".//str:DimensionList", ns))==0){ 
  
	message("Query with dsd = '", dsd, "' Dataset does not exist.")

	y <- NULL
  
  } else {
  
    y <-  xml_attr(xml_find_all(xml_find_all(X, ".//str:DimensionList", ns), ".//str:Dimension", ns), 'id') 
	
  }

  message('dimension from : ', mypath )

  y								

}

