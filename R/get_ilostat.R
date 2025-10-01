#' @title Read ilostat Data
#' @description Download datasets from ilostat \url{https://ilostat.ilo.org} via bulk download facility 
#' \url{https://ilostat.ilo.org/data/bulk/}.
#' @param id A code name for the dataset of interest.
#'        See \code{\link{get_ilostat_toc}} or details for how to get code.
#' @param segment A character, way to get datasets by: \code{"indicator"} (default) or \code{"ref_area"}.
#'        Can be set also with options(ilostat_segment = 'ref_area'),   
#' @param type a character, type of variables, \code{"code"} (default), \code{"label"} or \code{"both"}.
#'        Can be set also with options(ilostat_type = 'both'),  
#' @param lang a character, code for language. Available are \code{"en"} (default), 
#'        \code{"fr"} and \code{"es"}. Can be set also with options(ilostat_lang = 'fr'),
#' @param time_format a string giving a type of the conversion of the time
#'        column from the ilostat format. "raw" (default)
#'        does not do conversion and return time as character (ie. '2017', '2017Q1', '2017M01'). A "date" converted to
#'        a \code{\link{Date}} with a first date of the period. A "date_last" converted to a \code{\link{Date}} with
#'        a last date of the period and "num" converted to a numeric. Can be set also with 
#'        options(ilostat_time_format = 'date'),  
#' @param filters a list; \code{"none"} (default) to get a whole dataset or a named list of
#'			filters to get just part of the table. Names of list objects are
#'			ilostat variable codes and values are vectors of observation codes.
#'			filters detect on variables, so could be partial, ie. \code{list(sex = 'T')} is
#'			enough but equivalent to \code{list(sex = 'SEX_T')}. Additional options:
#'			enough but equivalent to \code{list(sex = 'SEX_T')}. Additional options:
#' 			\itemize{ 
#' 				\item{\code{timefrom}} : starting year of the return dataset,
#' 				\item{\code{timeto}} : ending year of the return dataset.
#'    		}
#' @param fixed a logical, if \code{TRUE} (default), filters arguments pattern is a string to be matched as is,
#'        Change to \code{FALSE} if more complex regex matching is needed.
#' @param detail a character, \code{'full'} (default), whether \code{'serieskeysonly'}, return
#' 		  only key, no data, no notes or \code{dataonly} return key and data, no notes or \code{'bestsourceonly'} return 
#'        best source only at the series key levels, see details.
#' 		  Can be set also with options(ilostat_detail = 'serieskeysonly'),
#' @param cache a logical whether to do caching. Default is \code{TRUE}. Affects
#' 			only queries from the ilostat bulk download facility. 
#'			Can be set also with options(ilostat_cache = FALSE),
#' @param cache_update a logical whether to update cache. Check cache update with last.update attribute store on the cache file name 
#' 			and the one from the table of contents. Can be set also with
#'        	options(ilostat_cache_update = FALSE). Default is \code{TRUE},  
#' @param cache_dir a path to a cache directory. The directory has to exist.
#'        	The \code{NULL} (default) uses and creates
#'        	'ilostat' directory in the temporary directory from
#'        	\code{\link{tempdir}}. The directory can also be set with
#'        	\code{option} ilostat_cache_dir,
#' @param cache_format a character, format to store on the cache \code{"rds"} (default), but also \code{"csv"}, \code{"dta"}, 
#'        \code{"sav"}, \code{"sas7bdat"}. useful for getting ilostat dataset directly on the \code{cache_dir} without R.  
#' 			Can be set also with options(ilostat_cache_format = 'dta'),  
#' @param back a logical, \code{TRUE} return dataframe on R or not \code{FALSE}, useful for just saving file in specific \code{cache_format},
#' @param cmd a character, R expression use for manipulate internal data frame \code{dat} object applied to each datasets retrieved 
#         after filters and type setting. Manipulation should return data.frame \code{'none'} (default),
#'        If use, \code{cache} is set to FALSE. see examples. 
#' @param quiet a logical, if \code{TRUE} , don't return message from processing, \code{FALSE} (default).
#' 			Can be set also with options(ilostat_quiet = TRUE), 
#' @author David Bescond \email{bescond@ilo.org} 
#'
#' @section others:
#'
#'   Data sets are downloaded from the
#'   ilostat bulk download facility. 
#'   If only the table \code{id} is given, the whole table is downloaded from the
#'   bulk download facility. 
#' 
#'   The bulk download facility is the fastest method to download whole datasets.
#'
#'   By default datasets from the bulk download facility are cached as they are
#'   often rather large. 
#'
#'   Cache files are stored in a temporary directory by default or in
#'   a named directory if cache_dir or option ilostat_cache_dir is defined.
#'   The cache can be emptied with \code{\link{clean_ilostat_cache}}.
#' 
#'   The \code{id}, a code, for the dataset can be searched with
#'   the \code{\link{get_ilostat_toc}} or from the [bulk download facility](https://ilostat.ilo.org/data/bulk/).
#'
#' @references
#' See citation("Rilostat")
#' ilostat bulk download facility user guidelines \url{https://ilostat.ilo.org/data/bulk/}
#' @return a tibble. One column for each dimension in the data and
#'         the values column for numerical values, as well as the metadata columns.
#'         The time column for a time dimension.  
#' @seealso \code{\link{get_ilostat_toc}}, \code{\link{label_ilostat}}
#' @examples 
#' \dontrun{
#' ############# get simple dataset
#'  dat <- get_ilostat("UNE_2UNE_SEX_AGE_NB_A")
#'  head(dat)
#'  dat <- get_ilostat("NZL_Q", segment = "ref_area")
#'  head(dat)
#'
#'  dir.create(file.path(tempdir(), "r_cache"))
#'  dat <- get_ilostat("UNE_2UNE_SEX_AGE_NB_A", 
#'                   cache_dir = file.path(tempdir(), "r_cache"))
#'  head(dat)
#'
#'  clean_ilostat_cache(cache_dir = file.path(tempdir(), "r_cache")) 
#'
#'  options(ilostat_update = TRUE)
#'  dat <- get_ilostat("UNE_2UNE_SEX_AGE_NB_A")
#'  head(dat)
#'  options(ilostat_update = FALSE)
#'  options(ilostat_cache_dir = file.path(tempdir(), "r_cache"))
#'  dat <- get_ilostat("UNE_2UNE_SEX_AGE_NB_A")
#'
#'  clean_ilostat_cache() 
#'
#' ############# get multiple datasets
#'  dat <- get_ilostat(c("CPI_ACPI_COI_RT_M", 'CPI_ACPI_COI_RT_Q'), cache = FALSE)
#'  head(dat)
#'  toc <- get_ilostat_toc(search = 'CPI_')
#'  head(toc)
#'  dat <- get_ilostat(toc, cache = FALSE) #id as a tibble
#'
#' ############# get datasets with filters
#'  dat <- get_ilostat(id = c("UNE_2UNE_SEX_AGE_NB_A", 'EMP_2EMP_SEX_AGE_NB_A'), 
#'  		filters = list(	ref_area = "FRA", 
#'  		classif1 = "AGE_YTHADULT_YGE15", 
#'  		time = "2016",
#'  		sex = c("T", 'SEX_F')), quiet = TRUE)
#'  head(dat)
#'  clean_ilostat_cache() 
#'
#' ############# store in other format
#'  dir.create(file.path(tempdir(), "ilostat"))
#'
#'  dat <- get_ilostat("UNE_2UNE_SEX_AGE_NB_A", 
#'                   cache_dir = file.path(tempdir(), "r_cache"), cache_format = 'csv')
#'  dat <- get_ilostat("UNE_2UNE_SEX_AGE_NB_A", 
#'                   cache_dir = file.path(tempdir(), "r_cache"), cache_format = 'dta')
#'
#' ############# advanced manipulation
#'
#'  dat <- get_ilostat("UNE_2UNE_SEX_AGE_NB_A", cmd = "dat %>% count(ref_area)", quiet = TRUE)
#'  label_ilostat(dat, code = 'ref_area')
#'
#'  clean_ilostat_cache()
#' }
#' @export

get_ilostat <- function(id, 
						segment = getOption('ilostat_segment', 'indicator'), 
						type = getOption('ilostat_type', 'code'),
						lang = getOption('ilostat_lang', 'en'),
						time_format = getOption('ilostat_time_format', 'raw'),
						filters = getOption('ilostat_filter', 'none'),
						fixed = getOption('ilostat_fixed', TRUE),
						detail = getOption('ilostat_detail', 'full'),
						cache = getOption('ilostat_cache', TRUE),
						cache_update = getOption('ilostat_cache_update', TRUE),
						cache_dir = getOption('ilostat_cache_dir', NULL),
						cache_format = getOption('ilostat_cache_format', 'rds'),
						back = getOption('ilostat_back', TRUE),
						cmd = getOption('ilostat_cmd', 'none'),
						quiet = getOption('ilostat_quiet', TRUE)){

  # get multi id from tibble
  if (is_tibble(id)) { 
    
	ref_id <- unique(id$id) 
  
  } else {
  
    ref_id <- unique(id)
  
  }
  
    if(stringr::str_detect(tolower(segment), 'model')){
	
	lang <- 'en' 
	segment <- 'modelled_estimates'
  
  }

  dat <- NULL

  for (i in 1:length(ref_id)) {
	    
    dat <- 	bind_rows(
				
				dat, 
				
				get_ilostat_dat(id = ref_id[i], segment, type, lang, time_format, filters, fixed, detail, cache, cache_update,	cache_dir, cache_format, back, cmd, quiet 
								
				)
			)
  }

  ref_cols <- ilostat_cols_ref[ilostat_cols_ref %in% colnames(dat)]
	
  order_cols <- c(ref_cols, colnames(dat)[!colnames(dat) %in% ref_cols])
	
  select_at(dat, .vars = order_cols)
 
}


get_ilostat_dat <- function(id, 
							segment, 
							type, 
							lang, 
							time_format,
							filters,
							fixed,
							detail,
							cache, 
							cache_update, 
							cache_dir,
							cache_format,
							back,
							cmd, 
							quiet 
							){

  # check id validity and return last update
  ref_id <- id
  last_toc_update <- get_ilostat_toc(segment, lang) %>% 
					 filter(id %in% ref_id) 
  last_toc_update <- unique(last_toc_update$last.update)
					
  last_toc_update <- ifelse(substr(last_toc_update, 6,8) %in% '/20', 
												format(strptime(last_toc_update, '%d/%m/%Y  %H:%M'), '%Y%m%dT%H%M'), 
												last_toc_update) 
					
  if(length(last_toc_update) == 0){
    
	if(!quiet){
	  message("Dataset with id = '", id, "' does not exist or is not readable")
	}
	
	return(NULL)
  
  }
  
  # get cache directory
  if (is.null(cache_dir)){
    
	  cache_dir <- file.path(tempdir(), "ilostat")
 	  
  } 
	
  if (!file.exists(cache_dir)) dir.create(cache_dir)
	
  if (!file.exists(cache_dir)) stop("The folder ", cache_dir, " does not exist")	

  # cache filename
  cache_file <- file.path(cache_dir, paste0(segment, "-", id, "-", type,"-",time_format, "-", last_toc_update ,paste0(".", cache_format)))
  
  if (cache){
    
	if(cache_update & file.exists(cache_file)){
	  
	  cache_update <- FALSE
	
	  if(!quiet){
	  
	    message("Table ", id, " is up to date")
	  
	  }
  
    }  
  
  }
  
  # if cache = FALSE or update or new: download else read from cache
  if (!cache || cache_update || !file.exists(cache_file) ){
      
	  dat <- get_ilostat_raw(id, segment, cache_file, cache_dir, cache_format, quiet)
	
	  # delete alod cache 
	  delete_old_cache(cache_dir, cache_file, quiet)
  
	  
      if (type %in% 'code') {
        
		dat <- dat 
	  
	  } else if(type %in% 'both') {
		
		dat <- label_ilostat(dat, code = 'all', lang = lang)
	  
	  } else if (type %in% 'label') {
        
		dat <- label_ilostat(dat, lang = lang)
      
	  } else {
        
		stop("Invalid type.")
      
	  }
  
  } else {
	
    cf <- path.expand(cache_file)
 
	if(cache_format == 'rds'){
		
		dat <- read_rds(cache_file) %>% as_tibble %>% mutate_if(is.factor, as.character)
	
	}
	
	if(cache_format == 'csv'){
		
		dat <- read_csv(cache_file, col_types = cols(.default = col_character(), obs_value = col_double()), progress = FALSE)
	
	}
	
	if(cache_format == 'dta'){
		
		dat <- read_dta(cache_file)
		
		colnames(dat) <- colnames(dat) %>% stringr::str_replace(stringr::fixed('label'), stringr::fixed('.label'))
		
	} 
	
	if(cache_format == 'sav'){
		
		dat <- read_sav(cache_file)
	
	} 
	
	if(cache_format == 'sas7bdat'){
		
		dat <- read_sas(cache_file)
		
		colnames(dat) <- colnames(dat) %>% stringr::str_replace(stringr::fixed('label'), stringr::fixed('.label'))
	
	} 
	
	if(!quiet){
	
	  message("Table ", id, " read from cache file: ", cf)
	
	}
  
  }
  


  # process time_format
  if(tolower(time_format) %in% 'num' & str_sub(id,-1,-1) %in% c('Q', 'M')){
    
  dat$time <- paste0(
				str_sub(dat$time,1,4), 
				mapvalues(
						str_sub(dat$time, 5,-1), 
						from = ilostat_date_convert$code, 
						to = ilostat_date_convert[['num']], 
						warn_missing = FALSE)) %>% as.numeric
						
						
  } else if(tolower(time_format) %in% 'num'){
  
    dat$time <- as.numeric(dat$time)
  
  }
  
  if(tolower(str_sub(time_format,1,4)) %in% 'date'){
    
	if(str_sub(id,-1,-1) %in% c('Q', 'M')){
	  
	  dat$time <- paste0(
					str_sub(dat$time,1,4), 
					mapvalues(
							str_sub(dat$time, 5,-1), 
							from = ilostat_date_convert$code, 
							to = ilostat_date_convert[['date']], 
							warn_missing = FALSE)) 
	} else {
	  
	  dat$time = paste0(dat$time, '-01-01')
	
	}

	if(tolower(time_format) %in% 'date_last'){
	
	  if(str_sub(id,-1,-1) %in% c('Q', 'M')){
	
        shift <- c("A" = 367, "Q" = 96, "M" = 32)[str_sub(id,-1,-1)]  
    
	    dat$time <- mapvalues(
							dat$time, 
							from = as.character(as.Date(levels(as.factor(dat$time)))), 
							to = as.character(as.Date(cut(as.Date(levels(as.factor(dat$time))) + shift, "month")) - 1), 
							warn_missing = FALSE)

	  } else {
	  
	    dat$time = stringr::str_replace(dat$time, '-01-01', '-12-31')
	
	  }
	  
	}
	
	dat$time <- as.Date(dat$time)
  
  }						

  
  
  # if save in cache_dir, cache_format
  if (cache && (cache_update || !file.exists(cache_file))){

    # resort colnames

	ref_cols <- ilostat_cols_ref[ilostat_cols_ref %in% colnames(dat)]
	
	order_cols <- c(ref_cols, colnames(dat)[!colnames(dat) %in% ref_cols])
	
	dat <- select_at(dat, .vars = order_cols)
	

	if(cache_format == 'rds'){
		
		saveRDS(dat, file = cache_file)
	
	}
	
	if(cache_format == 'csv'){
		
		fwrite(dat, file = cache_file)
	
	}
	
	if(cache_format == 'dta'){
		
		colnames(dat) <- colnames(dat) %>% stringr::str_replace(stringr::fixed('.label'), stringr::fixed('label'))
		
		write_dta(dat, path = cache_file)
		
		colnames(dat) <- colnames(dat) %>% stringr::str_replace(stringr::fixed('label'), stringr::fixed('.label'))
		
	}
	
	if(cache_format == 'sav'){
		
		write_sav(dat, path = cache_file)
	
	}
	
	if(cache_format == 'sas7bdat'){
		
		colnames(dat) <- colnames(dat) %>% stringr::str_replace(stringr::fixed('.label'), stringr::fixed('label'))
		
		write_sas(dat, path = cache_file)
		
		colnames(dat) <- colnames(dat) %>% stringr::str_replace(stringr::fixed('label'), stringr::fixed('.label'))
	
	}
	
	if(!quiet){
	  message("Table ", id, " cached at ", path.expand(cache_file))
	}
	
  }
  

  if(!back){return(NULL)}

  # process filters
  if(!unique(filters == 'none')){ 
    
	filters <- filters_ilostat(filters, names(dat), fixed)
	  
    if(!is.null(filters)){
        
	  dat <- filter(dat, eval(parse(text = filters)))
	  
    }

  }	
  
  
  if(detail %in% 'dataonly'){
	
	ref_dataonly <- ilostat_cols_ref[1:14]
	  
	ref_dataonly <- ref_dataonly[ref_dataonly %in% names(dat)] 
	  
	dat <- select_at(dat, .vars = ref_dataonly)
	
  }
  if(detail %in% 'serieskeysonly'){
	
	ref_serieskeysonly <- ilostat_cols_ref[1:12]
	  
	ref_serieskeysonly <- ref_serieskeysonly[ref_serieskeysonly %in% names(dat)] 
	  
	dat <- select_at(dat, .vars =  ref_serieskeysonly) %>% distinct()
	
  }
  if(detail %in% 'bestsourceonly'){
	
	ref_bestsourceonly <- ilostat_cols_ref[1:13]
	  
	ref_bestsourceonly <- ref_bestsourceonly[ref_bestsourceonly %in% names(dat)] 
	  
	ref_bestsourceonly <- ref_bestsourceonly[!ref_bestsourceonly %in% 'source']
	  
	rest_bestsourceonly <- names(dat)[!names(dat)%in% ref_bestsourceonly]
	  
	summa <-  eval(parse(text = paste0("list(", paste0(paste0(rest_bestsourceonly, " = quo(first(",rest_bestsourceonly, "))"), collapse = ', '), ")")))
	  
	dat <- group_by_at(dat, .vars = ref_bestsourceonly) %>% summarise(!!!summa, .groups = "drop")
      
  }
  
 
  

  # process cmd  
  if(cmd != 'none'){
	
	try(dat <- eval(parse(text = cmd)), silent = TRUE) 
	

  }	
  
  dat   	  
}



get_ilostat_raw <- function(id, 
                            segment,   
                            cache_file, 
                            cache_dir, 
                            cache_format, 
                            quiet) {

  base <- paste0(ilostat_url(), segment, "/", id, ".rds")	   

  if (str_sub(ilostat_url(), 1, 5) == "https") {
    tfile <- cache_file %>%
      stringr::str_replace(paste0(stringr::fixed('.'), cache_format), ".rds")

    url <- paste0("https://rplumber.ilo.org/files/", segment, "/", id, ".rds")
    message("Trying URL '", url, "'")

    # Build User-Agent with platform + OS
    ua <- build_user_agent()

    # Perform request with User-Agent
    resp <- request(url) |>
      req_headers(`User-Agent` = ua) |>
      req_perform()

    dat <- NULL
    if (resp_status(resp) == 200) {
      writeBin(resp_body_raw(resp), tfile)
      dat <- read_rds(tfile) %>%
        as_tibble() %>%
        mutate(across(where(is.factor), as.character))

      if (!quiet) message("Cache saved at '", tfile, "'")
    }

  } else {
    dat <- tryCatch(
      read_rds(base) %>%
        as_tibble() %>%
        mutate(across(where(is.factor), as.character)),
      error = function(e) NULL
    )
  }

  # check validity
  if (!is_tibble(dat)) {
    stop("Dataset with id = '", id, "' does not exist or is not readable")
  }

  dat
}

delete_old_cache <- function(cache_dir, new_file_path, quiet) {
  
  # Step 1: Extract the segment-id from the new file's name.
  filename <- basename(new_file_path)
  segment_id_pattern <- sub("([[:alnum:]]+-[[:alnum:]]+)-.*", "\\1", filename)
  
  # Step 2: List all files in the cache directory that match the segment-id pattern.
  matching_files <- list.files(
    path = cache_dir,
    pattern = paste0("^", segment_id_pattern, "-"),
    full.names = TRUE
  )
  
  # Step 3: Exclude the new file from the list of files to be deleted.
  files_to_delete <- setdiff(matching_files, new_file_path)

  # Step 4: Delete the matched files.
  if (length(files_to_delete) > 0) {
    if(! quiet) message("Deleting the following old cache files:\n", paste(files_to_delete, collapse = "\n"))
    file.remove(files_to_delete)
  } else {
    if(! quiet) message("No old cache files found to delete for this segment-id.")
  }

  invisible(files_to_delete)
}