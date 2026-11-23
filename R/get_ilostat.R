#' @title Read ilostat Data
#' @description Download datasets from ilostat \url{https://ilostat.ilo.org} via bulk download facility 
#' \url{https://ilostat.ilo.org/data/bulk/}.
#' @param id A code name for the dataset of interest.
#'        See \code{\link{get_ilostat_toc}} or details for how to get code.
#' @param segment A character, way to get datasets by: \code{"indicator"} (default) or \code{"ref_area"}.
#'        Can be set also with options(ilostat_segment = "ref_area"),   
#' @param type a character, type of variables, \code{"code"} (default), \code{"label"} or \code{"both"}.
#'        Can be set also with options(ilostat_type = "both"),  
#' @param lang a character, code for language. Available are \code{"en"} (default), 
#'        \code{"fr"} and \code{"es"}. Can be set also with options(ilostat_lang = "fr"),
#' @param time_format a string giving a type of the conversion of the time
#'        column from the ilostat format. "raw" (default)
#'        does not do conversion and return time as character (ie. "2017", "2017Q1", "2017M01"). A "date" converted to
#'        a \code{\link{Date}} with a first date of the period. A "date_last" converted to a \code{\link{Date}} with
#'        a last date of the period and "num" converted to a numeric. Can be set also with 
#'        options(ilostat_time_format = "date"),  
#' @param filters a list; \code{"none"} (default) to get a whole dataset or a named list of
#'			filters to get just part of the table. Names of list objects are
#'			ilostat variable codes and values are vectors of observation codes.
#'			filters detect on variables, so could be partial, ie. \code{list(sex = "T")} is
#'			enough but equivalent to \code{list(sex = "SEX_T")}. Additional options:
#' 			\itemize{ 
#' 				\item{\code{timefrom}} : starting year of the return dataset,
#' 				\item{\code{timeto}} : ending year of the return dataset.
#'    		}
#' @param best_source a character; \code{"yes"} (default) to get secondary source if exist \code{"all"}, \code{"no"} for secondary only.
#' @param fixed a logical, if \code{TRUE} (default), filters arguments pattern is a string to be matched as is,
#'        Change to \code{FALSE} if more complex regex matching is needed.
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
#' 			Can be set also with options(ilostat_cache_format = "dta"),  
#' @param back a logical; if TRUE (default) return a data frame, otherwise only write to cache,
#' @param cmd a character, R expression use for manipulate internal data frame \code{dat} object applied to each datasets retrieved 
#         after filters and type setting. Manipulation should return data.frame \code{"none"} (default),
#'        If use, \code{cache} is set to FALSE. see examples. 
#'        This argument uses non-standard evaluation and should be used with care.
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
#'  dat <- get_ilostat("UNE_TUNE_SEX_AGE_NB_A")
#'  head(dat)
#'  dat <- get_ilostat("NZL_Q", segment = "ref_area")
#'  head(dat)
#'
#'  dir.create(file.path(tempdir(), "r_cache"))
#'  dat <- get_ilostat("UNE_TUNE_SEX_AGE_NB_A", 
#'                   cache_dir = file.path(tempdir(), "r_cache"))
#'  head(dat)
#'
#'  clean_ilostat_cache(cache_dir = file.path(tempdir(), "r_cache")) 
#'
#'  options(ilostat_update = TRUE)
#'  dat <- get_ilostat("UNE_TUNE_SEX_AGE_NB_A")
#'  head(dat)
#'  options(ilostat_update = FALSE)
#'  options(ilostat_cache_dir = file.path(tempdir(), "r_cache"))
#'  dat <- get_ilostat("UNE_TUNE_SEX_AGE_NB_A")
#'
#'  clean_ilostat_cache() 
#'
#' ############# get multiple datasets
#'  dat <- get_ilostat(c("CPI_ACPI_COI_RT_M", "CPI_ACPI_COI_RT_Q"), cache = FALSE)
#'  head(dat)
#'  toc <- get_ilostat_toc(search = "CPI_")
#'  head(toc)
#'  dat <- get_ilostat(toc, cache = FALSE) #id as a tibble
#'
#' ############# get datasets with filters
#'  dat <- get_ilostat(id = c("UNE_TUNE_SEX_AGE_NB_A", "EMP_TEMP_SEX_AGE_NB_A"), 
#'          filters = list(	ref_area = "FRA", 
#'          classif1 = "_YGE15", 
#'          time = "2016",
#'          sex = c("T", "SEX_F")), quiet = TRUE)
#'  head(dat)
#'  clean_ilostat_cache() 
#'
#' ############# store in other format
#'  dir.create(file.path(tempdir(), "ilostat"))
#'
#'  dat <- get_ilostat("UNE_TUNE_SEX_AGE_NB_A", 
#'                   cache_dir = file.path(tempdir(), "r_cache"), cache_format = "csv")
#'  dat <- get_ilostat("UNE_TUNE_SEX_AGE_NB_A", 
#'                   cache_dir = file.path(tempdir(), "r_cache"), cache_format = "dta")
#'
#' ############# advanced manipulation
#'
#'  dat <- get_ilostat("UNE_TUNE_SEX_AGE_NB_A", cmd = "dat %>% count(ref_area)", quiet = TRUE)
#'  label_ilostat(dat, code = "ref_area")
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
						best_source = getOption('ilostat_best_source', 'yes'),
						fixed = getOption('ilostat_fixed', TRUE),
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
 

# normalize best_source into a vector
best_source_vec <- switch(
  tolower(as.character(best_source)),
  "yes" = c(1),
  "no" = c(0),
  "all" = c(1, 0),
  stop("Invalid best_source value. Use 'yes', 'no', or 'all'")
)



  dat <- NULL

 for (i in seq_along(ref_id)) {
	    
    dat <- 	bind_rows(
				
				dat, 
				
				get_ilostat_dat(id = ref_id[i], segment, type, lang, time_format, filters, best_source_vec, fixed, cache, cache_update,	cache_dir, cache_format, back, cmd, quiet 
								
				)
			)
  }

  ref_cols <- ilostat_cols_ref[ilostat_cols_ref %in% colnames(dat)]
	
  order_cols <- c(ref_cols, colnames(dat)[!colnames(dat) %in% ref_cols])
	
  dat %>% select(all_of(order_cols))
 
}



get_ilostat_dat <- function(id,
                            segment,
                            type,
                            lang,
                            time_format,
                            filters,
                            best_source_vec,
                            fixed,
                            cache,
                            cache_update,
                            cache_dir,
                            cache_format,
                            back,
                            cmd,
                            quiet) {

  backend <- tolower(
    Sys.getenv("ILOSTAT_BACKEND", unset = "remote")
  )

  if (!backend %in% c("remote", "local")) {
    stop("Invalid ILOSTAT_BACKEND.", call. = FALSE)
  }


# TRUE when observations are loaded from an existing
# processed remote cache.
from_processed_cache <- FALSE

  # ------------------------------------------------------------
  # 1. LOCAL BACKEND
  # No TOC, HTTP, cache creation or cache deletion.
  # ------------------------------------------------------------

  if (backend == "local") {

    dat <- get_ilostat_raw(
      id = id,
      segment = segment,
      cache_file = NULL,
      cache_dir = NULL,
      cache_format = "rds",
      quiet = quiet,
      best_source_vec = best_source_vec
    )

    # Local data is already filtered by best_source.
    # The raw reader also converts time to character.

    if (type == "both") {
      dat <- label_ilostat(dat, code = "all", lang = lang)
    } else if (type == "label") {
      dat <- label_ilostat(dat, lang = lang)
    } else if (type != "code") {
      stop("Invalid type.")
    }

  } else {

    # ----------------------------------------------------------
    # 2. REMOTE BACKEND
    # Preserve existing TOC and cache behavior.
    # ----------------------------------------------------------

    ref_id <- id

    last_toc_update <- get_ilostat_toc(
      segment,
      lang
    ) %>%
      dplyr::filter(.data$id %in% ref_id)

    last_toc_update <- unique(
      last_toc_update$last.update
    )

    last_toc_update <- ifelse(
      substr(last_toc_update, 6, 8) %in% "/20",
      format(
        strptime(
          last_toc_update,
          "%d/%m/%Y  %H:%M"
        ),
        "%Y%m%dT%H%M"
      ),
      last_toc_update
    )

    if (length(last_toc_update) == 0) {

      if (!quiet) {
        message(
          "Dataset with id = '",
          id,
          "' does not exist or is not readable"
        )
      }

      return(NULL)
    }

    if (is.null(cache_dir)) {
      cache_dir <- file.path(tempdir(), "ilostat")
    }

    dir.create(
      cache_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    cache_file <- file.path(
      cache_dir,
      paste0(
        segment, "-",
        id, "-",
        type, "-",
        time_format, "-",
        last_toc_update, "-",
        paste0(best_source_vec, collapse = ""),
        ".", cache_format
      )
    )

    if (cache) {

      if (cache_update && file.exists(cache_file)) {

        cache_update <- FALSE

        if (!quiet) {
          message("Table ", id, " is up to date")
        }
      }
    }

    # ----------------------------------------------------------
    # Remote cache miss or forced retrieval
    # ----------------------------------------------------------

    if (!cache || cache_update || !file.exists(cache_file)) {

      dat <- get_ilostat_raw(
        id = id,
        segment = segment,
        cache_file = cache_file,
        cache_dir = cache_dir,
        cache_format = cache_format,
        quiet = quiet
      ) %>%
        dplyr::filter(
          .data$best_source %in% best_source_vec
        )

      delete_old_cache(
        cache_dir,
        cache_file,
        quiet
      )

      if (type == "both") {

        dat <- label_ilostat(
          dat,
          code = "all",
          lang = lang
        )

      } else if (type == "label") {

        dat <- label_ilostat(
          dat,
          lang = lang
        )

      } else if (type != "code") {

        stop("Invalid type.")
      }

    } else {

      # --------------------------------------------------------
      # Remote cache hit
      # --------------------------------------------------------

	from_processed_cache <- TRUE

      cf <- path.expand(cache_file)

      if (cache_format == "rds") {

        dat <- readr::read_rds(cache_file) %>%
          tibble::as_tibble() %>%
          dplyr::mutate(
            dplyr::across(
              where(is.factor),
              as.character
            )
          )

      } else if (cache_format == "csv") {

        dat <- readr::read_csv(
          cache_file,
          col_types = readr::cols(
            .default = readr::col_character(),
            obs_value = readr::col_double()
          ),
          progress = FALSE
        )

      } else if (cache_format == "dta") {

        dat <- haven::read_dta(cache_file)

        names(dat) <- stringr::str_replace(
          names(dat),
          stringr::fixed("label"),
          stringr::fixed(".label")
        )

      } else if (cache_format == "sav") {

        dat <- haven::read_sav(cache_file)

      } else if (cache_format == "sas7bdat") {

        dat <- haven::read_sas(cache_file)

        names(dat) <- stringr::str_replace(
          names(dat),
          stringr::fixed("label"),
          stringr::fixed(".label")
        )

      } else {

        stop("Unsupported cache format: ", cache_format)
      }

      if (!quiet) {
        message("Table ", id, " read from cache file: ", cf)
      }
    }
  }

  # ------------------------------------------------------------
  # 3. COMMON TIME CONVERSION
  # ------------------------------------------------------------

  # Remote cached data already contains the requested time format.
  # Preserve the existing behavior of the remote pipeline.
  #
  # Note: the original implementation also applies this section
  # after a cache hit. We retain that behavior for compatibility.

if (!from_processed_cache) {


  if (tolower(time_format) %in% "num" &&
      stringr::str_sub(id, -1, -1) %in% c("Q", "M")) {

    lookup <- setNames(
      ilostat_date_convert[["num"]],
      ilostat_date_convert$code
    )

    dat$time <- paste0(
      stringr::str_sub(dat$time, 1, 4),
      lookup[stringr::str_sub(dat$time, 5, -1)]
    ) |>
      as.numeric()

  } else if (tolower(time_format) %in% "num") {

    dat$time <- as.numeric(dat$time)
  }

  if (tolower(stringr::str_sub(time_format, 1, 4)) %in% "date") {

    if (stringr::str_sub(id, -1, -1) %in% c("Q", "M")) {

      lookup_date <- setNames(
        ilostat_date_convert[["date"]],
        ilostat_date_convert$code
      )

      dat$time <- paste0(
        stringr::str_sub(dat$time, 1, 4),
        lookup_date[stringr::str_sub(dat$time, 5, -1)]
      )

    } else {

      dat$time <- paste0(dat$time, "-01-01")
    }

    if (tolower(time_format) == "date_last") {

      if (stringr::str_sub(id, -1, -1) %in% c("Q", "M")) {

        shift <- c(
          A = 367,
          Q = 96,
          M = 32
        )[stringr::str_sub(id, -1, -1)]

        u <- unique(dat$time)

        lookup_last <- setNames(
          as.Date(
            cut(as.Date(u) + shift, "month")
          ) - 1,
          u
        )

        dat$time <- lookup_last[dat$time]

      } else {

        dat$time <- stringr::str_replace(
          dat$time,
          "-01-01",
          "-12-31"
        )
      }
    }
  }

}

  # ------------------------------------------------------------
  # 4. REMOTE CACHE WRITING ONLY
  # ------------------------------------------------------------

  if (backend == "remote" &&
      cache &&
      (cache_update || !file.exists(cache_file))) {

    ref_cols <- ilostat_cols_ref[
      ilostat_cols_ref %in% names(dat)
    ]

    order_cols <- c(
      ref_cols,
      names(dat)[!names(dat) %in% ref_cols]
    )

    dat <- dat %>%
      dplyr::select(dplyr::all_of(order_cols))

    if (cache_format == "rds") {

      saveRDS(dat, file = cache_file)

    } else if (cache_format == "csv") {

      data.table::fwrite(dat, file = cache_file)

    } else if (cache_format == "dta") {

      names(dat) <- stringr::str_replace(
        names(dat),
        stringr::fixed(".label"),
        stringr::fixed("label")
      )

      haven::write_dta(dat, path = cache_file)

      names(dat) <- stringr::str_replace(
        names(dat),
        stringr::fixed("label"),
        stringr::fixed(".label")
      )

    } else if (cache_format == "sav") {

      haven::write_sav(dat, path = cache_file)

    } else if (cache_format == "sas7bdat") {

      names(dat) <- stringr::str_replace(
        names(dat),
        stringr::fixed(".label"),
        stringr::fixed("label")
      )

      haven::write_sas(dat, path = cache_file)

      names(dat) <- stringr::str_replace(
        names(dat),
        stringr::fixed("label"),
        stringr::fixed(".label")
      )
    }

    if (!quiet) {
      message(
        "Table ",
        id,
        " cached at ",
        path.expand(cache_file)
      )
    }
  }

  # ------------------------------------------------------------
  # 5. COMMON POST-PROCESSING
  # ------------------------------------------------------------

  if (!back) {
    return(NULL)
  }

  # Apply user filters after data retrieval and time conversion.
  if (!identical(filters, "none")) {

    filters_expr <- filters_ilostat(
      filters,
      names(dat),
      fixed
    )

    if (!is.null(filters_expr)) {

      dat <- dat %>%
        dplyr::filter(
          !!rlang::parse_expr(filters_expr)
        )
    }
  }

  # Preserve legacy cmd behavior.
  if (!identical(cmd, "none")) {

    try(
      dat <- rlang::eval_tidy(
        rlang::parse_expr(cmd),
        data = list(dat = dat)
      ),
      silent = TRUE
    )
  }

  # Hide best_source for the default selection.
  if (length(best_source_vec) == 1L &&
      best_source_vec == 1) {

    dat %>%
      dplyr::select(
        -tidyselect::contains("best_source")
      )

  } else {

    dat
  }
}

get_ilostat_raw <- function(id,
                            segment,
                            cache_file,
                            cache_dir,
                            cache_format,
                            quiet,
                            best_source_vec = NULL) {

  backend <- tolower(
    Sys.getenv("ILOSTAT_BACKEND", unset = "remote")
  )

  base <- paste0(
    ilostat_url(),
    segment,
    "/",
    id,
    ".rds"
  )

  # ------------------------------------------------------------
  # REMOTE BACKEND
  # Preserve existing HTTP retrieval and caching behavior.
  # ------------------------------------------------------------

  if (backend == "remote") {

    tfile <- cache_file %>%
      stringr::str_replace(
        paste0(stringr::fixed("."), cache_format),
        ".rds"
      )

    url <- base

    if (!quiet) {
      message("Trying URL '", url, "'")
    }

    ua <- build_user_agent()

    resp <- httr2::request(url) |>
      httr2::req_headers(`User-Agent` = ua) |>
      httr2::req_timeout(40) |>
      httr2::req_perform()

    dat <- NULL

    if (httr2::resp_status(resp) == 200) {

      writeBin(
        httr2::resp_body_raw(resp),
        tfile
      )

      dat <- readr::read_rds(tfile) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
          dplyr::across(
            where(is.factor),
            as.character
          )
        )

      if (!quiet) {
        message("Cache saved at '", tfile, "'")
      }
    }

    return(dat)
  }

  # ------------------------------------------------------------
  # LOCAL BACKEND
  # Direct RDS access with early best_source filtering.
  # ------------------------------------------------------------

  if (backend == "local") {

    if (!file.exists(base)) {
      stop(
        "Dataset with id = '", id,
        "' does not exist or is not readable",
        call. = FALSE
      )
    }

    dat <- readr::read_rds(base)

    # Filter before converting the full dataset.
    if (!is.null(best_source_vec)) {

		if (!"best_source" %in% names(dat)) {
		  stop(
			"Dataset '", id, "' is missing the best_source column.",
			call. = FALSE
		  )
		}

		if (data.table::is.data.table(dat)) {

		  keep <- dat[["best_source"]] %in% best_source_vec

		  dat <- dat[keep]

		} else {

		  dat <- dplyr::filter(
			dat,
			.data$best_source %in% best_source_vec
		  )
		}
    }

    # Preserve the original time conversion.
    if ("time" %in% names(dat)) {
      dat$time <- as.character(dat$time)
    }

    # Convert only the selected observations.
    dat <- dat %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        dplyr::across(
          where(is.factor),
          as.character
        )
      )

    return(dat)
  }

  stop(
    "Invalid ILOSTAT_BACKEND: ", backend,
    call. = FALSE
  )
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