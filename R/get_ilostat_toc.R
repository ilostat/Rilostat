#' @title Read ilostat Table of Contents / workflow
#' @description Download one table of contents from ilostat \url{https://ilostat.ilo.org} via bulk download facility 
#' \url{https://ilostat.ilo.org/data/bulk/}.
#' @param segment A character, way to get datasets by: \code{"indicator"} (default) or \code{"ref_area"}, 
#'        Can be set also with options(ilostat_segment = "ref_area"),
#' @param lang a character, code for language. Available are \code{"en"} (default), 
#'        \code{"fr"} and \code{"es"}. Can be set also with options(ilostat_lang = "fr"),
#' @param search a character vector, "none" (default), datasets with this pattern in
#'  	  the description will be returned,
#' 	      characters vector will be use as AND, Character with "|" as OR, see example,
#'        options(ilostat_time_format = "date"),  
#' @param filters a list; \code{"none"} (default) to get a whole toc or a named list of
#'			filters to get just part of the table. Names of list objects are
#'			ilostat toc variable codes and values are vectors of observation codes.
#'			filters detect on variables.
#' @param fixed a logical, if \code{TRUE} (default), pattern is a string to be matched as is,
#'        Change to \code{FALSE} if more complex regex matching is needed.
#' @param cache_update a logical whether to update cache. Check cache update with last.update attribute store on the cache file name 
#' 			and the one from the table of contents. Can be set also with
#'        	options(ilostat_cache_update = FALSE). Default is \code{TRUE},  
#' @param cache_dir a path to a cache directory. The directory has to exist.
#'        	The \code{NULL} (default) uses and creates
#'        	'ilostat' directory in the temporary directory from
#'        	\code{\link{tempdir}}. The directory can also be set with
#'        	\code{option} ilostat_cache_dir,
#' @param quiet a logical, if \code{FALSE} , return message from processing, \code{TRUE} (default).
#' 			Can be set also with options(ilostat_quiet = TRUE), 
#' @return A tibble with ten columns depending of the segment: indicator or ref_area
#' 		\itemize{
#'      \item{\code{id}} : The codename of dataset of theme, will be used by the get_ilostat and get_ilostat_raw functions,
#'      \item{\code{indicator or ref_area}} : The indicator or ref_area code of dataset,
#'      \item{\code{indicator.label or ref_area.label}} : The indicator or ref_area name of dataset,
#'      \item{\code{freq}}  : The frequency code of dataset,
#'      \item{\code{freq.label}} : Is freq name of dataset,
#'      \item{\code{size}} : Size of the csv.gz files,
#'      \item{\code{data.start}} : First time period of the dataset, 
#'      \item{\code{data.end}} : Last time period of the dataset,
#'      \item{\code{last.update}} : Last update of the dataset,
#'      \item{\code{...}} : Others relevant information
#'      }
#' @seealso \code{\link{get_ilostat}}.
#' @details The toc in English by indicator is downloaded from ilostat API \url{https://rplumber.ilo.org/__docs__/#get-/metadata/toc/indicator/}. 
#' The values in column "id" should be used to download a selected dataset.
#' @details The toc in English by ref_area is downloaded from ilostat API \url{https://rplumber.ilo.org/__docs__/#get-/metadata/toc/ref_area/}. 
#' The values in column "id" should be used to download a selected dataset.
#' @references
#' See citation("Rilostat")
#' ilostat bulk download facility user guidelines \url{https://ilostat.ilo.org/data/bulk/} 
#' @author David Bescond \email{bescond@ilo.org}
#' @keywords utilities database
#' @examples
#' \dontrun{
#' ## default segment by indicator, default lang English
#'  toc <- get_ilostat_toc()
#'  head(toc)
#'  toc <- get_ilostat_toc(segment = "ref_area", lang = "fr")
#'  head(toc)
#' ##
#' ## search on toc
#'  toc <- get_ilostat_toc(search = "education")
#'  head(toc)
#'  toc <- get_ilostat_toc(lang = "fr", search = "age")
#'  head(toc)
#'  toc <- get_ilostat_toc(segment = "ref_area", lang = "fr", search = "Albanie")
#'  toc
#'  toc <- get_ilostat_toc(segment = "ref_area", lang = "es", search = "Trimestral")
#'  head(toc)
#' ##
#' ## search multi on toc
#'  toc <- get_ilostat_toc(segment = "ref_area", lang = "fr", 
#'              search = "Albanie|France", fixed = FALSE)
#'  head(toc)
#'  toc <- get_ilostat_toc(search = "youth|adult", fixed = FALSE)
#'  head(toc)
#' ##
#' }
#' @export

get_ilostat_toc <- function(
  segment = getOption('ilostat_segment', 'indicator'),
  lang = getOption('ilostat_lang', 'en'),
  search = getOption('ilostat_search', 'none'),
  filters = getOption('ilostat_filter', 'none'),
  fixed = getOption('ilostat_fixed', TRUE),
  cache_dir = getOption('ilostat_cache_dir', NULL),
  cache_update = getOption('ilostat_cache_update', TRUE),
  quiet = getOption('ilostat_quiet', TRUE)
) {

  segment <- match.arg(segment, c("indicator", "ref_area"))
  lang <- match.arg(lang, c("en", "fr", "es"))

  # get cache directory
  if (is.null(cache_dir)){
    
	  cache_dir <- file.path(tempdir(), "ilostat")
 	  
  } 
	
	dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # ---- cache file
  
  cache_file <- file.path(
    cache_dir,
    paste0("toc_", segment, "_", lang, ".rds")
  )

  # ---- download if needed
  if (!file.exists(cache_file) || isTRUE(cache_update)) {

    url <- paste0(ilostat_url(), segment, "/table_of_contents_", lang, ".rds")

    if (!quiet) message("Downloading toc from '", url, "'")

    ua <- build_user_agent()

    resp <- request(url) |>
      req_headers(`User-Agent` = ua) |>
      req_timeout(30) |>
      req_perform()

    if (resp_status(resp) != 200) {
      stop("TOC not available for segment '", segment, "' and lang '", lang, "'")
    }

    tmpfile <- tempfile(fileext = ".rds")
    on.exit(unlink(tmpfile), add = TRUE)

    writeBin(resp_body_raw(resp), tmpfile)

    y <- read_rds(tmpfile)

    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    saveRDS(y, cache_file)

  } else {

    if (!quiet) message("Using cached toc: ", cache_file)
    y <- read_rds(cache_file)
  }

  # ---- safety check
	if (!tibble::is_tibble(y)) {
	  stop("Invalid TOC file: ", cache_file)
	}

# ---- search
if (!identical(search, "none")) {

  y <- y %>%
    mutate(
      titles = do.call(paste0, across(c(2, 3, 4, 5, 11, 12, 13, 14)))
    )

  titles_vec <- y[["titles"]]

  if (fixed) {
    keep <- Reduce(`&`, lapply(search, function(s) {
      stringr::str_detect(titles_vec, stringr::fixed(s))
    }))
  } else {
    keep <- Reduce(`&`, lapply(search, function(s) {
      stringr::str_detect(titles_vec, s)
    }))
  }

  y <- y[keep, ]
  y$titles <- NULL
}

  # ---- filters (kept as-is)
  if (!identical(filters, "none")) {

    filters <- filters_ilostat(filters, names(y), fixed)

    if (!is.null(filters)) {
      y <- filter(y, eval(parse(text = filters)))
    }
  }

  y
}