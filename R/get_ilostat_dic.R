#' @title Read ilostat Dictionary
#' @description Downloads one ilostat dictionary from ilostat \url{https://ilostat.ilo.org} via bulk download facility 
#' \url{https://ilostat.ilo.org/data/bulk/}.
#' @details For a given coded variable from ilostat \url{https://ilostat.ilo.org/}.
#'    The dictionaries link codes with human-readable labels.
#'    To translate codes to labels, use \code{\link{label_ilostat}}.
#' @param dic A character, dictionary for the variable to be downloaded,
#' @param lang a character, code for language. Available are \code{"en"} (default), 
#'        \code{"fr"} and \code{"es"}. Can be set also with options(ilostat_lang = 'fr'),
#' @param cache_dir a path to a cache directory. The directory has to exist.
#'        	The \code{NULL} (default) uses and creates
#'        	'ilostat' directory in the temporary directory from
#'        	\code{\link{tempdir}}. The directory can also be set with
#'        	\code{option} ilostat_cache_dir,
#' @param quiet a logical, if \code{FALSE} , return message from processing, \code{TRUE} (default).
#' 			Can be set also with options(ilostat_quiet = TRUE), 
#' @return tibble with two columns: code names and full names.
#' @seealso \code{\link{label_ilostat}}, \code{\link{get_ilostat}}.
#' @keywords utilities database
#' @author David Bescond \email{bescond@ilo.org}
#' @references
#' See citation("Rilostat")
#' ilostat bulk download facility user guidelines 
#' \url{https://ilostat.ilo.org/data/bulk/}
#' @examples
#' \dontrun{
#'  tmp <- get_ilostat_dic("indicator")
#'  head(tmp)
#'  tmp <- get_ilostat_dic("classif1", lang = "fr")
#'  head(tmp)
#' }
#' @export
get_ilostat_dic <- function(dic,
                           lang = getOption("ilostat_lang", "en"),
                           cache_dir = getOption("ilostat_cache_dir", NULL),
                           quiet = getOption("ilostat_quiet", TRUE)) {

  dic  <- tolower(dic)
  lang <- tolower(lang)

  dictlang <- paste0(dic, "_", lang)

  # get cache directory
  if (is.null(cache_dir)){
    
	  cache_dir <- file.path(tempdir(), "ilostat")
 	  
  } 
	
	dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # ---- cache file
  
  cache_file <- file.path(cache_dir, paste0(dictlang, ".rds"))



  # ---- READ FROM CACHE ----
  if (file.exists(cache_file)) {

    if (!quiet) { message("Reading dictionary from cache: ", cache_file)}

    get_dic <- readr::read_rds(cache_file)

  } else {

    # ---- DOWNLOAD ----
    url <- paste0(ilostat_url(), "dic/", dictlang, ".rds")

    if (!quiet) {message("Trying dictionary URL '", url, "'")}

    ua <- build_user_agent()

    resp <- request(url) |>
      req_headers(`User-Agent` = ua) |>
      req_timeout(30) |>
      req_perform()

    if (resp_status(resp) != 200) {
      stop("DIC not available '", dictlang, "'")
    }

    tmpfile <- tempfile(fileext = ".rds")
    on.exit(unlink(tmpfile), add = TRUE)

    writeBin(resp_body_raw(resp), tmpfile)

    get_dic <- read_rds(tmpfile) %>%
      dplyr::select(-tidyselect::contains("description"))

    if (!tibble::is_tibble(get_dic)) {
      stop("The dictionary '", dictlang, ".rds' is not a valid tibble")
    }

    # ---- SAVE TO CACHE ----
    readr::write_rds(get_dic, cache_file)

    if (!quiet) { message("Dictionary cached at: ", cache_file) }
  }

  get_dic
}