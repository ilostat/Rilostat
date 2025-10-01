#' @title Read ilostat Dictionary
#' @description Downloads one ilostat dictionary from ilostat \url{https://ilostat.ilo.org} via bulk download facility 
#' \url{https://ilostat.ilo.org/data/bulk/}.
#' @details For a given coded variable from ilostat \url{https://ilostat.ilo.org/}.
#'    The dictionaries link codes with human-readable labels.
#'    To translate codes to labels, use \code{\link{label_ilostat}}.
#' @param dic A character, dictionary for the variable to be downloaded,
#' @param lang a character, code for language. Available are \code{"en"} (default), 
#'        \code{"fr"} and \code{"es"}. Can be set also with options(ilostat_lang = 'fr'),
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
							quiet = getOption('ilostat_quiet', FALSE)) {

  dictlang <- paste0(tolower(dic), "_", tolower(lang))

  if (!exists(dictlang, envir = .ilostatEnv)) {

    # Build URL for API dictionary
    url <- paste0(ilostat_url(), "dic/", tolower(dic), "_", tolower(lang), ".rds")
    if(!quiet) message("Trying dictionary URL '", url, "'")

    # Build User-Agent
    ua <- build_user_agent()

    # Perform request
    resp <- request(url) |>
      req_headers(`User-Agent` = ua) |>
      req_perform()

    if (resp_status(resp) != 200) {
      stop("The dictionary '", dic, "_", lang, ".rds' does not exist or is not accessible")
    }

    # Save temp file and read it
    tmpfile <- tempfile(fileext = ".rds")
    writeBin(resp_body_raw(resp), tmpfile)

    get_dic <- read_rds(tmpfile) %>%
      select(-contains("description"))

    # Store in environment cache
    assign(dictlang, get_dic, envir = .ilostatEnv)

    if (!is_tibble(get_dic)) {
      stop("The dictionary '", dic, "_", lang, ".rds' is not a valid tibble")
    }
  }

  get(dictlang, envir = .ilostatEnv)
}
