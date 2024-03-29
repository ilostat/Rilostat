#' @title Switch ilostat to distribution 
#' @description Get distribution for ilostat number of persons only.
#' @param x  dataset to transform into distribution. 
#' @param var  String variable name use for the distribution default \code{"no"}, 
#' 		could be \code{"sex"}, \code{"classif1"}, \code{"classif2"}.
#' @param .keep if true return only new column call distribution default \code{FALSE}. 
#' @details this function use the max of the corresponding grouping so it is 
#' 		important to not filter any subset of the corresponding variable selected for the distribution
#'		at this level, ie. if you remove SEX_T, the distribution by sex will only have SEX_F or SEX_M / max(SEX_M, SEX_F) * 100,
#'		which is no longer a distribution.
#'
#'		In addition, distribution is only applicable for indicators with Number of persons (usually in thousands), 
#'		So plse do not distribute ratios, earnings, hours of works, CPI, GDP etc ... no warning will prevent for that
#'		if doubts use distribution from get_ilostat() instead of, warnings will help you.
#'
#'
#' @author David Bescond \email{bescond@ilo.org}
#' @return a data_frame. obs_status will no longer be a number of persons but a percentage.
#' @references
#' See citation("Rilostat")
#' ilostat bulk download facility user guidelines 
#' \url{https://webapps.ilo.org/ilostat-files/Documents/ILOSTAT_BulkDownload_Guidelines.pdf}
#' @examples
#' \dontrun{
#'  dat <- get_ilostat("EMP_TEMP_SEX_STE_GEO_NB_A", cache = FALSE)
#'  dat_dist <- distribution_ilostat(dat, "classif1")
#'  dat_plus_dist <- mutate(dat, dist = distribution_ilostat(dat,"classif1", .keep=TRUE))
#'  head(dat_dist)
#'  clean_ilostat_cache() 
#' }
#' @export

distribution_ilostat <- function(x, var, .keep = FALSE){

		if(length(var) > 1){warning("var should on of character 'sex', 'classif1', 'classif2', no distribution return !!! ") ; return(x)}
		if(!tolower(var) %in% c("sex", "classif1", "classif2")) {warning("var should on of character 'sex', 'classif1', 'classif2', no distribution return !!! ") ; return(x)}

			 
						message(paste0("Processing of ",str_sub(unique(x[["indicator"]])),"... distribution by ",var,", could take few time !"))
						
						eval(parse(text = "ref_col <- ilostat_cols_ref[ilostat_cols_ref %in% colnames(x %>% select(ref_area:time))]"))
						
						ref_col <- c(ref_col[!ref_col %in% var])
								
						run <- paste0(	"group_by(x,",paste0(ref_col, collapse = ', '),") %>% ",
										"mutate(obs_value = ifelse(!obs_value %in% c(NA, 0),obs_value / max(obs_value, na.rm = TRUE) * 100, obs_value)) %>% ",
										"mutate(test = sum(obs_value, na.rm = TRUE) / n()) %>% ",
										"ungroup", 
										ifelse(.keep, "%>% .$obs_value", "")
										)
							
					eval(parse(text = run))	}