#' @title Switch ilostat to distribution 
#' @description Get distribution for ilostat number of persons only.
#' @param x  dataset to transform into distribution. 
#' @param var  String variable name use for the distribution default \code{"no"}, 
#' 		could be \code{"sex"}, \code{"classif1"}, \code{"classif2"}.
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
#'  head(dat_dist)
#'  clean_ilostat_cache() 
#' }
#' @export

distribution_ilostat <- function(x, var){

		

	
		switch(tolower(var), 
		
		"sex"= { 		
						 
						message(paste0("Processing of ",str_sub(unique(x[["indicator"]])),"... distribution by sex, could take few time !"))
						
						eval(parse(text = "ref_col <- ilostat_cols_ref[ilostat_cols_ref %in% colnames(x %>% select(ref_area:time))]"))
						
						ref_col <- c(ref_col[!ref_col %in% "sex"], "sex_version")
								
						run <- paste0(	"separate (x, sex,c('sex_version'), sep='_', extra = 'drop', remove = FALSE) %>% ", 
										"mutate(sex_version = gsub('NA', NA,sex_version,  fixed = TRUE) %>% as.factor) %>% ", 
										"group_by(",paste0(ref_col, collapse = ', '),") %>% ",
										"mutate(obs_value = ifelse(!obs_value %in% c(NA, 0),obs_value / max(obs_value, na.rm = TRUE) * 100, obs_value)) %>% ",
										"mutate(test = sum(obs_value, na.rm = TRUE) / n()) %>% ",
										"ungroup %>% ",
										"filter(!test %in% 100) %>% ",
										"select(-sex_version, -test)")
							
					eval(parse(text = run))				
					
				},
		
		
		'classif1' = { 
						message(paste0("Processing of ",str_sub(unique(x[["indicator"]])),"... distribution by classif1, could take few time !"))
						
						eval(parse(text = "ref_col <- ilostat_cols_ref[ilostat_cols_ref %in% colnames(x %>% select(ref_area:time))]"))
						
						ref_col <- c(ref_col[!ref_col %in% "classif1"], "classif1_version")
								
						run <- paste0(	"separate(x, classif1,c('CODE_CLACL1','CODE_VSCL1'), sep='_', extra = 'drop', remove = FALSE) %>% ",
										"unite(	classif1_version,CODE_CLACL1,CODE_VSCL1, sep = '_', remove = TRUE) %>% ",
										"mutate(classif1_version = gsub('NA_NA', NA,classif1_version, fixed = TRUE)) %>% ",
										"group_by(",paste0(ref_col, collapse = ', '),") %>% ",
										"mutate(obs_value = ifelse(!obs_value %in% c(NA, 0),obs_value / max(obs_value, na.rm = TRUE) * 100, obs_value)) %>% ",
										"mutate(test = sum(obs_value, na.rm = TRUE) / n()) %>% ",
										"ungroup %>% ",
										"filter(!test %in% 100) %>% ",
										"select(-classif1_version, -test)")
							
					eval(parse(text = run))			
				},
		
		'classif2' = {
						message(paste0("Processing of ",str_sub(unique(x[["indicator"]])),"... distribution by classif2, could take few time !"))
						
						eval(parse(text = "ref_col <- ilostat_cols_ref[ilostat_cols_ref %in% colnames(dat %>% select(ref_area:time))]"))
						
						ref_col <- c(ref_col[!ref_col %in% "classif2"], "classif2_version")
								
						run <- paste0(	"separate(dat, classif2,c('CODE_CLACL1','CODE_VSCL1'), sep='_', extra = 'drop', remove = FALSE) %>% ",
										"unite(	classif2_version,CODE_CLACL1,CODE_VSCL1, sep = '_', remove = TRUE) %>% ",
										"mutate(classif2_version = gsub('NA_NA', NA,classif2_version, fixed = TRUE)) %>% ",
										"group_by(",paste0(ref_col, collapse = ', '),") %>% ",
										"mutate(obs_value = ifelse(!obs_value %in% c(NA, 0),obs_value / max(obs_value, na.rm = TRUE) * 100, obs_value)) %>% ",
										"mutate(test = sum(obs_value, na.rm = TRUE) / n()) %>% ",
										"ungroup %>% ",
										"filter(!test %in% 100) %>% ",
										"select(-classif2_version, -test)")
							
					eval(parse(text = run))		
				}
		
		)


}