## ----echo = FALSE, results = 'hide',   eval=TRUE, message = FALSE--------
if(!require(devtools)){install.packages('devtools', repos='http://cran.us.r-project.org')}
if(!require(plotly)){install.packages('plotly', repos='http://cran.us.r-project.org')}
if(!require(dplyr)){install.packages('dplyr', repos='http://cran.us.r-project.org')}
if(!require(ggplot2)){install.packages('ggplot2', repos='http://cran.us.r-project.org')}
if(!require(plotrix)){install.packages('plotrix', repos='http://cran.us.r-project.org')}
if(!require(stringr)){install.packages('stringr', repos='http://cran.us.r-project.org')}
require(Rilostat)


## ---- eval=FALSE---------------------------------------------------------
#  install.packages("Rilostat")

## ---- eval=FALSE---------------------------------------------------------
#  if(!require(devtools)){install.packages('devtools')}
#  install_github("ilostat/Rilostat")

## ---- eval=TRUE----------------------------------------------------------
require(Rilostat)
as.data.frame(ls("package:Rilostat"))


## ---- echo=FALSE, eval=TRUE----------------------------------------------
options(ilostat_quiet = TRUE)

## ---- eval=TRUE----------------------------------------------------------
toc <- get_ilostat_toc()


## ---- echo=FALSE, eval=TRUE, results = 'asis'----------------------------
knitr::kable(head(toc[1:3,1:5]), caption = "Table 1a. Extract, 'Table of contents by indicator in English'")

## ---- eval=TRUE----------------------------------------------------------
toc <- get_ilostat_toc(segment = 'ref_area', lang = 'es')

## ---- echo=FALSE, eval=TRUE----------------------------------------------
knitr::kable(head(toc[1:3,1:9]),caption = "Table 1b. Extract, 'Table of contents by ref_area in Spanish'")

## ---- eval=TRUE----------------------------------------------------------
toc <- get_ilostat_toc(search = 'bargaining')

## ---- echo=FALSE, eval=TRUE----------------------------------------------
knitr::kable(toc[,1:8],caption = "Table 1c. 'Table of contents by indicator with search word 'bargaining''")

## ---- eval=TRUE----------------------------------------------------------
toc <- get_ilostat_toc(segment = 'ref_area', search = c('France|Albania', 'Annual'), 
											fixed = FALSE)

## ---- echo=FALSE, eval=TRUE----------------------------------------------
knitr::kable(toc[1:2,1:9],caption = "Table 1d. 'Table of contents by ref_area with search words 'France' or 'Albania' and 'Annual''")

## ---- eval=TRUE----------------------------------------------------------
toc <-  dplyr::filter(get_ilostat_toc(), collection == 'STI', freq == 'M')

## ---- echo=FALSE, eval=TRUE----------------------------------------------
knitr::kable(head(toc[1:5,1:5]),caption = "Table 1e. Extract, 'Table of contents by indicator filtered for Monthly Short term indicators'")

## ---- eval=TRUE----------------------------------------------------------
dat <- get_ilostat(id = 'UNE_2UNE_SEX_AGE_NB_A', segment = 'indicator') 

## ---- echo=FALSE, eval=TRUE----------------------------------------------
knitr::kable(head(dat[1:3,1:7]),caption = "Table 2a. Extract, 'Annual unemployment by sex and age, ILO modelled estimates, Nov. 2018'")

## ---- eval=TRUE----------------------------------------------------------
dat <- get_ilostat(id = 'ARM_A', segment = 'ref_area') 


## ---- echo=FALSE, eval=TRUE----------------------------------------------
knitr::kable(head(dat[1:3, c(1:5,7:8)]),caption = "Table 2b. Extract, 'Armenia, annual data'")

## ---- eval=TRUE----------------------------------------------------------
dat <- get_ilostat(id = c('AFG_A', 'TTO_A'), segment = 'ref_area') 

dplyr::count(dat, ref_area)


## ---- eval=TRUE----------------------------------------------------------
toc <- get_ilostat_toc(search = 'CPI_')

dat <- get_ilostat(id = toc, segment = 'indicator', quiet = TRUE) 

dplyr::count(dat, indicator)


## ---- eval=TRUE----------------------------------------------------------
dat <- get_ilostat(id = 'UNE_TUNE_SEX_AGE_NB_Q', time_format = 'num') 

## ---- echo=FALSE, eval=TRUE----------------------------------------------
knitr::kable(dat[1:3, 1:7],caption = "Table 3a. Extract, 'Quarterly unemployment by sex and age'")

## ---- eval=TRUE----------------------------------------------------------
dat <- get_ilostat(id = 'TRU_TTRU_SEX_AGE_NB_M', time_format = 'date') 

## ---- echo=FALSE, eval=TRUE----------------------------------------------
knitr::kable(head(dat[1:3, 1:7]),caption = "Table 3b. Extract, Monthly time-related underemployment by sex and age")

## ----eval=FALSE----------------------------------------------------------
#  dat <- get_ilostat(id = 'TRU_TTRU_SEX_AGE_NB_M', cache_dir = 'c:/temp', cache_format = 'dta')

## ---- eval=FALSE---------------------------------------------------------
#  get_ilostat(id = get_ilostat_toc(search = 'SDG'), 	cache_dir = 'c:/temp', cache_format = 'dta',
#  													back = FALSE)

## ---- eval=FALSE---------------------------------------------------------
#  options(ilostat_cache_dir = 'C:/temp')
#  dat <- get_ilostat(id = 'UNE_2EAP_SEX_AGE_RT_A', filters = list(
#  													ref_area = c('BRA', 'ZAF'),
#  													sex = 'T',
#  													classif1 = 'AGE_YTHADULT_Y15-24'))
#  dplyr::count(dat, ref_area, sex, classif1)

