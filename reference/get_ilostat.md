# Read ilostat Data

Download datasets from ilostat <https://ilostat.ilo.org> via bulk
download facility <https://ilostat.ilo.org/data/bulk/>.

## Usage

``` r
get_ilostat(
  id,
  segment = getOption("ilostat_segment", "indicator"),
  type = getOption("ilostat_type", "code"),
  lang = getOption("ilostat_lang", "en"),
  time_format = getOption("ilostat_time_format", "raw"),
  filters = getOption("ilostat_filter", "none"),
  best_source = getOption("ilostat_best_source", "yes"),
  fixed = getOption("ilostat_fixed", TRUE),
  cache = getOption("ilostat_cache", TRUE),
  cache_update = getOption("ilostat_cache_update", TRUE),
  cache_dir = getOption("ilostat_cache_dir", NULL),
  cache_format = getOption("ilostat_cache_format", "rds"),
  back = getOption("ilostat_back", TRUE),
  cmd = getOption("ilostat_cmd", "none"),
  quiet = getOption("ilostat_quiet", TRUE)
)
```

## Arguments

- id:

  A code name for the dataset of interest. See
  [`get_ilostat_toc`](https://ilostat.github.io/Rilostat/reference/get_ilostat_toc.md)
  or details for how to get code.

- segment:

  A character, way to get datasets by: `"indicator"` (default) or
  `"ref_area"`. Can be set also with options(ilostat_segment =
  "ref_area"),

- type:

  a character, type of variables, `"code"` (default), `"label"` or
  `"both"`. Can be set also with options(ilostat_type = "both"),

- lang:

  a character, code for language. Available are `"en"` (default), `"fr"`
  and `"es"`. Can be set also with options(ilostat_lang = "fr"),

- time_format:

  a string giving a type of the conversion of the time column from the
  ilostat format. "raw" (default) does not do conversion and return time
  as character (ie. "2017", "2017Q1", "2017M01"). A "date" converted to
  a [`Date`](https://rdrr.io/r/base/Dates.html) with a first date of the
  period. A "date_last" converted to a
  [`Date`](https://rdrr.io/r/base/Dates.html) with a last date of the
  period and "num" converted to a numeric. Can be set also with
  options(ilostat_time_format = "date"),

- filters:

  a list; `"none"` (default) to get a whole dataset or a named list of
  filters to get just part of the table. Names of list objects are
  ilostat variable codes and values are vectors of observation codes.
  filters detect on variables, so could be partial, ie.
  `list(sex = "T")` is enough but equivalent to `list(sex = "SEX_T")`.
  Additional options:

  - `timefrom` : starting year of the return dataset,

  - `timeto` : ending year of the return dataset.

- best_source:

  a character; `"yes"` (default) to get secondary source if exist
  `"all"`, `"no"` for secondary only.

- fixed:

  a logical, if `TRUE` (default), filters arguments pattern is a string
  to be matched as is, Change to `FALSE` if more complex regex matching
  is needed.

- cache:

  a logical whether to do caching. Default is `TRUE`. Affects only
  queries from the ilostat bulk download facility. Can be set also with
  options(ilostat_cache = FALSE),

- cache_update:

  a logical whether to update cache. Check cache update with last.update
  attribute store on the cache file name and the one from the table of
  contents. Can be set also with options(ilostat_cache_update = FALSE).
  Default is `TRUE`,

- cache_dir:

  a path to a cache directory. The directory has to exist. The `NULL`
  (default) uses and creates 'ilostat' directory in the temporary
  directory from [`tempdir`](https://rdrr.io/r/base/tempfile.html). The
  directory can also be set with `option` ilostat_cache_dir,

- cache_format:

  a character, format to store on the cache `"rds"` (default), but also
  `"csv"`, `"dta"`, `"sav"`, `"sas7bdat"`. useful for getting ilostat
  dataset directly on the `cache_dir` without R. Can be set also with
  options(ilostat_cache_format = "dta"),

- back:

  a logical; if TRUE (default) return a data frame, otherwise only write
  to cache,

- cmd:

  a character, R expression use for manipulate internal data frame `dat`
  object applied to each datasets retrieved If use, `cache` is set to
  FALSE. see examples. This argument uses non-standard evaluation and
  should be used with care.

- quiet:

  a logical, if `TRUE` , don't return message from processing, `FALSE`
  (default). Can be set also with options(ilostat_quiet = TRUE),

## Value

a tibble. One column for each dimension in the data and the values
column for numerical values, as well as the metadata columns. The time
column for a time dimension.

## others

Data sets are downloaded from the ilostat bulk download facility. If
only the table `id` is given, the whole table is downloaded from the
bulk download facility.

The bulk download facility is the fastest method to download whole
datasets.

By default datasets from the bulk download facility are cached as they
are often rather large.

Cache files are stored in a temporary directory by default or in a named
directory if cache_dir or option ilostat_cache_dir is defined. The cache
can be emptied with
[`clean_ilostat_cache`](https://ilostat.github.io/Rilostat/reference/clean_ilostat_cache.md).

The `id`, a code, for the dataset can be searched with the
[`get_ilostat_toc`](https://ilostat.github.io/Rilostat/reference/get_ilostat_toc.md)
or from the \[bulk download
facility\](https://ilostat.ilo.org/data/bulk/).

## References

See citation("Rilostat") ilostat bulk download facility user guidelines
<https://ilostat.ilo.org/data/bulk/>

## See also

[`get_ilostat_toc`](https://ilostat.github.io/Rilostat/reference/get_ilostat_toc.md),
[`label_ilostat`](https://ilostat.github.io/Rilostat/reference/label_ilostat.md)

## Author

David Bescond <bescond@ilo.org>

## Examples

``` r
if (FALSE) { # \dontrun{
############# get simple dataset
 dat <- get_ilostat("UNE_TUNE_SEX_AGE_NB_A")
 head(dat)
 dat <- get_ilostat("NZL_Q", segment = "ref_area")
 head(dat)

 dir.create(file.path(tempdir(), "r_cache"))
 dat <- get_ilostat("UNE_TUNE_SEX_AGE_NB_A", 
                  cache_dir = file.path(tempdir(), "r_cache"))
 head(dat)

 clean_ilostat_cache(cache_dir = file.path(tempdir(), "r_cache")) 

 options(ilostat_update = TRUE)
 dat <- get_ilostat("UNE_TUNE_SEX_AGE_NB_A")
 head(dat)
 options(ilostat_update = FALSE)
 options(ilostat_cache_dir = file.path(tempdir(), "r_cache"))
 dat <- get_ilostat("UNE_TUNE_SEX_AGE_NB_A")

 clean_ilostat_cache() 

############# get multiple datasets
 dat <- get_ilostat(c("CPI_ACPI_COI_RT_M", "CPI_ACPI_COI_RT_Q"), cache = FALSE)
 head(dat)
 toc <- get_ilostat_toc(search = "CPI_")
 head(toc)
 dat <- get_ilostat(toc, cache = FALSE) #id as a tibble

############# get datasets with filters
 dat <- get_ilostat(id = c("UNE_TUNE_SEX_AGE_NB_A", "EMP_TEMP_SEX_AGE_NB_A"), 
         filters = list(  ref_area = "FRA", 
         classif1 = "_YGE15", 
         time = "2016",
         sex = c("T", "SEX_F")), quiet = TRUE)
 head(dat)
 clean_ilostat_cache() 

############# store in other format
 dir.create(file.path(tempdir(), "ilostat"))

 dat <- get_ilostat("UNE_TUNE_SEX_AGE_NB_A", 
                  cache_dir = file.path(tempdir(), "r_cache"), cache_format = "csv")
 dat <- get_ilostat("UNE_TUNE_SEX_AGE_NB_A", 
                  cache_dir = file.path(tempdir(), "r_cache"), cache_format = "dta")

############# advanced manipulation

 dat <- get_ilostat("UNE_TUNE_SEX_AGE_NB_A", cmd = "dat %>% count(ref_area)", quiet = TRUE)
 label_ilostat(dat, code = "ref_area")

 clean_ilostat_cache()
} # }
```
